/*
 * beam_load.c
 *
 *  Created on: Oct 30, 2013
 *      Author: Studnicki
 */

#include "global.h"
#include "beam_load.h"
#include "beam_emu.h"
#include "export.h"
#include "io.h"
#include "config.h"

#if (THREADED_CODE == 1)
void* jump_table[ALL_OPCODES];

void jump_table_add(int opcode, const void* ptr) {
	//char buf[30];
	//sprintf(buf, "op %d: %d\n", opcode, ptr);
	//debug(buf);
	jump_table[opcode] = (void*)ptr;
}
#endif

void erts_load(byte* code) {
	//debug("before allocating loader: ");
	//debug_32(xPortGetFreeHeapSize());

	LoaderState *loader = pvPortMalloc(sizeof(LoaderState));
	loader->code_file = code;

	//debug("before loading the atom table: ");
	//debug_32(xPortGetFreeHeapSize());

	//Initialize binary file and verify if it's ok
	if(init_iff_file(loader) || scan_iff_file(loader) || verify_chunks(loader)) {
		goto load_error;
	}


	//Load atom table
	loader->file_left = loader->chunks[ATOM_CHUNK].size;
	loader->file_p = loader->chunks[ATOM_CHUNK].start;
	if(load_atom_table(loader)) {
		goto load_error;
	}

	//debug("after loading the atom table: ");
	//debug_32(xPortGetFreeHeapSize());

	//Load import table
	loader->file_left = loader->chunks[IMP_CHUNK].size;
	loader->file_p = loader->chunks[IMP_CHUNK].start;
	if(load_import_table(loader)) {
		goto load_error;
	}

	//debug("after loading the import table: ");
	//debug_32(xPortGetFreeHeapSize());

	//Load literal table
	if(loader->chunks[LITERAL_CHUNK].size > 0) {
		loader->file_left = loader->chunks[LITERAL_CHUNK].size;
		loader->file_p = loader->chunks[LITERAL_CHUNK].start;
		if(load_literal_table(loader)) {
			goto load_error;
		}
	}

	//debug("after loading the literal table: ");
	//debug_32(xPortGetFreeHeapSize());

	//Read code chunk header
	loader->file_left = loader->chunks[CODE_CHUNK].size;
	loader->file_p = loader->chunks[CODE_CHUNK].start;
	if(read_code_header(loader)) {
		goto load_error;
	}

	//Load code chunk
	loader->file_left = loader->code_size;
	loader->file_p = loader->code_start;
	if(load_code(loader)) {
		goto load_error;
	}

	//debug("after loading the code: ");
	//debug_32(xPortGetFreeHeapSize());

	//Load export table
	loader->file_left = loader->chunks[EXP_CHUNK].size;
	loader->file_p = loader->chunks[EXP_CHUNK].start;
	if(load_export_table(loader)) {
		goto load_error;
	}

	//debug("after loading the export table: ");
	//debug_32(xPortGetFreeHeapSize());

	//Finalize (patch labels, export functions and stuff)
	if(finalize(loader)) {
		goto load_error;
	}

	//Everything should be fine at this point
	goto free_loader;

	load_error:
	vPrintString("error while loading module\n");

	free_loader:
	//debug("before freeing loader: ");
	//debug_32(xPortGetFreeHeapSize());

	//vPrintString("freeing loader\n");
	vPortFree(loader->literal);
	vPortFree(loader->labels);
	vPortFree(loader->import);
	vPortFree(loader->export);
	vPortFree(loader->atom);
	vPortFree(loader);

	//debug("after freeing loader: ");
	//debug_32(xPortGetFreeHeapSize());
}


static int read_code_header(LoaderState* loader) {
	uint32_t header_size;
	uint32_t version;
	uint32_t max_opcode;
	int i;

	GetInt(loader, header_size);
	loader->code_start = loader->file_p + header_size;
	loader->code_size = loader->file_left - header_size;

	GetInt(loader, version);
	//@todo verify BEAM_FORMAT_NUMBER

	GetInt(loader, max_opcode);
	//@todo verify max opcode (MAX_GENERIC_OPCODE 153)

	GetInt(loader, loader->num_labels);
	GetInt(loader, loader->num_functions);

	loader->labels = (Label*)pvPortMalloc(loader->num_labels * sizeof(Label));
	for(i=0; i<loader->num_labels; i++) {
		loader->labels[i].value = 0;
		loader->labels[i].patches = 0;
	}

	loader->catches = 0;

	return 0;
}

static int load_export_table(LoaderState* loader) {
	uint32_t function, arity, label, value;
	int i;

	GetInt(loader, loader->num_exports);
	loader->export = pvPortMalloc(loader->num_exports * sizeof(ExportEntry));
	for(i=0; i<loader->num_exports; i++) {
		GetInt(loader, function);
		loader->export[i].function = loader->atom[function];
		GetInt(loader, arity);
		loader->export[i].arity = arity;
		GetInt(loader, label);
		value = loader->labels[label].value;
		loader->export[i].address = (uint16_t)value;
	}

}

static int load_import_table(LoaderState* loader) {
	// @todo check if atoms has correct indices
	int i;
	GetInt(loader, loader->num_imports);
	loader->import = pvPortMalloc(loader->num_imports * sizeof(ImportEntry));

	for(i=0; i<loader->num_imports; i++) {
		int n;
		uint32_t arity;

		GetInt(loader, n);
		loader->import[i].module = loader->atom[n];
		GetInt(loader, n);
		loader->import[i].function = loader->atom[n];
		GetInt(loader, arity);
		loader->import[i].arity = arity;
		loader->import[i].patches = 0;
	}

	return 0;
}

static int load_code(LoaderState* loader) {
	byte op;
	uint8_t arity, arg;
	int i;
	int c=0;

	loader->code_buffer_used = 0;

	while(loader->file_left) {
		GetByte(loader, op);
		arity = opcode_arities[op];

		uint32_t p = loader->code_buffer_used;

#if (THREADED_CODE == 1)
		loader->code[loader->code_buffer_used++] = (BeamInstr)jump_table[op];
#else
		loader->code[loader->code_buffer_used++] = (BeamInstr)op;
#endif

		for(i=0; i<arity; i++) {
			BeamInstr* code = loader->code + loader->code_buffer_used;
			loader->code_buffer_used += get_tag_and_value(loader, &code);
		}
		define_label(loader, p, op);
		replace_ext_call(loader, p, op);
		//replace_local_call(loader, p, op);
	}
}

static void define_label(LoaderState* loader, uint16_t offset, byte op) {
	//define a pointer to the next instruction when label/1 has been identified
	if(op == LABEL) {
		uint16_t value = unsigned_val(loader->code[offset+1]);
		loader->labels[value].value = (offset+2);
	}
}

static void replace_local_call(LoaderState* loader, uint16_t offset, byte op) {
	if(LABEL_OP_1(op) || LABEL_OP_2(op)) {
		uint8_t arg = LABEL_OP_1(op) ? 1 : 2;
		uint16_t label = unsigned_val(loader->code[offset+arg]);
		loader->code[offset+arg] = (BeamInstr)loader->labels[label].patches;
		loader->labels[label].patches = (uint16_t)(offset+arg);
	}
}

static void replace_ext_call(LoaderState* loader, uint16_t offset, byte op) {
	//leaves a trace in import entries to finally replace with the export table entry
	if(EXTERNAL_OP_1(op) || EXTERNAL_OP_2(op) || EXTERNAL_OP_3(op)) {
		uint8_t arg;
		if(EXTERNAL_OP_1(op)) {
			arg = 1;
		}
		else if(EXTERNAL_OP_2(op)) {
			arg = 2;
		}
		else {
			arg = 3;
		}
		uint16_t imp = unsigned_val(loader->code[offset+arg]);
		loader->code[offset+arg] = (BeamInstr)loader->import[imp].patches;
		loader->import[imp].patches = (uint16_t)(offset+arg);
	}
}

static int get_tag_and_value(LoaderState* loader, BeamInstr** result) {
	uint8_t tag, start;
	uint32_t value;

	UInt used = 0;

	//@todo handle bignums
	GetByte(loader, start);

	//1 byte
	if((start & 0x08) == 0x00) {
		tag = start & 0x0f;
		value = start >> 4;
	}
	//2 bytes
	else if((start & 0x08) == 0x08) {
		tag = start & 0x07;
		GetByte(loader, value);
		value |= (start & 0xe0) << 3;
	}
	else {
		return 1;
	}



	switch(tag) {
	case TAG_f:
		**result = (BeamInstr)loader->labels[value].patches;
		loader->labels[value].patches = (uint16_t)(*result-loader->code);
		break;
	case TAG_u:
		**result = make_small(value);
		break;
	case TAG_i:
		**result = make_small(value);
		break;
	case TAG_a:
		if(value == 0) {
			**result = NIL;
		}
		else {
			**result = loader->atom[value];
		}
		break;
	case TAG_x:
		if(value == 0) {
			**result = make_rreg();
		}
		else {
			**result = make_xreg(value);
		}
		break;
	case TAG_y:
		**result = make_yreg(value);
		break;
	case TAG_z:
		//list
		if(value == 1) {
			get_tag_and_value(loader, result);
			UInt arity = unsigned_val((Eterm)*(*result-1));
			int i;
			for(i=0; i<arity; i++) {
				used += get_tag_and_value(loader, result);
			}
		}
		//literal
		else if(value == 4) {
			BeamInstr tmp, *tmp1 = &tmp;
			get_tag_and_value(loader, &tmp1);
			**result = (BeamInstr)loader->literal[unsigned_val((Eterm)tmp)].term;
		}
		break;
	}

	*result += 1;

	return used+1;
}

static int finalize(LoaderState* loader) {
	int i;

	BeamModule *module = pvPortMalloc(sizeof(BeamModule));

	module->name = loader->atom[1];
	module->size = loader->code_buffer_used;
	module->code = pvPortMalloc(loader->code_buffer_used*sizeof(BeamInstr));
	if(module->code == NULL) {
		return 1;
	}

	BeamInstr* code = module->code;
	memcpy(code, loader->code, loader->code_buffer_used*sizeof(BeamInstr));

	//imports all the external functions and patch all callers with the export table entry
	for(i = 0; i<loader->num_imports; i++) {
		Eterm module;
		Eterm function;
		uint8_t arity;
		BeamInstr import;
		uint16_t current, next;

		module = loader->import[i].module;
		function = loader->import[i].function;
		arity = loader->import[i].arity;
		import = (BeamInstr)(erts_export_put(module, function, arity));
		current = loader->import[i].patches;
		while(current != 0) {
			next = loader->code[current];
			code[current] = import;
			current = next;
		}
	}

	//export functions
	for(i = 0; i<loader->num_exports; i++) {
		Export *export;
		export = erts_export_put(loader->atom[1], loader->export[i].function,
				                 loader->export[i].arity);
		export->address = (BeamInstr*)(code+loader->export[i].address);
	}

	//exchange all labels with the code addresses
	for(i = 0; i<loader->num_labels; i++) {
		uint16_t current, next;
		uint16_t value = loader->labels[i].value;
		current = loader->labels[i].patches;
		while(current != 0) {
			next = loader->code[current];
			BeamInstr* ptr = (BeamInstr*)(code+value);
			code[current] = (BeamInstr)ptr;
			current = next;
		}
	}

	return 0;
}

static int load_atom_table(LoaderState* loader) {
	// @todo load the actual atom table
	// @todo check whether the first atom in the table is also name of the module
	uint8_t i;
	GetInt(loader, loader->num_atoms);

	// Read all atoms (indexes are 1..num_atoms in order to pick labeled atom directly)
	loader->num_atoms++;
	loader->atom = pvPortMalloc(loader->num_atoms*sizeof(Eterm));
	for(i=1; i<loader->num_atoms; i++) {
		byte* atom;
		uint8_t n;
		GetByte(loader, n);
		GetString(loader, atom, n);
		loader->atom[i] = erts_atom_put(atom, n);
	}

	// @todo check whether load->atom[1] is the module name, if so return an error
	return 0;
}

static int load_literal_table(LoaderState* loader) {
	GetInt(loader, loader->num_literals);

	loader->literal = (LiteralEntry*)pvPortMalloc(loader->num_literals * sizeof(LiteralEntry));
	uint8_t i;
	for(i=0; i<loader->num_literals; i++) {
		loader->literal[i].heap = NULL;
		uint32_t size;
		GetInt(loader, size);

		uint32_t heap_size = erts_decode_ext_size(loader->file_p, size);
		loader->literal[i].heap = pvPortMalloc((heap_size+1) * sizeof(Eterm));
		Eterm* hp = loader->literal[i].heap+1;
		Eterm term = erts_decode_ext(&hp, &loader->file_p);
		*(loader->literal[i].heap) = term;
		loader->literal[i].term = (Eterm)(loader->literal[i].heap);
	}
}

static int init_iff_file(LoaderState* loader) {
	uint32_t form_id = MakeIffId('F','O','R','1');
	byte* code = loader->code_file;
	if(MakeIffId(code[0],code[1],code[2],code[3])!=form_id) {
		vPrintString("Not a BEAM file, no FOR1 chunk\n");
		return 1;
	}

	// prepare LoaderState struct
	loader->file_p = code+4;
	//read the code size, in file it's big-endian, here it's most likely little-endian
	GetInt(loader, loader->code_size);
	loader->file_left = loader->code_size;

	// check file header, should be "BEAM"
	uint32_t beam_header = MakeIffId('B','E','A','M');
	uint32_t header;
	GetInt(loader, header);
	if(beam_header!=header) {
		vPrintString("Not a BEAM file, no BEAM header\n");
		return 1;
	}

	return 0;
}

static int scan_iff_file(LoaderState* loader) {

	// initialize chunks array
	uint8_t i;
	for(i=0; i<NUM_CHUNK_TYPES; i++) {
		loader->chunks[i].start = NULL;
		loader->chunks[i].size = 0;
	}

	// scan the file and read all the chunks
	while(loader->file_left) {
		uint32_t chunk_id, chunk_size;
		GetInt(loader, chunk_id);
		GetInt(loader, chunk_size);

		// @todo verify whether chunk_id is 4 ascii characters
		// @todo verify whether chunk_size is in bounds of what is left in the file

		// iterate over available chunks and check if the current one is one of these
		// observation: chunks may be in a random order in the BEAM file
		for(i=0; i<NUM_CHUNK_TYPES; i++) {
			if(chunk_types[i] == chunk_id) {
				loader->chunks[i].start = loader->file_p;
				loader->chunks[i].size = chunk_size;
				break;
			}
		}

		// go to the next chunk (the chunk area is padded to 4 bytes)
		uint8_t remainder = chunk_size % 4;
		chunk_size += (remainder == 0 ? 0 : (4-remainder));

		loader->file_p += chunk_size;
		loader->file_left -= chunk_size;
	}
	return 0;
}

static int verify_chunks(LoaderState* loader) {
	// @todo calculate md5 of file

	//check whether all mandatory chunks are present
	uint8_t i;
	for(i=0; i<NUM_MANDATORY; i++) {
		if(loader->chunks[i].start == NULL) {
			char buffer[33], cbuffer[5];
			cbuffer[4] = '\0';
			strncpy(cbuffer, &chunk_types[i], 4);
			sprintf(buffer, "No mandatory chunk of type %s\n", cbuffer);
			vPrintString(buffer);
			return 1;
		}
	}
	return 0;
}
