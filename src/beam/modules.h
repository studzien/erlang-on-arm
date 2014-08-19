#ifndef MODULES_H_
#define MODULES_H_
#define MODULES_N 3

#define ENTRYPOINT_ARITY 0
#define ENTRYPOINT_M "main"
#define ENTRYPOINT_F "main"
#define ENTRYPOINT_M_LEN 4
#define ENTRYPOINT_F_LEN 4

const byte module_lists[] = {70,79,82,49,0,0,2,108,66,69,65,77,65,116,111,109,0,0,0,65,0,0,0,7,5,108,105,115,116,115,9,100,117,112,108,105,99,97,116,101,6,101,114,108,97,110,103,1,45,7,110,116,104,116,97,105,108,11,109,111,100,117,108,101,95,105,110,102,111,15,103,101,116,95,109,111,100,117,108,101,95,105,110,102,111,0,0,0,67,111,100,101,0,0,0,157,0,0,0,16,0,0,0,0,0,0,0,153,0,0,0,13,0,0,0,5,1,16,153,16,2,18,34,32,1,32,64,2,35,6,48,69,1,48,153,32,2,18,34,48,1,64,43,85,3,1,64,35,3,19,1,80,153,48,125,5,48,0,3,17,3,16,32,48,69,19,35,35,6,48,69,1,96,153,64,2,18,82,32,1,112,43,133,3,1,64,19,3,19,1,128,56,101,19,65,19,35,51,153,80,125,5,64,0,3,17,3,64,51,19,6,32,117,1,144,153,0,2,18,98,0,1,160,64,18,3,153,0,78,16,16,1,176,153,0,2,18,98,16,1,192,64,3,19,64,18,3,153,0,78,32,32,3,0,0,0,83,116,114,84,0,0,0,0,73,109,112,84,0,0,0,40,0,0,0,3,0,0,0,3,0,0,0,4,0,0,0,2,0,0,0,3,0,0,0,7,0,0,0,1,0,0,0,3,0,0,0,7,0,0,0,2,69,120,112,84,0,0,0,52,0,0,0,4,0,0,0,6,0,0,0,1,0,0,0,12,0,0,0,6,0,0,0,0,0,0,0,10,0,0,0,5,0,0,0,2,0,0,0,7,0,0,0,2,0,0,0,2,0,0,0,2,76,111,99,84,0,0,0,16,0,0,0,1,0,0,0,2,0,0,0,3,0,0,0,4,65,116,116,114,0,0,0,40,131,108,0,0,0,1,104,2,100,0,3,118,115,110,108,0,0,0,1,110,16,0,38,52,106,81,5,180,249,241,152,22,6,139,161,69,205,53,106,106,67,73,110,102,0,0,0,130,131,108,0,0,0,4,104,2,100,0,7,111,112,116,105,111,110,115,106,104,2,100,0,7,118,101,114,115,105,111,110,107,0,5,52,46,57,46,52,104,2,100,0,4,116,105,109,101,104,6,98,0,0,7,222,97,8,97,19,97,11,97,27,97,4,104,2,100,0,6,115,111,117,114,99,101,107,0,50,47,85,115,101,114,115,47,83,116,117,100,110,105,99,107,105,47,119,111,114,107,47,109,103,114,47,101,120,97,109,112,108,101,115,47,114,102,109,55,48,47,108,105,115,116,115,46,101,114,108,106,0,0,65,98,115,116,0,0,0,0,76,105,110,101,0,0,0,26,0,0,0,0,0,0,0,0,0,0,0,9,0,0,0,5,0,0,0,0,97,145,193,225,9,17,0,0};
const byte module_main[] = {70,79,82,49,0,0,2,144,66,69,65,77,65,116,111,109,0,0,0,96,0,0,0,10,4,109,97,105,110,5,114,102,109,55,48,5,115,116,97,114,116,11,109,111,100,117,108,101,95,105,110,102,111,6,101,114,108,97,110,103,15,103,101,116,95,109,111,100,117,108,101,95,105,110,102,111,14,45,109,97,105,110,47,48,45,102,117,110,45,48,45,3,110,111,119,9,108,112,99,95,100,101,98,117,103,10,112,114,105,110,116,95,116,101,114,109,67,111,100,101,0,0,0,119,0,0,0,16,0,0,0,0,0,0,0,153,0,0,0,9,0,0,0,4,1,16,153,16,2,18,18,0,1,32,103,0,16,32,16,69,3,2,3,153,32,78,16,0,1,48,153,0,2,18,66,0,1,64,64,18,3,153,0,78,16,16,1,80,153,0,2,18,66,16,1,96,64,3,19,64,18,3,153,0,78,32,32,1,112,153,48,2,18,114,16,1,128,12,16,16,64,3,4,153,64,7,0,48,153,64,7,16,64,64,4,3,153,80,8,16,64,16,3,0,83,116,114,84,0,0,0,0,73,109,112,84,0,0,0,64,0,0,0,5,0,0,0,2,0,0,0,3,0,0,0,1,0,0,0,5,0,0,0,6,0,0,0,1,0,0,0,5,0,0,0,6,0,0,0,2,0,0,0,5,0,0,0,8,0,0,0,0,0,0,0,9,0,0,0,10,0,0,0,1,69,120,112,84,0,0,0,40,0,0,0,3,0,0,0,4,0,0,0,1,0,0,0,6,0,0,0,4,0,0,0,0,0,0,0,4,0,0,0,1,0,0,0,0,0,0,0,2,70,117,110,84,0,0,0,28,0,0,0,1,0,0,0,7,0,0,0,1,0,0,0,8,0,0,0,0,0,0,0,0,6,246,42,254,76,111,99,84,0,0,0,16,0,0,0,1,0,0,0,7,0,0,0,1,0,0,0,8,65,116,116,114,0,0,0,40,131,108,0,0,0,1,104,2,100,0,3,118,115,110,108,0,0,0,1,110,16,0,55,56,102,150,65,50,109,95,104,4,168,139,201,95,197,222,106,106,67,73,110,102,0,0,0,129,131,108,0,0,0,4,104,2,100,0,7,111,112,116,105,111,110,115,106,104,2,100,0,7,118,101,114,115,105,111,110,107,0,5,52,46,57,46,52,104,2,100,0,4,116,105,109,101,104,6,98,0,0,7,222,97,8,97,19,97,11,97,27,97,4,104,2,100,0,6,115,111,117,114,99,101,107,0,49,47,85,115,101,114,115,47,83,116,117,100,110,105,99,107,105,47,119,111,114,107,47,109,103,114,47,101,120,97,109,112,108,101,115,47,114,102,109,55,48,47,109,97,105,110,46,101,114,108,106,0,0,0,65,98,115,116,0,0,0,0,76,105,110,101,0,0,0,25,0,0,0,0,0,0,0,0,0,0,0,10,0,0,0,5,0,0,0,0,81,161,97,113,129,0,0,0};
const byte module_rfm70[] = {70,79,82,49,0,0,16,12,66,69,65,77,65,116,111,109,0,0,2,38,0,0,0,52,5,114,102,109,55,48,5,115,116,97,114,116,6,101,114,108,97,110,103,4,115,101,108,102,4,112,105,110,103,4,108,111,111,112,9,105,110,116,101,114,114,117,112,116,11,104,97,110,100,108,101,95,112,105,110,103,10,115,101,110,100,95,97,102,116,101,114,19,109,97,121,98,101,95,114,101,99,101,105,118,101,95,102,114,97,109,101,4,98,97,110,100,2,111,107,13,114,101,99,101,105,118,101,95,102,114,97,109,101,3,98,111,114,10,115,101,110,100,95,102,114,97,109,101,9,115,119,105,116,99,104,95,114,120,8,108,112,99,95,103,112,105,111,3,108,111,119,4,104,105,103,104,9,115,119,105,116,99,104,95,116,120,9,105,110,105,116,95,112,105,110,115,6,111,117,116,112,117,116,5,105,110,112,117,116,7,102,97,108,108,105,110,103,7,108,112,99,95,115,112,105,4,105,110,105,116,10,105,110,105,116,95,98,97,110,107,48,10,105,110,105,116,95,98,97,110,107,49,4,98,97,110,107,3,98,115,114,14,114,101,103,105,115,116,101,114,95,119,114,105,116,101,13,114,101,103,105,115,116,101,114,95,114,101,97,100,2,114,119,5,108,105,115,116,115,9,100,117,112,108,105,99,97,116,101,2,43,43,7,110,116,104,116,97,105,108,5,98,97,110,107,48,15,100,121,110,97,109,105,99,95,112,97,121,108,111,97,100,9,97,100,100,114,101,115,115,101,115,5,98,97,110,107,49,11,109,111,100,117,108,101,95,105,110,102,111,15,103,101,116,95,109,111,100,117,108,101,95,105,110,102,111,16,45,114,119,47,51,45,108,99,36,94,48,47,49,45,48,45,16,45,114,119,47,49,45,108,99,36,94,48,47,49,45,48,45,24,45,105,110,105,116,95,98,97,110,107,49,47,48,45,108,99,36,94,48,47,49,45,48,45,24,45,105,110,105,116,95,98,97,110,107,48,47,48,45,108,99,36,94,50,47,49,45,50,45,24,45,105,110,105,116,95,98,97,110,107,48,47,48,45,108,99,36,94,49,47,49,45,49,45,24,45,105,110,105,116,95,98,97,110,107,48,47,48,45,108,99,36,94,48,47,49,45,48,45,27,45,114,101,99,101,105,118,101,95,102,114,97,109,101,47,50,45,108,99,36,94,48,47,49,45,48,45,15,102,117,110,99,116,105,111,110,95,99,108,97,117,115,101,5,101,114,114,111,114,0,0,67,111,100,101,0,0,8,19,0,0,0,16,0,0,0,0,0,0,0,153,0,0,0,90,0,0,0,29,1,16,153,16,2,18,34,16,1,32,12,16,16,64,3,4,153,32,4,0,13,34,153,48,4,0,13,36,153,64,4,0,13,39,153,80,1,48,26,53,9,100,22,64,1,3,153,96,4,16,13,41,153,112,4,0,13,28,9,0,3,64,82,19,153,128,20,64,4,3,5,16,85,16,1,64,153,144,2,18,98,16,1,80,12,16,16,64,3,4,153,160,1,96,23,165,3,48,149,3,59,3,149,23,64,114,117,82,133,1,112,21,64,4,3,153,176,4,16,229,64,4,3,5,16,85,16,1,128,21,153,192,4,0,197,64,4,3,5,16,85,16,1,144,21,64,4,3,5,16,85,16,1,160,25,101,1,176,153,208,2,18,130,0,1,192,12,0,0,64,71,0,3,153,224,4,16,13,21,9,0,19,64,82,35,64,105,232,3,153,240,8,48,16,0,1,208,153,8,16,2,18,162,16,1,224,12,16,16,64,3,4,64,113,3,153,8,17,4,16,13,47,56,13,16,3,65,3,19,35,52,13,16,35,153,8,18,125,5,32,32,19,9,64,3,43,245,3,9,64,64,4,3,5,32,13,18,16,1,240,64,194,3,18,16,19,1,8,16,153,8,17,72,3,1,8,17,153,8,19,2,18,210,32,1,8,18,12,48,32,17,4,64,3,36,64,9,96,3,64,19,20,153,8,20,4,16,13,47,56,13,19,3,65,3,19,35,52,13,19,35,64,17,35,64,71,16,3,153,8,21,4,48,13,51,64,3,4,64,71,32,3,153,8,22,4,16,13,49,153,8,23,125,5,0,48,20,9,64,3,16,32,16,69,3,2,19,17,20,64,113,3,153,8,23,4,32,13,45,64,4,19,64,36,3,5,32,13,87,48,1,8,19,153,8,20,72,3,1,8,20,153,8,24,2,18,242,16,1,8,21,12,16,16,64,3,4,153,8,25,4,0,13,31,16,32,0,69,9,176,4,3,17,4,153,8,26,4,16,13,49,153,8,27,1,8,22,23,13,24,3,43,13,23,3,114,21,64,194,3,61,13,25,1,8,23,24,13,22,1,8,24,26,13,22,9,100,22,64,71,48,3,1,8,25,64,3,4,64,113,3,153,8,28,4,16,13,47,56,13,26,3,65,3,19,35,52,13,26,35,153,8,29,125,5,32,48,19,9,32,3,16,32,32,69,3,2,19,64,113,3,153,8,29,4,32,13,45,153,8,30,4,0,13,28,64,4,3,18,16,19,1,8,26,153,8,28,72,3,1,8,27,153,8,31,2,18,10,16,0,1,8,28,12,0,0,64,71,32,3,153,8,32,4,16,13,49,64,9,24,19,64,1,3,153,8,33,7,32,64,64,1,3,153,8,34,4,16,13,47,56,13,29,3,65,3,19,35,52,13,29,35,153,8,35,125,5,32,48,19,17,3,16,32,32,69,3,2,19,64,1,3,153,8,35,4,32,13,45,64,9,24,19,64,1,3,153,8,36,8,32,80,0,1,8,29,153,8,34,72,3,1,8,30,153,8,37,2,18,10,20,0,1,8,31,12,0,0,64,71,64,3,153,8,38,4,16,13,49,64,9,24,19,64,1,3,153,8,39,7,32,64,64,1,3,153,8,40,4,16,13,47,56,13,32,3,65,3,19,35,52,13,32,35,153,8,41,125,5,32,32,19,9,254,3,16,32,32,69,3,2,19,64,1,3,153,8,41,4,32,13,45,64,9,24,19,64,1,3,153,8,42,8,32,80,0,1,8,32,153,8,40,72,3,1,8,33,153,8,43,2,18,10,21,0,1,8,34,12,0,0,64,9,24,19,64,1,3,153,8,44,7,32,96,64,9,23,19,64,1,3,153,8,45,7,32,96,64,17,19,64,1,3,153,8,46,7,32,112,64,17,19,64,10,24,35,64,1,3,153,8,47,7,48,128,64,57,122,18,0,3,153,8,48,8,16,144,0,1,8,35,153,8,49,2,18,10,27,0,1,8,36,12,0,0,64,1,3,153,8,50,4,16,13,41,153,8,51,4,0,13,53,153,8,51,4,16,13,83,153,8,52,4,0,13,57,153,8,52,4,16,13,79,64,9,29,3,153,8,53,4,16,13,47,43,13,37,3,71,80,64,71,96,3,153,8,54,4,16,13,49,1,8,37,153,8,55,4,0,13,55,5,16,13,75,0,1,8,38,153,8,56,2,18,10,28,0,1,8,39,12,0,0,64,17,3,153,8,57,4,16,13,41,153,8,58,4,0,13,59,5,16,13,71,0,1,8,40,153,8,59,2,18,10,29,16,1,8,41,12,16,16,64,3,4,64,113,3,153,8,60,4,16,13,47,56,13,43,3,65,3,19,35,153,8,61,125,5,48,160,19,113,3,43,13,42,3,4,64,194,3,18,16,19,1,8,42,136,16,0,64,71,112,3,153,8,62,4,16,13,49,64,194,3,18,0,19,1,8,43,153,8,60,72,3,1,8,44,153,8,63,2,18,10,31,32,1,8,45,153,8,64,125,5,32,48,9,32,3,3,16,32,32,69,3,19,3,6,16,13,49,1,8,46,153,8,65,2,18,10,32,16,1,8,47,153,8,66,125,5,16,48,1,3,3,16,32,16,69,3,2,3,64,17,35,64,17,19,6,48,13,51,1,8,48,153,8,67,2,18,10,33,16,1,8,49,12,16,16,64,3,4,64,9,23,19,64,1,3,153,8,68,7,32,64,64,4,3,17,4,153,8,69,4,16,13,68,64,3,4,64,9,23,19,64,1,3,153,8,70,7,32,80,64,4,3,18,16,19,1,8,50,153,8,71,2,18,10,33,48,1,8,51,12,32,48,64,3,4,64,19,3,64,1,19,64,35,20,153,8,72,7,32,176,64,3,19,64,4,3,17,4,153,8,72,7,32,192,64,3,4,64,9,23,19,64,1,3,153,8,73,7,32,64,64,4,3,17,4,153,8,74,4,16,13,65,64,3,4,64,9,23,19,64,1,3,153,8,75,7,32,80,64,4,19,64,20,3,153,8,76,8,32,208,32,1,8,52,153,8,77,2,18,10,38,0,1,8,53,64,71,128,3,19,1,8,54,153,8,78,2,18,10,39,0,1,8,55,64,71,144,3,19,1,8,56,153,8,79,2,18,10,40,0,1,8,57,64,71,160,3,19,1,8,58,153,8,80,2,18,10,41,0,1,8,59,64,71,176,3,19,1,8,60,153,0,2,18,10,42,0,1,8,61,64,18,3,153,0,78,16,224,1,8,62,153,0,2,18,10,42,16,1,8,63,64,3,19,64,18,3,153,0,78,32,240,1,8,64,153,8,74,2,18,10,44,16,1,8,65,56,13,66,3,12,16,16,65,3,3,4,153,8,74,7,16,8,16,64,3,19,64,4,3,64,19,4,153,8,74,4,16,13,65,16,32,16,69,4,3,3,18,16,19,1,8,66,52,13,64,3,19,1,8,67,153,8,69,2,18,10,45,16,1,8,68,56,13,69,3,12,16,16,65,3,3,4,153,8,69,7,16,8,16,64,3,19,64,4,3,64,19,4,153,8,69,4,16,13,68,16,32,16,69,4,3,3,18,16,19,1,8,69,52,13,67,3,19,1,8,70,153,8,58,2,18,10,46,16,1,8,71,56,13,73,3,65,3,19,35,57,13,72,19,58,13,72,19,32,12,16,48,66,19,0,3,66,19,16,19,64,35,4,153,8,58,4,32,13,45,64,3,19,64,4,3,64,19,4,153,8,58,4,16,13,71,16,32,16,69,4,3,3,18,16,19,1,8,72,64,35,3,6,16,13,71,1,8,73,52,13,70,3,19,1,8,74,153,8,81,2,18,10,47,16,1,8,75,56,13,77,3,65,3,19,35,57,13,76,19,58,13,76,19,32,12,16,48,66,19,0,3,66,19,16,19,64,35,4,153,8,81,4,32,13,45,64,3,19,64,4,3,64,19,4,153,8,81,4,16,13,75,16,32,16,69,4,3,3,18,16,19,1,8,76,64,35,3,6,16,13,75,1,8,77,52,13,74,3,19,1,8,78,153,8,52,2,18,10,48,16,1,8,79,56,13,81,3,65,3,19,35,57,13,80,19,58,13,80,19,32,12,16,48,66,19,0,3,66,19,16,19,64,35,4,153,8,52,4,32,13,45,64,4,3,5,16,13,79,16,1,8,80,64,35,3,6,16,13,79,1,8,81,52,13,78,3,19,1,8,82,153,8,51,2,18,10,49,16,1,8,83,56,13,85,3,65,3,19,35,57,13,84,19,58,13,84,19,32,12,16,48,66,19,0,3,66,19,16,19,64,35,4,153,8,51,4,32,13,45,64,4,3,5,16,13,83,16,1,8,84,64,35,3,6,16,13,83,1,8,85,52,13,82,3,19,1,8,86,153,8,82,2,18,10,50,32,1,8,87,56,13,88,3,12,32,32,65,3,35,20,64,19,3,64,35,19,64,3,4,153,8,82,75,16,64,3,35,64,4,19,64,20,3,64,35,20,136,16,16,153,8,82,4,32,13,87,16,32,16,69,4,3,3,18,16,19,1,8,88,52,13,89,3,19,1,8,89,16,32,16,69,3,2,19,64,10,51,3,153,8,82,7,32,8,17,3,0,83,116,114,84,0,0,0,0,73,109,112,84,0,0,0,220,0,0,0,18,0,0,0,3,0,0,0,4,0,0,0,0,0,0,0,3,0,0,0,9,0,0,0,3,0,0,0,3,0,0,0,11,0,0,0,2,0,0,0,3,0,0,0,14,0,0,0,2,0,0,0,17,0,0,0,18,0,0,0,2,0,0,0,17,0,0,0,19,0,0,0,2,0,0,0,17,0,0,0,22,0,0,0,2,0,0,0,17,0,0,0,23,0,0,0,2,0,0,0,17,0,0,0,7,0,0,0,3,0,0,0,25,0,0,0,26,0,0,0,1,0,0,0,3,0,0,0,30,0,0,0,2,0,0,0,34,0,0,0,35,0,0,0,2,0,0,0,3,0,0,0,36,0,0,0,2,0,0,0,34,0,0,0,37,0,0,0,2,0,0,0,3,0,0,0,43,0,0,0,1,0,0,0,3,0,0,0,43,0,0,0,2,0,0,0,25,0,0,0,33,0,0,0,1,0,0,0,3,0,0,0,52,0,0,0,2,69,120,112,84,0,0,0,40,0,0,0,3,0,0,0,42,0,0,0,1,0,0,0,63,0,0,0,42,0,0,0,0,0,0,0,61,0,0,0,2,0,0,0,1,0,0,0,2,76,105,116,84,0,0,1,205,0,0,0,12,0,0,0,8,131,107,0,4,112,105,110,103,0,0,0,5,131,107,0,1,97,0,0,0,6,131,107,0,2,226,0,0,0,0,21,131,104,2,100,0,5,101,114,114,111,114,100,0,7,116,105,109,101,111,117,116,0,0,0,6,131,107,0,2,225,0,0,0,0,5,131,107,0,1,0,0,0,0,6,131,107,0,2,80,115,0,0,0,6,131,107,0,2,80,83,0,0,0,167,131,108,0,0,0,20,104,2,97,0,107,0,1,31,104,2,97,1,107,0,1,63,104,2,97,2,107,0,1,63,104,2,97,3,107,0,1,3,104,2,97,4,107,0,1,0,104,2,97,5,107,0,1,5,104,2,97,6,107,0,1,7,104,2,97,7,107,0,1,0,104,2,97,8,107,0,1,0,104,2,97,12,107,0,1,195,104,2,97,13,107,0,1,196,104,2,97,14,107,0,1,197,104,2,97,15,107,0,1,198,104,2,97,17,107,0,1,32,104,2,97,18,107,0,1,32,104,2,97,19,107,0,1,32,104,2,97,20,107,0,1,32,104,2,97,21,107,0,1,32,104,2,97,22,107,0,1,32,104,2,97,23,107,0,1,0,106,0,0,0,23,131,108,0,0,0,2,104,2,97,28,107,0,1,63,104,2,97,29,107,0,1,7,106,0,0,0,43,131,108,0,0,0,3,104,2,97,10,107,0,5,52,67,16,16,1,104,2,97,11,107,0,5,57,56,55,54,194,104,2,97,16,107,0,5,52,67,16,16,1,106,0,0,0,113,131,108,0,0,0,9,104,2,97,0,107,0,4,64,75,1,226,104,2,97,1,107,0,4,192,75,0,0,104,2,97,2,107,0,4,208,252,140,2,104,2,97,3,107,0,4,153,0,57,65,104,2,97,4,107,0,4,217,150,130,27,104,2,97,5,107,0,4,36,6,127,166,104,2,97,12,107,0,4,0,18,115,0,104,2,97,13,107,0,4,70,180,128,0,104,2,97,14,107,0,11,65,32,8,4,129,32,207,247,254,255,255,106,0,0,0,76,111,99,84,0,0,1,60,0,0,0,26,0,0,0,50,0,0,0,2,0,0,0,87,0,0,0,49,0,0,0,1,0,0,0,83,0,0,0,48,0,0,0,1,0,0,0,79,0,0,0,47,0,0,0,1,0,0,0,75,0,0,0,46,0,0,0,1,0,0,0,71,0,0,0,45,0,0,0,1,0,0,0,68,0,0,0,44,0,0,0,1,0,0,0,65,0,0,0,41,0,0,0,0,0,0,0,59,0,0,0,40,0,0,0,0,0,0,0,57,0,0,0,39,0,0,0,0,0,0,0,55,0,0,0,38,0,0,0,0,0,0,0,53,0,0,0,33,0,0,0,3,0,0,0,51,0,0,0,33,0,0,0,1,0,0,0,49,0,0,0,32,0,0,0,1,0,0,0,47,0,0,0,31,0,0,0,2,0,0,0,45,0,0,0,29,0,0,0,1,0,0,0,41,0,0,0,28,0,0,0,0,0,0,0,39,0,0,0,27,0,0,0,0,0,0,0,36,0,0,0,21,0,0,0,0,0,0,0,34,0,0,0,20,0,0,0,0,0,0,0,31,0,0,0,16,0,0,0,0,0,0,0,28,0,0,0,15,0,0,0,1,0,0,0,21,0,0,0,13,0,0,0,2,0,0,0,18,0,0,0,10,0,0,0,1,0,0,0,14,0,0,0,8,0,0,0,0,0,0,0,12,0,0,0,6,0,0,0,1,0,0,0,5,65,116,116,114,0,0,0,40,131,108,0,0,0,1,104,2,100,0,3,118,115,110,108,0,0,0,1,110,16,0,91,86,180,100,179,242,157,205,167,90,192,63,196,10,29,149,106,106,67,73,110,102,0,0,0,130,131,108,0,0,0,4,104,2,100,0,7,111,112,116,105,111,110,115,106,104,2,100,0,7,118,101,114,115,105,111,110,107,0,5,52,46,57,46,52,104,2,100,0,4,116,105,109,101,104,6,98,0,0,7,222,97,8,97,19,97,11,97,27,97,4,104,2,100,0,6,115,111,117,114,99,101,107,0,50,47,85,115,101,114,115,47,83,116,117,100,110,105,99,107,105,47,119,111,114,107,47,109,103,114,47,101,120,97,109,112,108,101,115,47,114,102,109,55,48,47,114,102,109,55,48,46,101,114,108,106,0,0,65,98,115,116,0,0,0,0,76,105,110,101,0,0,0,181,0,0,0,0,0,0,0,0,0,0,0,117,0,0,0,82,0,0,0,0,209,225,241,9,16,9,17,9,18,9,19,9,20,9,23,9,24,9,29,9,26,9,35,9,37,9,38,9,40,9,42,9,43,9,50,9,51,9,52,9,53,9,54,9,57,9,58,9,60,9,61,9,67,9,68,9,69,9,72,9,73,9,74,9,75,9,76,9,77,9,79,9,80,9,81,9,82,9,83,9,84,9,86,9,87,9,88,9,89,9,90,9,91,9,93,9,94,9,96,9,98,9,100,9,102,9,107,9,110,9,111,9,112,9,114,9,115,9,116,9,120,9,124,9,125,9,128,9,129,9,132,9,133,9,134,9,135,9,138,9,139,9,140,9,141,9,142,9,143,9,149,9,171,9,175,9,181,9,108,9,55,0,0,0};

const byte* code[] = { module_lists,module_main,module_rfm70 };

const Eterm entrypoint_a[] = {};
#endif
