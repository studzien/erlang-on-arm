{module, random}.  %% version = 0

{exports, [{module_info,0},{module_info,1},{uniform,1}]}.

{attributes, []}.

{labels, 8}.


{function, uniform, 1, 2}.
  {label,1}.
    {line,[{location,"random.erl",5}]}.
    {func_info,{atom,random},{atom,uniform},1}.
  {label,2}.
    {allocate,1,1}.
    {move,{x,0},{y,0}}.
    {line,[{location,"random.erl",6}]}.
    {call_ext,0,{extfunc,erlang,now,0}}.
    {test,is_tuple,{f,3},[{x,0}]}.
    {test,test_arity,{f,3},[{x,0},3]}.
    {get_tuple_element,{x,0},2,{x,1}}.
    {line,[{location,"random.erl",7}]}.
    {gc_bif,'rem',{f,0},2,[{x,1},{y,0}],{x,0}}.
    {line,[{location,"random.erl",7}]}.
    {gc_bif,'+',{f,0},2,[{x,0},{integer,1}],{x,0}}.
    {deallocate,1}.
    return.
  {label,3}.
    {line,[{location,"random.erl",6}]}.
    {badmatch,{x,0}}.


{function, module_info, 0, 5}.
  {label,4}.
    {line,[]}.
    {func_info,{atom,random},{atom,module_info},0}.
  {label,5}.
    {move,{atom,random},{x,0}}.
    {line,[]}.
    {call_ext_only,1,{extfunc,erlang,get_module_info,1}}.


{function, module_info, 1, 7}.
  {label,6}.
    {line,[]}.
    {func_info,{atom,random},{atom,module_info},1}.
  {label,7}.
    {move,{x,0},{x,1}}.
    {move,{atom,random},{x,0}}.
    {line,[]}.
    {call_ext_only,2,{extfunc,erlang,get_module_info,2}}.
