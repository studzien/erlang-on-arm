{module, fac}.  %% version = 0

{exports, [{fac,1},{module_info,0},{module_info,1}]}.

{attributes, []}.

{labels, 8}.


{function, fac, 1, 2}.
  {label,1}.
    {line,[{location,"fac.erl",5}]}.
    {func_info,{atom,fac},{atom,fac},1}.
  {label,2}.
    {test,is_eq_exact,{f,3},[{x,0},{integer,0}]}.
    {move,{integer,1},{x,0}}.
    return.
  {label,3}.
    {allocate_zero,1,1}.
    {line,[{location,"fac.erl",8}]}.
    {gc_bif,'-',{f,0},1,[{x,0},{integer,1}],{x,1}}.
    {move,{x,0},{y,0}}.
    {move,{x,1},{x,0}}.
    {line,[{location,"fac.erl",8}]}.
    {call_ext,1,{extfunc,fac,fac,1}}.
    {line,[{location,"fac.erl",8}]}.
    {gc_bif,'*',{f,0},1,[{y,0},{x,0}],{x,0}}.
    {deallocate,1}.
    return.


{function, module_info, 0, 5}.
  {label,4}.
    {line,[]}.
    {func_info,{atom,fac},{atom,module_info},0}.
  {label,5}.
    {move,{atom,fac},{x,0}}.
    {line,[]}.
    {call_ext_only,1,{extfunc,erlang,get_module_info,1}}.


{function, module_info, 1, 7}.
  {label,6}.
    {line,[]}.
    {func_info,{atom,fac},{atom,module_info},1}.
  {label,7}.
    {move,{x,0},{x,1}}.
    {move,{atom,fac},{x,0}}.
    {line,[]}.
    {call_ext_only,2,{extfunc,erlang,get_module_info,2}}.
