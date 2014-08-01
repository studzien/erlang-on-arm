{module, fac2}.  %% version = 0

{exports, [{fac,1},{module_info,0},{module_info,1}]}.

{attributes, []}.

{labels, 10}.


{function, fac, 1, 2}.
  {label,1}.
    {line,[{location,"fac2.erl",5}]}.
    {func_info,{atom,fac2},{atom,fac},1}.
  {label,2}.
    {move,{integer,1},{x,1}}.
    {call_only,2,{f,4}}.


{function, fac, 2, 4}.
  {label,3}.
    {line,[{location,"fac2.erl",8}]}.
    {func_info,{atom,fac2},{atom,fac},2}.
  {label,4}.
    {test,is_eq_exact,{f,5},[{x,0},{integer,0}]}.
    {move,{x,1},{x,0}}.
    return.
  {label,5}.
    {line,[{location,"fac2.erl",9}]}.
    {gc_bif,'-',{f,0},2,[{x,0},{integer,1}],{x,2}}.
    {line,[{location,"fac2.erl",9}]}.
    {gc_bif,'*',{f,0},3,[{x,0},{x,1}],{x,1}}.
    {move,{x,2},{x,0}}.
    {call_only,2,{f,4}}.


{function, module_info, 0, 7}.
  {label,6}.
    {line,[]}.
    {func_info,{atom,fac2},{atom,module_info},0}.
  {label,7}.
    {move,{atom,fac2},{x,0}}.
    {line,[]}.
    {call_ext_only,1,{extfunc,erlang,get_module_info,1}}.


{function, module_info, 1, 9}.
  {label,8}.
    {line,[]}.
    {func_info,{atom,fac2},{atom,module_info},1}.
  {label,9}.
    {move,{x,0},{x,1}}.
    {move,{atom,fac2},{x,0}}.
    {line,[]}.
    {call_ext_only,2,{extfunc,erlang,get_module_info,2}}.
