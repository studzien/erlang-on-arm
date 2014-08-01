{module, main}.  %% version = 0

{exports, [{main,0},{module_info,0},{module_info,1}]}.

{attributes, []}.

{labels, 7}.


{function, main, 0, 2}.
  {label,1}.
    {line,[{location,"main.erl",5}]}.
    {func_info,{atom,main},{atom,main},0}.
  {label,2}.
    {line,[{location,"main.erl",6}]}.
    {call_ext_only,0,{extfunc,copy3,start,0}}.


{function, module_info, 0, 4}.
  {label,3}.
    {line,[]}.
    {func_info,{atom,main},{atom,module_info},0}.
  {label,4}.
    {move,{atom,main},{x,0}}.
    {line,[]}.
    {call_ext_only,1,{extfunc,erlang,get_module_info,1}}.


{function, module_info, 1, 6}.
  {label,5}.
    {line,[]}.
    {func_info,{atom,main},{atom,module_info},1}.
  {label,6}.
    {move,{x,0},{x,1}}.
    {move,{atom,main},{x,0}}.
    {line,[]}.
    {call_ext_only,2,{extfunc,erlang,get_module_info,2}}.
