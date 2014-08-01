{module, max}.  %% version = 0

{exports, [{max,3},{module_info,0},{module_info,1}]}.

{attributes, []}.

{labels, 7}.


{function, max, 3, 2}.
  {label,1}.
    {line,[{location,"max.erl",5}]}.
    {func_info,{atom,max},{atom,max},3}.
  {label,2}.
    {allocate,1,3}.
    {move,{x,1},{x,3}}.
    {move,{x,0},{x,1}}.
    {move,{x,3},{x,0}}.
    {move,{x,2},{y,0}}.
    {line,[{location,"max.erl",6}]}.
    {call_ext,2,{extfunc,erlang,max,2}}.
    {move,{y,0},{x,1}}.
    {call_ext_last,2,{extfunc,erlang,max,2},1}.


{function, module_info, 0, 4}.
  {label,3}.
    {line,[]}.
    {func_info,{atom,max},{atom,module_info},0}.
  {label,4}.
    {move,{atom,max},{x,0}}.
    {line,[]}.
    {call_ext_only,1,{extfunc,erlang,get_module_info,1}}.


{function, module_info, 1, 6}.
  {label,5}.
    {line,[]}.
    {func_info,{atom,max},{atom,module_info},1}.
  {label,6}.
    {move,{x,0},{x,1}}.
    {move,{atom,max},{x,0}}.
    {line,[]}.
    {call_ext_only,2,{extfunc,erlang,get_module_info,2}}.
