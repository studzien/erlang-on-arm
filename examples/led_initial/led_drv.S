{module, led_drv}.  %% version = 0

{exports, [{init,2},{module_info,0},{module_info,1}]}.

{attributes, []}.

{labels, 11}.


{function, init, 2, 2}.
  {label,1}.
    {line,[{location,"led_drv.erl",7}]}.
    {func_info,{atom,led_drv},{atom,init},2}.
  {label,2}.
    {allocate,2,2}.
    {move,{x,1},{y,0}}.
    {move,{x,0},{y,1}}.
    {line,[{location,"led_drv.erl",8}]}.
    {call_ext,2,{extfunc,lpc_gpio,output,2}}.
    {move,{y,0},{x,1}}.
    {move,{y,1},{x,0}}.
    {call_last,2,{f,4},2}.


{function, loop, 2, 4}.
  {label,3}.
    {line,[{location,"led_drv.erl",11}]}.
    {func_info,{atom,led_drv},{atom,loop},2}.
  {label,4}.
    {allocate,2,2}.
    {move,{x,1},{y,0}}.
    {move,{x,0},{y,1}}.
    {line,[{location,"led_drv.erl",12}]}.
    {call_ext,2,{extfunc,lpc_gpio,low,2}}.
    {line,[{location,"led_drv.erl",13}]}.
  {label,5}.
    {wait_timeout,{f,5},{integer,1000}}.
    timeout.
    {move,{y,0},{x,1}}.
    {move,{y,1},{x,0}}.
    {line,[{location,"led_drv.erl",14}]}.
    {call_ext,2,{extfunc,lpc_gpio,high,2}}.
    {line,[{location,"led_drv.erl",15}]}.
  {label,6}.
    {wait_timeout,{f,6},{integer,1000}}.
    timeout.
    {move,{y,0},{x,1}}.
    {move,{y,1},{x,0}}.
    {call_last,2,{f,4},2}.


{function, module_info, 0, 8}.
  {label,7}.
    {line,[]}.
    {func_info,{atom,led_drv},{atom,module_info},0}.
  {label,8}.
    {move,{atom,led_drv},{x,0}}.
    {line,[]}.
    {call_ext_only,1,{extfunc,erlang,get_module_info,1}}.


{function, module_info, 1, 10}.
  {label,9}.
    {line,[]}.
    {func_info,{atom,led_drv},{atom,module_info},1}.
  {label,10}.
    {move,{x,0},{x,1}}.
    {move,{atom,led_drv},{x,0}}.
    {line,[]}.
    {call_ext_only,2,{extfunc,erlang,get_module_info,2}}.
