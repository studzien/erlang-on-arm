{module, led_drv}.  %% version = 0

{exports, [{init,2},{module_info,0},{module_info,1}]}.

{attributes, []}.

{labels, 17}.


{function, init, 2, 2}.
  {label,1}.
    {line,[{location,"led_drv.erl",5}]}.
    {func_info,{atom,led_drv},{atom,init},2}.
  {label,2}.
    {allocate,2,2}.
    {move,{x,1},{y,0}}.
    {move,{x,0},{y,1}}.
    {line,[{location,"led_drv.erl",6}]}.
    {call_ext,2,{extfunc,lpc_gpio,output,2}}.
    {move,{y,0},{x,1}}.
    {move,{y,1},{x,0}}.
    {line,[{location,"led_drv.erl",7}]}.
    {call_ext,2,{extfunc,lpc_gpio,high,2}}.
    {move,{integer,0},{x,2}}.
    {move,{y,0},{x,1}}.
    {move,{atom,low},{x,3}}.
    {move,{y,1},{x,0}}.
    {call_last,4,{f,4},2}.


{function, loop, 4, 4}.
  {label,3}.
    {line,[{location,"led_drv.erl",10}]}.
    {func_info,{atom,led_drv},{atom,loop},4}.
  {label,4}.
    {test,is_eq_exact,{f,5},[{x,2},{integer,1}]}.
    {move,{atom,timeout},{x,0}}.
    {line,[{location,"led_drv.erl",11}]}.
    {call_ext,1,{extfunc,erlang,exit,1}}.
  {label,5}.
    {allocate,4,4}.
    {move,{x,3},{y,0}}.
    {move,{x,2},{y,1}}.
    {move,{x,1},{y,2}}.
    {move,{x,0},{y,3}}.
    {line,[{location,"led_drv.erl",13}]}.
  {label,6}.
    {loop_rec,{f,8},{x,0}}.
    {test,is_eq_exact,{f,7},[{x,0},{atom,toggle}]}.
    remove_message.
    {move,{atom,lpc_gpio},{x,2}}.
    {move,{y,2},{x,1}}.
    {move,{y,0},{x,3}}.
    {move,{y,3},{x,0}}.
    {line,[{location,"led_drv.erl",15}]}.
    {apply,2}.
    {move,{y,0},{x,0}}.
    {trim,1,3}.
    {line,[{location,"led_drv.erl",16}]}.
    {call,1,{f,10}}.
    {move,{y,0},{x,2}}.
    {move,{y,1},{x,1}}.
    {move,{x,0},{x,3}}.
    {move,{y,2},{x,0}}.
    {call_last,4,{f,4},3}.
  {label,7}.
    remove_message.
    {move,{y,1},{x,2}}.
    {move,{y,2},{x,1}}.
    {move,{y,0},{x,3}}.
    {move,{y,3},{x,0}}.
    {call_last,4,{f,4},4}.
  {label,8}.
    {wait_timeout,{f,6},{integer,1500}}.
    timeout.
    {line,[{location,"led_drv.erl",20}]}.
    {gc_bif,'+',{f,0},0,[{y,1},{integer,1}],{x,2}}.
    {move,{y,2},{x,1}}.
    {move,{y,0},{x,3}}.
    {move,{y,3},{x,0}}.
    {call_last,4,{f,4},4}.


{function, next, 1, 10}.
  {label,9}.
    {line,[{location,"led_drv.erl",23}]}.
    {func_info,{atom,led_drv},{atom,next},1}.
  {label,10}.
    {test,is_atom,{f,9},[{x,0}]}.
    {select_val,{x,0},{f,9},{list,[{atom,high},{f,11},{atom,low},{f,12}]}}.
  {label,11}.
    {move,{atom,low},{x,0}}.
    return.
  {label,12}.
    {move,{atom,high},{x,0}}.
    return.


{function, module_info, 0, 14}.
  {label,13}.
    {line,[]}.
    {func_info,{atom,led_drv},{atom,module_info},0}.
  {label,14}.
    {move,{atom,led_drv},{x,0}}.
    {line,[]}.
    {call_ext_only,1,{extfunc,erlang,get_module_info,1}}.


{function, module_info, 1, 16}.
  {label,15}.
    {line,[]}.
    {func_info,{atom,led_drv},{atom,module_info},1}.
  {label,16}.
    {move,{x,0},{x,1}}.
    {move,{atom,led_drv},{x,0}}.
    {line,[]}.
    {call_ext_only,2,{extfunc,erlang,get_module_info,2}}.
