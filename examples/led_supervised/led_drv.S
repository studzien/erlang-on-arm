{module, led_drv}.  %% version = 0

{exports, [{init,4},{module_info,0},{module_info,1}]}.

{attributes, []}.

{labels, 16}.


{function, init, 4, 2}.
  {label,1}.
    {line,[{location,"led_drv.erl",5}]}.
    {func_info,{atom,led_drv},{atom,init},4}.
  {label,2}.
    {allocate,4,4}.
    {move,{x,3},{y,0}}.
    {move,{x,2},{y,1}}.
    {move,{x,1},{y,2}}.
    {move,{x,0},{y,3}}.
    {line,[{location,"led_drv.erl",6}]}.
    {call_ext,2,{extfunc,lpc_gpio,output,2}}.
    {move,{y,2},{x,1}}.
    {move,{y,3},{x,0}}.
    {line,[{location,"led_drv.erl",7}]}.
    {call_ext,2,{extfunc,lpc_gpio,high,2}}.
    {move,{y,1},{x,2}}.
    {move,{y,0},{x,3}}.
    {move,{y,2},{x,1}}.
    {move,{atom,low},{x,4}}.
    {move,{y,3},{x,0}}.
    {call_last,5,{f,4},4}.


{function, loop, 5, 4}.
  {label,3}.
    {line,[{location,"led_drv.erl",10}]}.
    {func_info,{atom,led_drv},{atom,loop},5}.
  {label,4}.
    {allocate,5,5}.
    {move,{x,4},{y,0}}.
    {move,{x,3},{y,1}}.
    {move,{x,2},{y,2}}.
    {move,{x,1},{y,3}}.
    {move,{x,0},{y,4}}.
    {line,[{location,"led_drv.erl",11}]}.
  {label,5}.
    {loop_rec,{f,7},{x,0}}.
    {test,is_tuple,{f,6},[{x,0}]}.
    {test,test_arity,{f,6},[{x,0},2]}.
    {get_tuple_element,{x,0},0,{x,1}}.
    {get_tuple_element,{x,0},1,{x,2}}.
    {test,is_eq_exact,{f,6},[{x,1},{atom,toggle}]}.
    {test,is_tuple,{f,6},[{x,2}]}.
    {test,test_arity,{f,6},[{x,2},3]}.
    remove_message.
    {move,{atom,lpc_gpio},{x,2}}.
    {move,{y,3},{x,1}}.
    {move,{y,0},{x,3}}.
    {move,{y,4},{x,0}}.
    {line,[{location,"led_drv.erl",13}]}.
    {apply,2}.
    {move,{y,1},{x,1}}.
    {move,{y,2},{x,0}}.
    {line,[{location,"led_drv.erl",14}]}.
    {call_ext,2,{extfunc,lpc_gpio,low,2}}.
    {move,{y,0},{x,0}}.
    {trim,1,4}.
    {line,[{location,"led_drv.erl",15}]}.
    {call,1,{f,9}}.
    {move,{y,1},{x,2}}.
    {move,{y,0},{x,3}}.
    {move,{y,2},{x,1}}.
    {move,{x,0},{x,4}}.
    {move,{y,3},{x,0}}.
    {call_last,5,{f,4},4}.
  {label,6}.
    remove_message.
    {move,{y,2},{x,2}}.
    {move,{y,1},{x,3}}.
    {move,{y,3},{x,1}}.
    {move,{y,0},{x,4}}.
    {move,{y,4},{x,0}}.
    {call_last,5,{f,4},5}.
  {label,7}.
    {wait_timeout,{f,5},{integer,500}}.
    timeout.
    {move,{y,1},{x,1}}.
    {move,{y,2},{x,0}}.
    {line,[{location,"led_drv.erl",19}]}.
    {call_ext,2,{extfunc,lpc_gpio,high,2}}.
    {move,{y,2},{x,2}}.
    {move,{y,1},{x,3}}.
    {move,{y,3},{x,1}}.
    {move,{y,0},{x,4}}.
    {move,{y,4},{x,0}}.
    {call_last,5,{f,4},5}.


{function, next, 1, 9}.
  {label,8}.
    {line,[{location,"led_drv.erl",23}]}.
    {func_info,{atom,led_drv},{atom,next},1}.
  {label,9}.
    {test,is_atom,{f,8},[{x,0}]}.
    {select_val,{x,0},{f,8},{list,[{atom,high},{f,10},{atom,low},{f,11}]}}.
  {label,10}.
    {move,{atom,low},{x,0}}.
    return.
  {label,11}.
    {move,{atom,high},{x,0}}.
    return.


{function, module_info, 0, 13}.
  {label,12}.
    {line,[]}.
    {func_info,{atom,led_drv},{atom,module_info},0}.
  {label,13}.
    {move,{atom,led_drv},{x,0}}.
    {line,[]}.
    {call_ext_only,1,{extfunc,erlang,get_module_info,1}}.


{function, module_info, 1, 15}.
  {label,14}.
    {line,[]}.
    {func_info,{atom,led_drv},{atom,module_info},1}.
  {label,15}.
    {move,{x,0},{x,1}}.
    {move,{atom,led_drv},{x,0}}.
    {line,[]}.
    {call_ext_only,2,{extfunc,erlang,get_module_info,2}}.
