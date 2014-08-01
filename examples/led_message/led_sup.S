{module, led_sup}.  %% version = 0

{exports, [{module_info,0},{module_info,1},{start,0}]}.

{attributes, []}.

{labels, 22}.


{function, start, 0, 2}.
  {label,1}.
    {line,[{location,"led_sup.erl",10}]}.
    {func_info,{atom,led_sup},{atom,start},0}.
  {label,2}.
    {allocate,0,0}.
    {line,[{location,"led_sup.erl",11}]}.
    {call_ext,0,{extfunc,lpc_debug,print_heap_size,0}}.
    {call_last,0,{f,4},0}.


{function, init, 0, 4}.
  {label,3}.
    {line,[{location,"led_sup.erl",14}]}.
    {func_info,{atom,led_sup},{atom,init},0}.
  {label,4}.
    {allocate,0,0}.
    {line,[{location,"led_sup.erl",15}]}.
    {call,0,{f,11}}.
    {line,[{location,"led_sup.erl",15}]}.
    {call,1,{f,20}}.
    {call_last,1,{f,6},0}.


{function, loop, 1, 6}.
  {label,5}.
    {line,[{location,"led_sup.erl",18}]}.
    {func_info,{atom,led_sup},{atom,loop},1}.
  {label,6}.
    {allocate,1,1}.
    {move,{x,0},{y,0}}.
    {line,[{location,"led_sup.erl",19}]}.
    {call,1,{f,17}}.
    {line,[{location,"led_sup.erl",20}]}.
  {label,7}.
    {wait_timeout,{f,7},{integer,1000}}.
    timeout.
    {move,{y,0},{x,0}}.
    {call_last,1,{f,6},1}.


{function, start_led, 1, 9}.
  {label,8}.
    {line,[{location,"led_sup.erl",24}]}.
    {func_info,{atom,led_sup},{atom,start_led},1}.
  {label,9}.
    {test,is_tuple,{f,8},[{x,0}]}.
    {test,test_arity,{f,8},[{x,0},4]}.
    {get_tuple_element,{x,0},0,{x,1}}.
    {get_tuple_element,{x,0},1,{x,2}}.
    {get_tuple_element,{x,0},2,{x,3}}.
    {test,is_eq_exact,{f,8},[{x,1},{atom,led}]}.
    {test,is_tuple,{f,8},[{x,2}]}.
    {test,test_arity,{f,8},[{x,2},2]}.
    {get_tuple_element,{x,2},0,{x,4}}.
    {get_tuple_element,{x,2},1,{x,5}}.
    {test,is_tuple,{f,8},[{x,3}]}.
    {test,test_arity,{f,8},[{x,3},2]}.
    {allocate,5,6}.
    {get_tuple_element,{x,3},0,{x,6}}.
    {get_tuple_element,{x,3},1,{x,1}}.
    {move,{x,0},{y,4}}.
    {move,{x,6},{x,0}}.
    {move,{x,1},{y,0}}.
    {move,{x,0},{y,1}}.
    {move,{x,5},{y,2}}.
    {move,{x,4},{y,3}}.
    {line,[{location,"led_sup.erl",25}]}.
    {call_ext,2,{extfunc,lpc_gpio,output,2}}.
    {move,{y,0},{x,1}}.
    {move,{y,1},{x,0}}.
    {trim,2,3}.
    {line,[{location,"led_sup.erl",26}]}.
    {call_ext,2,{extfunc,lpc_gpio,low,2}}.
    {test_heap,4,0}.
    {put_list,{y,0},nil,{x,0}}.
    {put_list,{y,1},{x,0},{x,2}}.
    {move,{atom,init},{x,1}}.
    {kill,{y,1}}.
    {trim,1,2}.
    {move,{atom,led_drv},{x,0}}.
    {line,[{location,"led_sup.erl",27}]}.
    {call_ext,3,{extfunc,erlang,spawn,3}}.
    {move,{x,0},{y,0}}.
    {line,[{location,"led_sup.erl",28}]}.
    {call_ext,0,{extfunc,lpc_debug,print_heap_size,0}}.
    {move,{y,1},{x,1}}.
    {move,{y,0},{x,2}}.
    {move,{integer,4},{x,0}}.
    {line,[{location,"led_sup.erl",29}]}.
    {call_ext_last,3,{extfunc,erlang,setelement,3},2}.


{function, children, 0, 11}.
  {label,10}.
    {line,[{location,"led_sup.erl",31}]}.
    {func_info,{atom,led_sup},{atom,children},0}.
  {label,11}.
    {move,{literal,[{led,{2,0},{2,1},undefined}]},{x,0}}.
    return.


{function, module_info, 0, 13}.
  {label,12}.
    {line,[]}.
    {func_info,{atom,led_sup},{atom,module_info},0}.
  {label,13}.
    {move,{atom,led_sup},{x,0}}.
    {line,[]}.
    {call_ext_only,1,{extfunc,erlang,get_module_info,1}}.


{function, module_info, 1, 15}.
  {label,14}.
    {line,[]}.
    {func_info,{atom,led_sup},{atom,module_info},1}.
  {label,15}.
    {move,{x,0},{x,1}}.
    {move,{atom,led_sup},{x,0}}.
    {line,[]}.
    {call_ext_only,2,{extfunc,erlang,get_module_info,2}}.


{function, '-loop/1-lc$^0/1-0-', 1, 17}.
  {label,16}.
    {line,[{location,"led_sup.erl",19}]}.
    {func_info,{atom,led_sup},{atom,'-loop/1-lc$^0/1-0-'},1}.
  {label,17}.
    {test,is_nonempty_list,{f,18},[{x,0}]}.
    {allocate,1,1}.
    {get_list,{x,0},{x,0},{y,0}}.
    {move,{atom,toggle},{x,1}}.
    {line,[{location,"led_sup.erl",19}]}.
    send.
    {move,{y,0},{x,0}}.
    {call_last,1,{f,17},1}.
  {label,18}.
    {test,is_nil,{f,16},[{x,0}]}.
    return.


{function, '-init/0-lc$^0/1-0-', 1, 20}.
  {label,19}.
    {line,[{location,"led_sup.erl",15}]}.
    {func_info,{atom,led_sup},{atom,'-init/0-lc$^0/1-0-'},1}.
  {label,20}.
    {test,is_nonempty_list,{f,21},[{x,0}]}.
    {allocate,1,1}.
    {get_list,{x,0},{x,0},{y,0}}.
    {line,[{location,"led_sup.erl",15}]}.
    {call,1,{f,9}}.
    {move,{x,0},{x,1}}.
    {move,{y,0},{x,0}}.
    {move,{x,1},{y,0}}.
    {line,[{location,"led_sup.erl",15}]}.
    {call,1,{f,20}}.
    {test_heap,2,1}.
    {put_list,{y,0},{x,0},{x,0}}.
    {deallocate,1}.
    return.
  {label,21}.
    {test,is_nil,{f,19},[{x,0}]}.
    return.
