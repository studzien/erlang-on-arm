{module, led_sup}.  %% version = 0

{exports, [{module_info,0},{module_info,1},{start,0}]}.

{attributes, []}.

{labels, 28}.


{function, start, 0, 2}.
  {label,1}.
    {line,[{location,"led_sup.erl",10}]}.
    {func_info,{atom,led_sup},{atom,start},0}.
  {label,2}.
    {call_only,0,{f,4}}.


{function, init, 0, 4}.
  {label,3}.
    {line,[{location,"led_sup.erl",13}]}.
    {func_info,{atom,led_sup},{atom,init},0}.
  {label,4}.
    {allocate,0,0}.
    {line,[{location,"led_sup.erl",14}]}.
    {call,0,{f,16}}.
    {line,[{location,"led_sup.erl",14}]}.
    {call,1,{f,26}}.
    {call_last,1,{f,6},0}.


{function, loop, 1, 6}.
  {label,5}.
    {line,[{location,"led_sup.erl",17}]}.
    {func_info,{atom,led_sup},{atom,loop},1}.
  {label,6}.
    {allocate_zero,2,1}.
    {line,[{location,"led_sup.erl",18}]}.
    {gc_bif,length,{f,0},1,[{x,0}],{x,1}}.
    {line,[{location,"led_sup.erl",18}]}.
    {gc_bif,'div',{f,0},2,[{x,1},{integer,2}],{x,1}}.
    {move,{x,0},{y,1}}.
    {line,[{location,"led_sup.erl",18}]}.
    {call,2,{f,9}}.
    {move,{x,0},{y,0}}.
    {line,[{location,"led_sup.erl",19}]}.
    {call_ext,1,{extfunc,lpc_debug,print_term,1}}.
    {move,{y,0},{x,0}}.
    {kill,{y,0}}.
    {line,[{location,"led_sup.erl",20}]}.
    {call,1,{f,22}}.
    {line,[{location,"led_sup.erl",21}]}.
  {label,7}.
    {wait_timeout,{f,7},{integer,1000}}.
    timeout.
    {move,{y,1},{x,0}}.
    {call_last,1,{f,6},2}.


{function, random_leds, 2, 9}.
  {label,8}.
    {line,[{location,"led_sup.erl",24}]}.
    {func_info,{atom,led_sup},{atom,random_leds},2}.
  {label,9}.
    {move,nil,{x,2}}.
    {call_only,3,{f,11}}.


{function, random_leds, 3, 11}.
  {label,10}.
    {line,[{location,"led_sup.erl",27}]}.
    {func_info,{atom,led_sup},{atom,random_leds},3}.
  {label,11}.
    {test,is_eq_exact,{f,12},[{x,1},{integer,0}]}.
    {move,{x,2},{x,0}}.
    return.
  {label,12}.
    {allocate_zero,3,3}.
    {line,[{location,"led_sup.erl",30}]}.
    {gc_bif,length,{f,0},3,[{x,0}],{x,3}}.
    {move,{x,0},{y,0}}.
    {move,{x,3},{x,0}}.
    {move,{x,1},{y,1}}.
    {move,{x,2},{y,2}}.
    {line,[{location,"led_sup.erl",30}]}.
    {call_ext,1,{extfunc,random,uniform,1}}.
    {move,{y,0},{x,1}}.
    {line,[{location,"led_sup.erl",31}]}.
    {call_ext,2,{extfunc,lists,nth,2}}.
    {move,{y,0},{x,1}}.
    {move,{x,0},{y,0}}.
    {line,[{location,"led_sup.erl",32}]}.
    {call_ext,2,{extfunc,lists,delete,2}}.
    {line,[{location,"led_sup.erl",32}]}.
    {gc_bif,'-',{f,0},1,[{y,1},{integer,1}],{x,1}}.
    {test_heap,2,2}.
    {put_list,{y,0},{y,2},{x,2}}.
    {call_last,3,{f,11},3}.


{function, start_led, 1, 14}.
  {label,13}.
    {line,[{location,"led_sup.erl",34}]}.
    {func_info,{atom,led_sup},{atom,start_led},1}.
  {label,14}.
    {test,is_tuple,{f,13},[{x,0}]}.
    {test,test_arity,{f,13},[{x,0},4]}.
    {get_tuple_element,{x,0},0,{x,1}}.
    {get_tuple_element,{x,0},1,{x,2}}.
    {get_tuple_element,{x,0},2,{x,3}}.
    {test,is_eq_exact,{f,13},[{x,1},{atom,led}]}.
    {test,is_tuple,{f,13},[{x,2}]}.
    {test,test_arity,{f,13},[{x,2},2]}.
    {get_tuple_element,{x,2},0,{x,4}}.
    {get_tuple_element,{x,2},1,{x,5}}.
    {test,is_tuple,{f,13},[{x,3}]}.
    {test,test_arity,{f,13},[{x,3},2]}.
    {allocate,5,6}.
    {get_tuple_element,{x,3},0,{x,6}}.
    {get_tuple_element,{x,3},1,{x,1}}.
    {move,{x,0},{y,4}}.
    {move,{x,6},{x,0}}.
    {move,{x,1},{y,0}}.
    {move,{x,0},{y,1}}.
    {move,{x,5},{y,2}}.
    {move,{x,4},{y,3}}.
    {line,[{location,"led_sup.erl",35}]}.
    {call_ext,2,{extfunc,lpc_gpio,output,2}}.
    {move,{y,0},{x,1}}.
    {move,{y,1},{x,0}}.
    {line,[{location,"led_sup.erl",36}]}.
    {call_ext,2,{extfunc,lpc_gpio,low,2}}.
    {test_heap,8,0}.
    {put_list,{y,0},nil,{x,0}}.
    {put_list,{y,1},{x,0},{x,0}}.
    {put_list,{y,2},{x,0},{x,0}}.
    {put_list,{y,3},{x,0},{x,2}}.
    {move,{atom,init},{x,1}}.
    {trim,4,1}.
    {move,{atom,led_drv},{x,0}}.
    {line,[{location,"led_sup.erl",37}]}.
    {call_ext,3,{extfunc,erlang,spawn,3}}.
    {move,{y,0},{x,1}}.
    {move,{x,0},{x,2}}.
    {move,{integer,4},{x,0}}.
    {line,[{location,"led_sup.erl",38}]}.
    {call_ext_last,3,{extfunc,erlang,setelement,3},1}.


{function, children, 0, 16}.
  {label,15}.
    {line,[{location,"led_sup.erl",40}]}.
    {func_info,{atom,led_sup},{atom,children},0}.
  {label,16}.
    {move,{literal,[{led,{2,0},{2,1},undefined},
                    {led,{2,2},{2,3},undefined},
                    {led,{2,4},{2,5},undefined},
                    {led,{2,6},{2,7},undefined}]},
          {x,0}}.
    return.


{function, module_info, 0, 18}.
  {label,17}.
    {line,[]}.
    {func_info,{atom,led_sup},{atom,module_info},0}.
  {label,18}.
    {move,{atom,led_sup},{x,0}}.
    {line,[]}.
    {call_ext_only,1,{extfunc,erlang,get_module_info,1}}.


{function, module_info, 1, 20}.
  {label,19}.
    {line,[]}.
    {func_info,{atom,led_sup},{atom,module_info},1}.
  {label,20}.
    {move,{x,0},{x,1}}.
    {move,{atom,led_sup},{x,0}}.
    {line,[]}.
    {call_ext_only,2,{extfunc,erlang,get_module_info,2}}.


{function, '-loop/1-lc$^0/1-0-', 1, 22}.
  {label,21}.
    {line,[{location,"led_sup.erl",20}]}.
    {func_info,{atom,led_sup},{atom,'-loop/1-lc$^0/1-0-'},1}.
  {label,22}.
    {test,is_nonempty_list,{f,24},[{x,0}]}.
    {get_list,{x,0},{x,1},{x,2}}.
    {test,is_tuple,{f,23},[{x,1}]}.
    {test,test_arity,{f,23},[{x,1},4]}.
    {get_tuple_element,{x,1},0,{x,3}}.
    {get_tuple_element,{x,1},3,{x,4}}.
    {test,is_eq_exact,{f,23},[{x,3},{atom,led}]}.
    {allocate,2,5}.
    {move,{x,4},{y,0}}.
    {move,{x,2},{y,1}}.
    {line,[{location,"led_sup.erl",20}]}.
    {call_ext,0,{extfunc,erlang,now,0}}.
    {test_heap,4,1}.
    {put_list,{x,0},nil,{x,2}}.
    {put_list,{atom,toggle},{x,2},{x,1}}.
    {move,{y,0},{x,0}}.
    {trim,1,1}.
    {line,[{location,"led_sup.erl",20}]}.
    send.
    {move,{y,0},{x,0}}.
    {call_last,1,{f,22},1}.
  {label,23}.
    {move,{x,2},{x,0}}.
    {call_only,1,{f,22}}.
  {label,24}.
    {test,is_nil,{f,21},[{x,0}]}.
    return.


{function, '-init/0-lc$^0/1-0-', 1, 26}.
  {label,25}.
    {line,[{location,"led_sup.erl",14}]}.
    {func_info,{atom,led_sup},{atom,'-init/0-lc$^0/1-0-'},1}.
  {label,26}.
    {test,is_nonempty_list,{f,27},[{x,0}]}.
    {allocate,1,1}.
    {get_list,{x,0},{x,0},{y,0}}.
    {line,[{location,"led_sup.erl",14}]}.
    {call,1,{f,14}}.
    {move,{x,0},{x,1}}.
    {move,{y,0},{x,0}}.
    {move,{x,1},{y,0}}.
    {line,[{location,"led_sup.erl",14}]}.
    {call,1,{f,26}}.
    {test_heap,2,1}.
    {put_list,{y,0},{x,0},{x,0}}.
    {deallocate,1}.
    return.
  {label,27}.
    {test,is_nil,{f,25},[{x,0}]}.
    return.
