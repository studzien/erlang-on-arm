{module, led_sup}.  %% version = 0

{exports, [{module_info,0},{module_info,1},{start,0}]}.

{attributes, []}.

{labels, 40}.


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
    {allocate_zero,1,0}.
    {move,{atom,true},{x,1}}.
    {move,{atom,trap_exit},{x,0}}.
    {line,[{location,"led_sup.erl",14}]}.
    {call_ext,2,{extfunc,erlang,process_flag,2}}.
    {line,[{location,"led_sup.erl",15}]}.
    {call,0,{f,28}}.
    {line,[{location,"led_sup.erl",15}]}.
    {call,1,{f,38}}.
    {bif,self,{f,0},[],{x,1}}.
    {move,{x,0},{y,0}}.
    {move,{x,1},{x,0}}.
    {move,{atom,toggle},{x,1}}.
    {line,[{location,"led_sup.erl",16}]}.
    send.
    {move,{y,0},{x,0}}.
    {call_last,1,{f,6},1}.


{function, loop, 1, 6}.
  {label,5}.
    {line,[{location,"led_sup.erl",19}]}.
    {func_info,{atom,led_sup},{atom,loop},1}.
  {label,6}.
    {allocate,1,1}.
    {move,{x,0},{y,0}}.
    {line,[{location,"led_sup.erl",20}]}.
  {label,7}.
    {loop_rec,{f,12},{x,0}}.
    {test,is_tuple,{f,10},[{x,0}]}.
    {select_tuple_arity,{x,0},{f,11},{list,[2,{f,8},3,{f,9}]}}.
  {label,8}.
    {get_tuple_element,{x,0},0,{x,1}}.
    {get_tuple_element,{x,0},1,{x,2}}.
    {test,is_eq_exact,{f,11},[{x,1},{atom,restart}]}.
    remove_message.
    {move,{y,0},{x,1}}.
    {move,{x,2},{x,0}}.
    {call_last,2,{f,19},1}.
  {label,9}.
    {get_tuple_element,{x,0},0,{x,1}}.
    {get_tuple_element,{x,0},1,{x,2}}.
    {test,is_eq_exact,{f,11},[{x,1},{atom,'EXIT'}]}.
    remove_message.
    {move,{y,0},{x,1}}.
    {move,{x,2},{x,0}}.
    {call_last,2,{f,16},1}.
  {label,10}.
    {test,is_eq_exact,{f,11},[{x,0},{atom,toggle}]}.
    remove_message.
    {move,{y,0},{x,0}}.
    {call_last,1,{f,14},1}.
  {label,11}.
    remove_message.
    {move,{y,0},{x,0}}.
    {call_last,1,{f,6},1}.
  {label,12}.
    {wait,{f,7}}.


{function, handle_toggle, 1, 14}.
  {label,13}.
    {line,[{location,"led_sup.erl",31}]}.
    {func_info,{atom,led_sup},{atom,handle_toggle},1}.
  {label,14}.
    {allocate_zero,1,1}.
    {line,[{location,"led_sup.erl",32}]}.
    {gc_bif,length,{f,0},1,[{x,0}],{x,1}}.
    {line,[{location,"led_sup.erl",32}]}.
    {gc_bif,'div',{f,0},2,[{x,1},{integer,2}],{x,1}}.
    {move,{x,0},{y,0}}.
    {line,[{location,"led_sup.erl",32}]}.
    {call,2,{f,21}}.
    {line,[{location,"led_sup.erl",33}]}.
    {call,1,{f,34}}.
    {bif,self,{f,0},[],{x,1}}.
    {move,{atom,toggle},{x,2}}.
    {move,{integer,500},{x,0}}.
    {line,[{location,"led_sup.erl",34}]}.
    {call_ext,3,{extfunc,erlang,send_after,3}}.
    {move,{y,0},{x,0}}.
    {call_last,1,{f,6},1}.


{function, handle_exit, 2, 16}.
  {label,15}.
    {line,[{location,"led_sup.erl",37}]}.
    {func_info,{atom,led_sup},{atom,handle_exit},2}.
  {label,16}.
    {allocate,2,2}.
    {move,{x,1},{x,2}}.
    {move,{integer,4},{x,1}}.
    {move,{x,2},{y,0}}.
    {move,{x,0},{y,1}}.
    {line,[{location,"led_sup.erl",38}]}.
    {call_ext,3,{extfunc,lists,keyfind,3}}.
    {test,is_tuple,{f,17},[{x,0}]}.
    {test,test_arity,{f,17},[{x,0},4]}.
    {get_tuple_element,{x,0},0,{x,1}}.
    {get_tuple_element,{x,0},2,{x,2}}.
    {test,is_eq_exact,{f,17},[{x,1},{atom,led}]}.
    {test,is_tuple,{f,17},[{x,2}]}.
    {test,test_arity,{f,17},[{x,2},2]}.
    {get_tuple_element,{x,2},0,{x,0}}.
    {get_tuple_element,{x,2},1,{x,1}}.
    {line,[{location,"led_sup.erl",40}]}.
    {call_ext,2,{extfunc,lpc_gpio,low,2}}.
    {test_heap,3,0}.
    {bif,self,{f,0},[],{x,1}}.
    {put_tuple,2,{x,2}}.
    {put,{atom,restart}}.
    {put,{y,1}}.
    {kill,{y,1}}.
    {move,{integer,1000},{x,0}}.
    {line,[{location,"led_sup.erl",41}]}.
    {call_ext,3,{extfunc,erlang,send_after,3}}.
    {move,{y,0},{x,0}}.
    {call_last,1,{f,6},2}.
  {label,17}.
    {line,[{location,"led_sup.erl",39}]}.
    {badmatch,{x,0}}.


{function, handle_restart, 2, 19}.
  {label,18}.
    {line,[{location,"led_sup.erl",44}]}.
    {func_info,{atom,led_sup},{atom,handle_restart},2}.
  {label,19}.
    {allocate,2,2}.
    {move,{x,1},{x,2}}.
    {move,{integer,4},{x,1}}.
    {move,{x,2},{y,0}}.
    {move,{x,0},{y,1}}.
    {line,[{location,"led_sup.erl",45}]}.
    {call_ext,3,{extfunc,lists,keyfind,3}}.
    {line,[{location,"led_sup.erl",46}]}.
    {call,1,{f,26}}.
    {move,{y,0},{x,2}}.
    {move,{integer,4},{x,1}}.
    {move,{x,0},{x,3}}.
    {move,{y,1},{x,0}}.
    {trim,2,0}.
    {line,[{location,"led_sup.erl",47}]}.
    {call_ext,4,{extfunc,lists,keyreplace,4}}.
    {call_last,1,{f,6},0}.


{function, random_leds, 2, 21}.
  {label,20}.
    {line,[{location,"led_sup.erl",50}]}.
    {func_info,{atom,led_sup},{atom,random_leds},2}.
  {label,21}.
    {move,nil,{x,2}}.
    {call_only,3,{f,23}}.


{function, random_leds, 3, 23}.
  {label,22}.
    {line,[{location,"led_sup.erl",53}]}.
    {func_info,{atom,led_sup},{atom,random_leds},3}.
  {label,23}.
    {test,is_eq_exact,{f,24},[{x,1},{integer,0}]}.
    {move,{x,2},{x,0}}.
    return.
  {label,24}.
    {allocate_zero,3,3}.
    {line,[{location,"led_sup.erl",56}]}.
    {gc_bif,length,{f,0},3,[{x,0}],{x,3}}.
    {move,{x,0},{y,0}}.
    {move,{x,3},{x,0}}.
    {move,{x,1},{y,1}}.
    {move,{x,2},{y,2}}.
    {line,[{location,"led_sup.erl",56}]}.
    {call_ext,1,{extfunc,random,uniform,1}}.
    {move,{y,0},{x,1}}.
    {line,[{location,"led_sup.erl",57}]}.
    {call_ext,2,{extfunc,lists,nth,2}}.
    {move,{y,0},{x,1}}.
    {move,{x,0},{y,0}}.
    {line,[{location,"led_sup.erl",58}]}.
    {call_ext,2,{extfunc,lists,delete,2}}.
    {line,[{location,"led_sup.erl",58}]}.
    {gc_bif,'-',{f,0},1,[{y,1},{integer,1}],{x,1}}.
    {test_heap,2,2}.
    {put_list,{y,0},{y,2},{x,2}}.
    {call_last,3,{f,23},3}.


{function, start_led, 1, 26}.
  {label,25}.
    {line,[{location,"led_sup.erl",60}]}.
    {func_info,{atom,led_sup},{atom,start_led},1}.
  {label,26}.
    {test,is_tuple,{f,25},[{x,0}]}.
    {test,test_arity,{f,25},[{x,0},4]}.
    {get_tuple_element,{x,0},0,{x,1}}.
    {get_tuple_element,{x,0},1,{x,2}}.
    {get_tuple_element,{x,0},2,{x,3}}.
    {test,is_eq_exact,{f,25},[{x,1},{atom,led}]}.
    {test,is_tuple,{f,25},[{x,2}]}.
    {test,test_arity,{f,25},[{x,2},2]}.
    {get_tuple_element,{x,2},0,{x,4}}.
    {get_tuple_element,{x,2},1,{x,5}}.
    {test,is_tuple,{f,25},[{x,3}]}.
    {test,test_arity,{f,25},[{x,3},2]}.
    {allocate,5,6}.
    {get_tuple_element,{x,3},0,{x,6}}.
    {get_tuple_element,{x,3},1,{x,1}}.
    {move,{x,0},{y,4}}.
    {move,{x,6},{x,0}}.
    {move,{x,1},{y,0}}.
    {move,{x,0},{y,1}}.
    {move,{x,5},{y,2}}.
    {move,{x,4},{y,3}}.
    {line,[{location,"led_sup.erl",61}]}.
    {call_ext,2,{extfunc,lpc_gpio,output,2}}.
    {move,{y,0},{x,1}}.
    {move,{y,1},{x,0}}.
    {trim,2,3}.
    {line,[{location,"led_sup.erl",62}]}.
    {call_ext,2,{extfunc,lpc_gpio,high,2}}.
    {test_heap,4,0}.
    {put_list,{y,0},nil,{x,0}}.
    {put_list,{y,1},{x,0},{x,2}}.
    {move,{atom,init},{x,1}}.
    {trim,2,1}.
    {move,{atom,led_drv},{x,0}}.
    {line,[{location,"led_sup.erl",63}]}.
    {call_ext,3,{extfunc,erlang,spawn_link,3}}.
    {move,{y,0},{x,1}}.
    {move,{x,0},{x,2}}.
    {move,{integer,4},{x,0}}.
    {line,[{location,"led_sup.erl",64}]}.
    {call_ext_last,3,{extfunc,erlang,setelement,3},1}.


{function, children, 0, 28}.
  {label,27}.
    {line,[{location,"led_sup.erl",66}]}.
    {func_info,{atom,led_sup},{atom,children},0}.
  {label,28}.
    {move,{literal,[{led,{2,0},{2,1},undefined},{led,{2,2},{2,3},undefined}]},
          {x,0}}.
    return.


{function, module_info, 0, 30}.
  {label,29}.
    {line,[]}.
    {func_info,{atom,led_sup},{atom,module_info},0}.
  {label,30}.
    {move,{atom,led_sup},{x,0}}.
    {line,[]}.
    {call_ext_only,1,{extfunc,erlang,get_module_info,1}}.


{function, module_info, 1, 32}.
  {label,31}.
    {line,[]}.
    {func_info,{atom,led_sup},{atom,module_info},1}.
  {label,32}.
    {move,{x,0},{x,1}}.
    {move,{atom,led_sup},{x,0}}.
    {line,[]}.
    {call_ext_only,2,{extfunc,erlang,get_module_info,2}}.


{function, '-handle_toggle/1-lc$^0/1-0-', 1, 34}.
  {label,33}.
    {line,[{location,"led_sup.erl",33}]}.
    {func_info,{atom,led_sup},{atom,'-handle_toggle/1-lc$^0/1-0-'},1}.
  {label,34}.
    {test,is_nonempty_list,{f,36},[{x,0}]}.
    {get_list,{x,0},{x,1},{x,2}}.
    {test,is_tuple,{f,35},[{x,1}]}.
    {test,test_arity,{f,35},[{x,1},4]}.
    {get_tuple_element,{x,1},0,{x,3}}.
    {get_tuple_element,{x,1},3,{x,4}}.
    {test,is_eq_exact,{f,35},[{x,3},{atom,led}]}.
    {allocate,1,5}.
    {move,{atom,toggle},{x,1}}.
    {move,{x,4},{x,0}}.
    {move,{x,2},{y,0}}.
    {line,[{location,"led_sup.erl",33}]}.
    send.
    {move,{y,0},{x,0}}.
    {call_last,1,{f,34},1}.
  {label,35}.
    {move,{x,2},{x,0}}.
    {call_only,1,{f,34}}.
  {label,36}.
    {test,is_nil,{f,33},[{x,0}]}.
    return.


{function, '-init/0-lc$^0/1-0-', 1, 38}.
  {label,37}.
    {line,[{location,"led_sup.erl",15}]}.
    {func_info,{atom,led_sup},{atom,'-init/0-lc$^0/1-0-'},1}.
  {label,38}.
    {test,is_nonempty_list,{f,39},[{x,0}]}.
    {allocate,1,1}.
    {get_list,{x,0},{x,0},{y,0}}.
    {line,[{location,"led_sup.erl",15}]}.
    {call,1,{f,26}}.
    {move,{x,0},{x,1}}.
    {move,{y,0},{x,0}}.
    {move,{x,1},{y,0}}.
    {line,[{location,"led_sup.erl",15}]}.
    {call,1,{f,38}}.
    {test_heap,2,1}.
    {put_list,{y,0},{x,0},{x,0}}.
    {deallocate,1}.
    return.
  {label,39}.
    {test,is_nil,{f,37},[{x,0}]}.
    return.
