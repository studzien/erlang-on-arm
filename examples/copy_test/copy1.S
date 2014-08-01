{module, copy1}.  %% version = 0

{exports, [{blink,1},{module_info,0},{module_info,1},{start,0}]}.

{attributes, []}.

{labels, 19}.


{function, start, 0, 2}.
  {label,1}.
    {line,[{location,"copy1.erl",9}]}.
    {func_info,{atom,copy1},{atom,start},0}.
  {label,2}.
    {allocate,0,0}.
    {line,[{location,"copy1.erl",10}]}.
    {call,0,{f,10}}.
    {call_last,1,{f,16},0}.


{function, blink, 1, 4}.
  {label,3}.
    {line,[{location,"copy1.erl",12}]}.
    {func_info,{atom,copy1},{atom,blink},1}.
  {label,4}.
    {test,is_tuple,{f,3},[{x,0}]}.
    {test,test_arity,{f,3},[{x,0},2]}.
    {allocate,1,1}.
    {get_tuple_element,{x,0},0,{x,1}}.
    {get_tuple_element,{x,0},1,{x,2}}.
    {move,{x,0},{y,0}}.
    {move,{x,1},{x,0}}.
    {move,{x,2},{x,1}}.
    {line,[{location,"copy1.erl",13}]}.
    {call_ext,2,{extfunc,lpc_gpio,output,2}}.
    {move,{y,0},{x,0}}.
    {line,[{location,"copy1.erl",14}]}.
    {call_ext,1,{extfunc,lpc_debug,print_term,1}}.
    {move,{y,0},{x,0}}.
    {call_last,1,{f,6},1}.


{function, do_blink, 1, 6}.
  {label,5}.
    {line,[{location,"copy1.erl",17}]}.
    {func_info,{atom,copy1},{atom,do_blink},1}.
  {label,6}.
    {test,is_tuple,{f,5},[{x,0}]}.
    {test,test_arity,{f,5},[{x,0},2]}.
    {allocate,3,1}.
    {get_tuple_element,{x,0},0,{x,1}}.
    {get_tuple_element,{x,0},1,{x,2}}.
    {move,{x,0},{y,2}}.
    {move,{x,1},{x,0}}.
    {move,{x,2},{x,1}}.
    {move,{x,1},{y,0}}.
    {move,{x,0},{y,1}}.
    {line,[{location,"copy1.erl",18}]}.
    {call_ext,2,{extfunc,lpc_gpio,high,2}}.
    {line,[{location,"copy1.erl",19}]}.
  {label,7}.
    {wait_timeout,{f,7},{integer,500}}.
    timeout.
    {move,{y,0},{x,1}}.
    {move,{y,1},{x,0}}.
    {kill,{y,0}}.
    {kill,{y,1}}.
    {line,[{location,"copy1.erl",20}]}.
    {call_ext,2,{extfunc,lpc_gpio,low,2}}.
    {line,[{location,"copy1.erl",21}]}.
  {label,8}.
    {wait_timeout,{f,8},{integer,500}}.
    timeout.
    {move,{y,2},{x,0}}.
    {call_last,1,{f,6},3}.


{function, children, 0, 10}.
  {label,9}.
    {line,[{location,"copy1.erl",24}]}.
    {func_info,{atom,copy1},{atom,children},0}.
  {label,10}.
    {move,{literal,[{led,{2,0},{2,1}},{led,{2,2},{2,3}}]},{x,0}}.
    return.


{function, module_info, 0, 12}.
  {label,11}.
    {line,[]}.
    {func_info,{atom,copy1},{atom,module_info},0}.
  {label,12}.
    {move,{atom,copy1},{x,0}}.
    {line,[]}.
    {call_ext_only,1,{extfunc,erlang,get_module_info,1}}.


{function, module_info, 1, 14}.
  {label,13}.
    {line,[]}.
    {func_info,{atom,copy1},{atom,module_info},1}.
  {label,14}.
    {move,{x,0},{x,1}}.
    {move,{atom,copy1},{x,0}}.
    {line,[]}.
    {call_ext_only,2,{extfunc,erlang,get_module_info,2}}.


{function, '-start/0-lc$^0/1-0-', 1, 16}.
  {label,15}.
    {line,[{location,"copy1.erl",10}]}.
    {func_info,{atom,copy1},{atom,'-start/0-lc$^0/1-0-'},1}.
  {label,16}.
    {test,is_nonempty_list,{f,18},[{x,0}]}.
    {get_list,{x,0},{x,1},{x,2}}.
    {test,is_tuple,{f,17},[{x,1}]}.
    {test,test_arity,{f,17},[{x,1},3]}.
    {get_tuple_element,{x,1},0,{x,3}}.
    {get_tuple_element,{x,1},1,{x,4}}.
    {test,is_eq_exact,{f,17},[{x,3},{atom,led}]}.
    {allocate_heap,1,2,5}.
    {move,{x,2},{y,0}}.
    {put_list,{x,4},nil,{x,2}}.
    {move,{atom,blink},{x,1}}.
    {move,{atom,copy1},{x,0}}.
    {line,[{location,"copy1.erl",10}]}.
    {call_ext,3,{extfunc,erlang,spawn,3}}.
    {move,{x,0},{x,1}}.
    {move,{y,0},{x,0}}.
    {move,{x,1},{y,0}}.
    {line,[{location,"copy1.erl",10}]}.
    {call,1,{f,16}}.
    {test_heap,2,1}.
    {put_list,{y,0},{x,0},{x,0}}.
    {deallocate,1}.
    return.
  {label,17}.
    {move,{x,2},{x,0}}.
    {call_only,1,{f,16}}.
  {label,18}.
    {test,is_nil,{f,15},[{x,0}]}.
    return.
