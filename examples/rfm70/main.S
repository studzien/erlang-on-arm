{module, main}.  %% version = 0

{exports, [{main,0},{module_info,0},{module_info,1}]}.

{attributes, []}.

{labels, 9}.


{function, main, 0, 2}.
  {label,1}.
    {line,[{location,"main.erl",5}]}.
    {func_info,{atom,main},{atom,main},0}.
  {label,2}.
    {allocate_zero,1,0}.
    {make_fun2,{f,8},0,0,0}.
    {move,{x,0},{y,0}}.
    {move,{atom,print},{x,1}}.
    {move,{integer,1},{x,2}}.
    {move,{atom,main},{x,0}}.
    {line,[{location,"main.erl",10}]}.
    {call_ext,3,{extfunc,erlang,make_fun,3}}.
    {test_heap,4,1}.
    {put_list,{x,0},nil,{x,1}}.
    {put_list,{y,0},{x,1},{x,0}}.
    {line,[{location,"main.erl",10}]}.
    {call_ext_last,1,{extfunc,rfm70,start,1},1}.


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


{function, '-main/0-fun-0-', 1, 8}.
  {label,7}.
    {line,[{location,"main.erl",6}]}.
    {func_info,{atom,main},{atom,'-main/0-fun-0-'},1}.
  {label,8}.
    {allocate,1,1}.
    {move,{x,0},{y,0}}.
    {line,[{location,"main.erl",7}]}.
    {call_ext,0,{extfunc,erlang,now,0}}.
    {line,[{location,"main.erl",7}]}.
    {call_ext,1,{extfunc,lpc_debug,print_term,1}}.
    {move,{y,0},{x,0}}.
    {line,[{location,"main.erl",8}]}.
    {call_ext_last,1,{extfunc,lpc_debug,print_term,1},1}.
