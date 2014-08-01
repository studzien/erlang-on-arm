{module, main}.  %% version = 0

{exports, [{main,0},{module_info,0},{module_info,1}]}.

{attributes, []}.

{labels, 10}.


{function, main, 0, 2}.
  {label,1}.
    {line,[{location,"main.erl",5}]}.
    {func_info,{atom,main},{atom,main},0}.
  {label,2}.
    {move,{literal,[1,2,3,4]},{x,0}}.
    {call_only,1,{f,8}}.


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


{function, '-main/0-lc$^0/1-0-', 1, 8}.
  {label,7}.
    {line,[{location,"main.erl",6}]}.
    {func_info,{atom,main},{atom,'-main/0-lc$^0/1-0-'},1}.
  {label,8}.
    {test,is_nonempty_list,{f,9},[{x,0}]}.
    {allocate,1,1}.
    {get_list,{x,0},{x,1},{y,0}}.
    {move,{atom,fac},{x,1}}.
    {move,{literal,[25]},{x,2}}.
    {move,{atom,fac2},{x,0}}.
    {line,[{location,"main.erl",6}]}.
    {call_ext,3,{extfunc,erlang,spawn,3}}.
    {move,{x,0},{x,1}}.
    {move,{y,0},{x,0}}.
    {move,{x,1},{y,0}}.
    {line,[{location,"main.erl",6}]}.
    {call,1,{f,8}}.
    {test_heap,2,1}.
    {put_list,{y,0},{x,0},{x,0}}.
    {deallocate,1}.
    return.
  {label,9}.
    {test,is_nil,{f,7},[{x,0}]}.
    return.
