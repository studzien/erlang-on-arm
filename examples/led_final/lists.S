{module, lists}.  %% version = 0

{exports, [{delete,2},
           {keyfind,3},
           {keyreplace,4},
           {module_info,0},
           {module_info,1},
           {nth,2}]}.

{attributes, []}.

{labels, 24}.


{function, nth, 2, 2}.
  {label,1}.
    {line,[{location,"lists.erl",8}]}.
    {func_info,{atom,lists},{atom,nth},2}.
  {label,2}.
    {test,is_eq_exact,{f,3},[{x,0},{integer,1}]}.
    {test,is_nonempty_list,{f,1},[{x,1}]}.
    {get_list,{x,1},{x,0},{x,3}}.
    return.
  {label,3}.
    {test,is_nonempty_list,{f,1},[{x,1}]}.
    {get_list,{x,1},{x,2},{x,3}}.
    {line,[{location,"lists.erl",9}]}.
    {gc_bif,'-',{f,0},4,[{x,0},{integer,1}],{x,0}}.
    {move,{x,3},{x,1}}.
    {call_only,2,{f,2}}.


{function, delete, 2, 5}.
  {label,4}.
    {line,[{location,"lists.erl",11}]}.
    {func_info,{atom,lists},{atom,delete},2}.
  {label,5}.
    {move,nil,{x,2}}.
    {call_only,3,{f,7}}.


{function, delete, 3, 7}.
  {label,6}.
    {line,[{location,"lists.erl",14}]}.
    {func_info,{atom,lists},{atom,delete},3}.
  {label,7}.
    {test,is_nonempty_list,{f,9},[{x,1}]}.
    {get_list,{x,1},{x,3},{x,4}}.
    {test,is_eq_exact,{f,8},[{x,3},{x,0}]}.
    {allocate,1,5}.
    {move,{x,2},{x,0}}.
    {move,{x,4},{y,0}}.
    {line,[{location,"lists.erl",17}]}.
    {call_ext,1,{extfunc,lists,reverse,1}}.
    {move,{y,0},{x,1}}.
    {line,[{location,"lists.erl",17}]}.
    {call_ext_last,2,{extfunc,erlang,'++',2},1}.
  {label,8}.
    {test_heap,2,5}.
    {put_list,{x,3},{x,2},{x,2}}.
    {move,{x,4},{x,1}}.
    {call_only,3,{f,7}}.
  {label,9}.
    {test,is_nil,{f,6},[{x,1}]}.
    {move,{x,2},{x,0}}.
    {line,[{location,"lists.erl",15}]}.
    {call_ext_only,1,{extfunc,lists,reverse,1}}.


{function, keyfind, 3, 11}.
  {label,10}.
    {line,[{location,"lists.erl",21}]}.
    {func_info,{atom,lists},{atom,keyfind},3}.
  {label,11}.
    {test,is_nonempty_list,{f,13},[{x,2}]}.
    {get_list,{x,2},{x,3},{x,4}}.
    {line,[{location,"lists.erl",24}]}.
    {bif,element,{f,0},[{x,1},{x,3}],{x,2}}.
    {test,is_eq_exact,{f,12},[{x,2},{x,0}]}.
    {move,{x,3},{x,0}}.
    return.
  {label,12}.
    {move,{x,4},{x,2}}.
    {call_only,3,{f,11}}.
  {label,13}.
    {test,is_nil,{f,10},[{x,2}]}.
    {move,{atom,false},{x,0}}.
    return.


{function, keyreplace, 4, 15}.
  {label,14}.
    {line,[{location,"lists.erl",29}]}.
    {func_info,{atom,lists},{atom,keyreplace},4}.
  {label,15}.
    {move,nil,{x,4}}.
    {call_only,5,{f,17}}.


{function, keyreplace, 5, 17}.
  {label,16}.
    {line,[{location,"lists.erl",32}]}.
    {func_info,{atom,lists},{atom,keyreplace},5}.
  {label,17}.
    {test,is_nonempty_list,{f,19},[{x,2}]}.
    {allocate_zero,1,5}.
    {get_list,{x,2},{x,5},{x,6}}.
    {line,[{location,"lists.erl",35}]}.
    {bif,element,{f,0},[{x,1},{x,5}],{x,2}}.
    {test,is_eq_exact,{f,18},[{x,2},{x,0}]}.
    {test_heap,2,7}.
    {move,{x,6},{y,0}}.
    {put_list,{x,3},{x,4},{x,0}}.
    {line,[{location,"lists.erl",37}]}.
    {call_ext,1,{extfunc,lists,reverse,1}}.
    {move,{y,0},{x,1}}.
    {line,[{location,"lists.erl",37}]}.
    {call_ext_last,2,{extfunc,erlang,'++',2},1}.
  {label,18}.
    {test_heap,2,7}.
    {put_list,{x,5},{x,4},{x,4}}.
    {move,{x,6},{x,2}}.
    {call_last,5,{f,17},1}.
  {label,19}.
    {test,is_nil,{f,16},[{x,2}]}.
    {move,{x,4},{x,0}}.
    {line,[{location,"lists.erl",33}]}.
    {call_ext_only,1,{extfunc,lists,reverse,1}}.


{function, module_info, 0, 21}.
  {label,20}.
    {line,[]}.
    {func_info,{atom,lists},{atom,module_info},0}.
  {label,21}.
    {move,{atom,lists},{x,0}}.
    {line,[]}.
    {call_ext_only,1,{extfunc,erlang,get_module_info,1}}.


{function, module_info, 1, 23}.
  {label,22}.
    {line,[]}.
    {func_info,{atom,lists},{atom,module_info},1}.
  {label,23}.
    {move,{x,0},{x,1}}.
    {move,{atom,lists},{x,0}}.
    {line,[]}.
    {call_ext_only,2,{extfunc,erlang,get_module_info,2}}.
