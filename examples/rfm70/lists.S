{module, lists}.  %% version = 0

{exports, [{duplicate,2},{module_info,0},{module_info,1},{nthtail,2}]}.

{attributes, []}.

{labels, 13}.


{function, duplicate, 2, 2}.
  {label,1}.
    {line,[{location,"lists.erl",6}]}.
    {func_info,{atom,lists},{atom,duplicate},2}.
  {label,2}.
    {move,nil,{x,2}}.
    {call_only,3,{f,4}}.


{function, duplicate, 3, 4}.
  {label,3}.
    {line,[{location,"lists.erl",9}]}.
    {func_info,{atom,lists},{atom,duplicate},3}.
  {label,4}.
    {test,is_eq_exact,{f,5},[{x,0},{integer,0}]}.
    {move,{x,2},{x,0}}.
    return.
  {label,5}.
    {line,[{location,"lists.erl",12}]}.
    {gc_bif,'-',{f,0},3,[{x,0},{integer,1}],{x,0}}.
    {test_heap,2,3}.
    {put_list,{x,1},{x,2},{x,2}}.
    {call_only,3,{f,4}}.


{function, nthtail, 2, 7}.
  {label,6}.
    {line,[{location,"lists.erl",14}]}.
    {func_info,{atom,lists},{atom,nthtail},2}.
  {label,7}.
    {test,is_eq_exact,{f,8},[{x,0},{integer,0}]}.
    {move,{x,1},{x,0}}.
    return.
  {label,8}.
    {test,is_nonempty_list,{f,6},[{x,1}]}.
    {get_list,{x,1},{x,2},{x,3}}.
    {line,[{location,"lists.erl",17}]}.
    {gc_bif,'-',{f,0},4,[{x,0},{integer,1}],{x,0}}.
    {move,{x,3},{x,1}}.
    {call_only,2,{f,7}}.


{function, module_info, 0, 10}.
  {label,9}.
    {line,[]}.
    {func_info,{atom,lists},{atom,module_info},0}.
  {label,10}.
    {move,{atom,lists},{x,0}}.
    {line,[]}.
    {call_ext_only,1,{extfunc,erlang,get_module_info,1}}.


{function, module_info, 1, 12}.
  {label,11}.
    {line,[]}.
    {func_info,{atom,lists},{atom,module_info},1}.
  {label,12}.
    {move,{x,0},{x,1}}.
    {move,{atom,lists},{x,0}}.
    {line,[]}.
    {call_ext_only,2,{extfunc,erlang,get_module_info,2}}.
