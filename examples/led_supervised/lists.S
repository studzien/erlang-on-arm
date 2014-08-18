{module, lists}.  %% version = 0

{exports, [{delete,2},{module_info,0},{module_info,1},{nth,2}]}.

{attributes, []}.

{labels, 14}.


{function, nth, 2, 2}.
  {label,1}.
    {line,[{location,"lists.erl",6}]}.
    {func_info,{atom,lists},{atom,nth},2}.
  {label,2}.
    {test,is_eq_exact,{f,3},[{x,0},{integer,1}]}.
    {test,is_nonempty_list,{f,1},[{x,1}]}.
    {get_list,{x,1},{x,0},{x,3}}.
    return.
  {label,3}.
    {test,is_nonempty_list,{f,1},[{x,1}]}.
    {get_list,{x,1},{x,2},{x,3}}.
    {line,[{location,"lists.erl",7}]}.
    {gc_bif,'-',{f,0},4,[{x,0},{integer,1}],{x,0}}.
    {move,{x,3},{x,1}}.
    {call_only,2,{f,2}}.


{function, delete, 2, 5}.
  {label,4}.
    {line,[{location,"lists.erl",9}]}.
    {func_info,{atom,lists},{atom,delete},2}.
  {label,5}.
    {move,nil,{x,2}}.
    {call_only,3,{f,7}}.


{function, delete, 3, 7}.
  {label,6}.
    {line,[{location,"lists.erl",12}]}.
    {func_info,{atom,lists},{atom,delete},3}.
  {label,7}.
    {test,is_nonempty_list,{f,9},[{x,1}]}.
    {get_list,{x,1},{x,3},{x,4}}.
    {test,is_eq_exact,{f,8},[{x,3},{x,0}]}.
    {allocate,1,5}.
    {move,{x,2},{x,0}}.
    {move,{x,4},{y,0}}.
    {line,[{location,"lists.erl",15}]}.
    {call_ext,1,{extfunc,lists,reverse,1}}.
    {move,{y,0},{x,1}}.
    {line,[{location,"lists.erl",15}]}.
    {call_ext_last,2,{extfunc,erlang,'++',2},1}.
  {label,8}.
    {move,{x,4},{x,1}}.
    {call_only,3,{f,7}}.
  {label,9}.
    {test,is_nil,{f,6},[{x,1}]}.
    {move,{x,2},{x,0}}.
    {line,[{location,"lists.erl",13}]}.
    {call_ext_only,1,{extfunc,lists,reverse,1}}.


{function, module_info, 0, 11}.
  {label,10}.
    {line,[]}.
    {func_info,{atom,lists},{atom,module_info},0}.
  {label,11}.
    {move,{atom,lists},{x,0}}.
    {line,[]}.
    {call_ext_only,1,{extfunc,erlang,get_module_info,1}}.


{function, module_info, 1, 13}.
  {label,12}.
    {line,[]}.
    {func_info,{atom,lists},{atom,module_info},1}.
  {label,13}.
    {move,{x,0},{x,1}}.
    {move,{atom,lists},{x,0}}.
    {line,[]}.
    {call_ext_only,2,{extfunc,erlang,get_module_info,2}}.
