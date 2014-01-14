-module(datalog_unify).

-export([subst/2, walk/2, occurs/3, ext/3, unify/3, unify_list/3, reify/1, reify_list/3, reified/1]).
-export([test/0]).


subst({var, _}=V, S) ->
    case proplists:lookup(V, S) of
	{V, Rhs} ->
	    subst(Rhs, S);
	none ->
	    V
    end;
subst(V, _) ->
    V.


walk(Vs, S) ->
    [subst(V, S) || V <- Vs].


occurs({var, _}=X, V, S) ->
    case subst(V, S) of
	{var, _} = Y -> X =:= Y;
	_ -> false
    end.


ext(X, V, S) ->
    case occurs(X, V, S) of
	true -> false;
	false -> [{X,V}|S]
    end.


unify(V1, V2, S) ->
    case {subst(V1, S), subst(V2, S)} of
	{{var, X}, {var, X}} -> S;
	{{var, _}=X1, X2} -> ext(X1, X2, S);
	{X1, {var, _}=X2} -> ext(X2, X1, S);
	{X, X} -> S;
	{_, _} -> false
    end.


unify_list([], [], S) ->
    S;
unify_list([H1|T1], [H2|T2], S) ->
    case unify(H1, H2, S) of
	false ->
	    false;
	S1 ->
	    unify_list(T1, T2, S1)
    end.


reify(V, S, C) ->
    case subst(V, S) of
	{var, A}
	  when is_atom(A) ->
	    G = {var, C},
	    {G, ext(V, G, S), C+1};
	X ->
	    {X, S, C}
    end.


reify_list([], S, C) ->
    {[], S, C};
reify_list([H|T], S, C) ->
    {H1, S1, C1} = reify(H, S, C),
    {T1, S2, C2} = reify_list(T, S1, C1),
    {[H1|T1], S2, C2}.

reify(V) ->
    {V1, S, _} = reify_list(V, [], 0),
    {V1, S}.


reified({var, N})
  when is_integer(N)->
    true;
reified(_) ->
    false.


test(unify) ->
    [{{var,2},{var,'Y'}},{{var,1},{var,'X'}}] = unify_list([{var, 1}, {var, 2}], [{var, 'X'}, {var, 'Y'}], []),
    [{{var,'X'},{var,'Y'}},{{var,1},{var,'X'}}] = unify_list([{var, 1}, {var, 1}], [{var, 'X'}, {var, 'Y'}], []),
    [{{var,2},{var,'X'}},{{var,1},{var,'X'}}] = unify_list([{var, 1}, {var, 2}], [{var, 'X'}, {var, 'X'}], []),
    [{{var,'X'},a},{{var,1},{var,'X'}}] = unify_list([{var, 1}, a], [{var, 'X'}, {var, 'X'}], []),
    [{{var,'X'},a},{{var,1},{var,'X'}}] = unify_list([{var, 1}, {var, 1}], [{var, 'X'}, a], []).

test() ->
    test(unify),
    ok.
