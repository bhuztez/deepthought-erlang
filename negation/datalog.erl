-module(datalog).

-import(datalog_parser, [parse_clause/1, parse_query/1]).
-import(datalog_unify, [subst/2, walk/2, occurs/3, ext/3, unify/3, unify_list/3, reify/1, reify_list/3, reified/1]).

-export([test/0]).


load_db(S) ->
    [{P, {rule, A, [{X,Y} || {true,X,Y} <- Body], [{X,Y} || {false,X,Y} <- Body]}} || {clause, {P, A}, Body} <- parse_clause(S)].

answer_of(S) ->
    [ subst({var, I}, S) || I <- lists:seq(0, length(S)-1) ].

subst_of(Answer) ->
    lists:zip(
      [ {var, I} || I <- lists:seq(0, length(Answer) - 1)],
      Answer).



%% -type table() :: dict(table_key(), {Answers, Poss, Negs, Complete})

query_l4(Goals, DB) ->
    PosGoals = [{X,Y} || {true, X, Y} <- Goals],
    NegGoals = [{X,Y} || {false, X, Y} <- Goals],

    {S, C} =
	lists:foldl(
	  fun ({_P,A}, {S,C}) ->
		  {_,S1,C1} = reify_list(A, S, C),
		  {S1,C1}
	  end,
	  {[], 0},
	  PosGoals),

    R = [{V,K} || {K,V} <- S],
    A = [{var, I} || I <- lists:seq(0,C-1)],

    Table =
	cont_l4(
	  [{[{rule, [ subst(X,R) || X <- A], PosGoals, NegGoals}], root, A}],
	  dict:store({root, A}, {[], [], [], false}, dict:new()),
	  DB),
    {Answers, [], _Negs, _Complete} = dict:fetch({root, A}, Table),
    [ [{K, subst(V, subst_of(Answer))} || {K, V} <- S ] || Answer <- Answers ].


%% Frame: {{Goal, Subst}, S, R, Subgoals}
%% {Goal, Subst}, S, Subgoals, Table, DB


success_l4({Goal, Subst}, S, [], [], Proceed, Cont, Table, DB) ->
    S1 = [ {K, subst(V, S)} || {K, V} <- Subst],
    {Answers, Poss, Negs, Complete} = dict:fetch(Goal, Table),

    Answer = answer_of(S1),
    case lists:member(Answer, Answers) of
	true ->
	    {Proceed, Cont, Table};
	false ->
	    Table1 = dict:store(Goal, {[Answer|Answers], Poss, Negs, Complete}, Table),

	    {[ {Answer, Frame} || Frame <- Poss ] ++ Proceed,
	     Cont,
	     Table1}
    end;
success_l4({Goal, Subst}, S, [], NegGoals, Proceed, Cont, Table, DB) ->
    ReifiedGoals =
	lists:map(
	  fun ({P,A})->
		  A1 = walk(A, S),
		  {A2, _} = reify(A1),
		  {P, A2}
	  end,
	  NegGoals),

    Goals = lists:zip(ReifiedGoals, NegGoals),

    lists:foldl(
      fun (G, {P,C,Tab}) ->
	      neglookup_l4(G, {{Goal, Subst}, S, Goals}, P, C, Tab, DB)
      end,
      {Proceed, Cont, Table},
      ReifiedGoals);
success_l4({Goal, Subst}, S, [{P, A}|T], NegGoals, Proceed, Cont, Table, DB) ->
    A1 = walk(A, S),
    {A2, R} = reify(A1),
    poslookup_l4({P, A2}, {{Goal, Subst}, S, R, T, NegGoals}, Proceed, Cont, Table, DB).




neglookup_l4({P, A}, Frame, Proceed, Cont, Table, DB) ->
    case dict:find({P, A}, Table) of
	{ok, {Answers, Poss, Negs, Complete}} ->
	    Table1 =
		case Complete of
		    true ->
			Table;
		    false ->
			dict:store({P, A}, {Answers, Poss, [Frame|Negs], Complete}, Table)
		end,
	    {Proceed, Cont, Table1};
	error ->
	    Choices = proplists:get_all_values(P, DB),
	    {Proceed,
	     [{Choices, P, A}|Cont],
	     dict:store({P,A}, {[], [], [Frame], false}, Table)}
    end.

poslookup_l4({P, A}, Frame, Proceed, Cont, Table, DB) ->
    case dict:find({P, A}, Table) of
	{ok, {Answers, Poss, Negs, Complete}} ->
	    Table1 =
		case Complete of
		    true ->
			Table;
		    false ->
			dict:store({P, A}, {Answers, [Frame|Poss], Negs, Complete}, Table)
		end,
	    {[ {Answer, Frame} || Answer <- Answers] ++ Proceed,
	     Cont,
	     Table1};
	error ->
	    Choices = proplists:get_all_values(P, DB),
	    {Proceed,
	     [{Choices, P, A}|Cont],
	     dict:store({P,A}, {[], [Frame], [], false}, Table)}
    end.


trace_l4([], Found, _Table) ->
    Found;
trace_l4(Delta, Found, Table) ->
    Reachable =
	lists:append(
	  lists:map(
	    fun (Goal) ->
		    {_, Poss, _, _} = dict:fetch(Goal, Table),
		    [ G || {{G, _}, _, _, _, _} <- Poss ]
	    end,
	    Delta)),

    NewDelta =
	lists:foldl(
	  fun (Goal, New) ->
		  case lists:member(Goal, Found) or lists:member(Goal, New) of
		      true ->
			  New;
		      false ->
			  [Goal|New]
		  end
	  end,
	  [],
	  Reachable),

    trace_l4(NewDelta, NewDelta ++ Found, Table).


remove_neglookup(Entry, Frame, Table) ->
    {Answers, Poss, Negs, false} = dict:fetch(Entry, Table),
    dict:store(Entry, {Answers, Poss, lists:delete(Frame, Negs), false}, Table).


complete_l4({P,A}, Proceed, Cont, Table, DB) ->
    {Answers, _, Negs, false} = dict:fetch({P,A}, Table),
    Table1 = dict:store({P,A}, {Answers, [], [], true}, Table),

    Empty = length(Answers) == 0,

    lists:foldl(
      fun({{Goal, Subst}, S, Goals}=Frame, {Proc, C, Tab}) ->
    	      {ReifiedGoals, NegGoals} = lists:unzip(proplists:delete({P,A}, Goals)),

	      Tab1 =
		  lists:foldl(
		    fun (G, T) ->
			    remove_neglookup(G, Frame, T)
		    end,
		    Tab,
		    ReifiedGoals),

    	      case Empty of
    		  true ->
		      success_l4({Goal, Subst}, S, [], NegGoals, Proc, C, Tab1, DB);
    		  false ->
    		      {Proc, C, Tab1}
    	      end
      end,
      {Proceed, Cont, Table1},
      Negs).


cont_l4([], Table, DB) ->
    {Active, NegsList} =
	lists:unzip(
	  [{Goal,Negs} || {Goal, {_,_,Negs,false}} <- dict:to_list(Table)]),
    NegTargets = lists:usort([G || {{G,_},_,_} <- lists:append(NegsList)]),
    NegReachable = trace_l4(NegTargets, NegTargets, Table),
    Completed = [ G || G <- Active, not lists:member(G, NegReachable)],

    {Proceed1, Cont1, Table1} =
	lists:foldl(
	  fun (Goal, {Proceed, Cont, Tab}) ->
		  complete_l4(Goal, Proceed, Cont, Tab, DB)
	  end,
	  {[], [], Table},
	  Completed),

    case NegReachable of
    	[] ->
    	    Table1;
    	_ ->
    	    case Completed of
    		[] ->
    		    throw({error, negative_loop});
    		_ ->
    		    proceed_l4(Proceed1, Cont1, Table1, DB)
    	    end
    end;

cont_l4([{[], _, _}|Rest], Table, DB) ->
    cont_l4(Rest, Table, DB);
cont_l4([{[H|T], P, A}|Rest], Table, DB) ->
    {Proceed, Cont, Table1} = call_l4(H, P, A, [], [{T,P,A}|Rest], Table, DB),
    proceed_l4(Proceed, Cont, Table1, DB).


proceed_l4([], Cont, Table, DB) ->
    cont_l4(Cont, Table, DB);
proceed_l4([{Answer, {{Goal, Subst}, S, R, PosGoals, NegGoals}}|Rest], Cont, Table, DB) ->
    S1 = subst_of(Answer),
    R1 = [{K, subst(V, S1)} || {K,V} <- R ],
    {Proceed, Cont1, Table1} = success_l4({Goal, Subst}, R1++S, PosGoals, NegGoals, Rest, Cont, Table, DB),
    proceed_l4(Proceed, Cont1, Table1, DB).


call_l4({rule, Head, PosGoals, NegGoals}, P, A, Proceed, Cont, Table, DB) ->
    case unify_list(A, Head, []) of
	false ->
	    {Proceed, Cont, Table};
	S ->
	    S1 = [ {K,V} || {K,V} <- S, reified(K) ],
	    S2 = [ {K,V} || {K,V} <- S, not reified(K) ],
	    success_l4({{P, A}, S1}, S2, PosGoals, NegGoals, Proceed, Cont, Table, DB)
    end.



test(l0) ->
    DB = load_db("e(a,b). f(b,c). g(X,Y):e(X,Z),f(Z,Y)."),
    Q = parse_query("g(X,Y)."),
    [[{{var, 'Y'}, c}, {{var, 'X'}, a}]] = query_l4(Q, DB);
test(l1) ->
    DB = load_db("c(X,Y):e(X,Z),e(Z,Y). e(a,b). e(b,c)."),
    Q = parse_query("c(X,Y)."),
    [[{{var, 'Y'}, c}, {{var, 'X'}, a}]] = query_l4(Q, DB);
test(l2) ->
    DB = load_db("c(X,Y): c(X,Z), e(Z,Y). c(X,Y): e(X,Y). e(a,b). e(b,c)."),
    Q = parse_query("c(X,Y)."),
    [[{{var, 'Y'}, c}, {{var, 'X'}, b}],
     [{{var, 'Y'}, c}, {{var, 'X'}, a}],
     [{{var, 'Y'}, b}, {{var, 'X'}, a}]
    ] = query_l4(Q, DB);
test(l3) ->
    DB = load_db(
	   "start(a). arc(d,a). arc(e,a). arc(a,b). arc(a,c). arc(b,f). arc(c,f)."
	   "black(X): start(X)."
	   "black(X): white(Y), arc(Y,X)."
	   "white(X): black(Y), arc(Y,X)."
	   "black(X): white(Y), arc(X,Y)."
	   "white(X): black(Y), arc(X,Y)."),
    Q = parse_query("black(X)."),
    [[{{var, 'X'}, f}],
     [{{var, 'X'}, a}]] = query_l4(Q, DB);
test(l4) ->
    DB = load_db("p(a). p(b). q(X,Y): p(X), p(Y), not same(X,Y). same(X,X): p(X)."),
    Q = parse_query("q(X,Y)."),
    [[{{var,'Y'},b},{{var,'X'},a}],
     [{{var,'Y'},a},{{var,'X'},b}]] = query_l4(Q, DB).


test() ->
    test(l1),
    test(l2),
    test(l3),
    test(l4),
    ok.
