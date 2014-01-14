-module(datalog).

-import(datalog_parser, [parse_clause/1, parse_query/1]).
-import(datalog_unify, [subst/2, walk/2, occurs/3, ext/3, unify/3, unify_list/3, reify/1, reify_list/3, reified/1]).

-export([test/0]).


load_db(S) ->
    [{P, {rule, A, Body}} || {clause, {P, A}, Body} <- parse_clause(S)].


answer_of(S) ->
    [ subst({var, I}, S) || I <- lists:seq(0, length(S)-1) ].

subst_of(Answer) ->
    lists:zip(
      [ {var, I} || I <- lists:seq(0, length(Answer) - 1)],
      Answer).


%% %% -type table() :: dict(table_key(), {Answers, Lookups})

%% query_l2(Goals, DB) ->
%%     {S, C} =
%% 	lists:foldr(
%% 	  fun ({_P,A}, {S,C}) ->
%% 		  {_,S1,C1} = reify_list(A, S, C),
%% 		  {S1,C1}
%% 	  end,
%% 	  {[], 0},
%% 	  Goals),

%%     R = [{V,K} || {K,V} <- S],
%%     A = [{var, I} || I <- lists:seq(0,C-1)],

%%     Table =
%% 	cont_l2(
%% 	  [{[{rule, [ subst(X,R) || X <- A], Goals}], root, A}],
%% 	  dict:store({root, A}, {[], []}, dict:new()),
%% 	  DB),
%%     {Answers, []} = dict:fetch({root, A}, Table),
%%     [ [{K, subst(V, subst_of(Answer))} || {K, V} <- S ] || Answer <- Answers ].


%% %% Frame: {{Goal, Subst}, S, R, Subgoals}
%% %% {Goal, Subst}, S, Subgoals, Table, DB


%% success_l2({Goal, Subst}, S, [], Proceed, Cont, Table, DB) ->
%%     S1 = [ {K, subst(V, S)} || {K, V} <- Subst],
%%     {Answers, Lookups} = dict:fetch(Goal, Table),
%%     Answer = answer_of(S1),
%%     case lists:member(Answer, Answers) of
%% 	true ->
%% 	    {Proceed, Cont, Table};
%% 	false ->
%% 	    Table1 = dict:store(Goal, {[Answer|Answers], Lookups}, Table),

%% 	    {[ {Answer, Frame} || Frame <- Lookups ] ++ Proceed,
%% 	     Cont,
%% 	     Table1}
%%     end;
%% success_l2({Goal, Subst}, S, [{P, A}|T], Proceed, Cont, Table, DB) ->
%%     A1 = walk(A, S),
%%     {A2, R} = reify(A1),
%%     lookup_l2({P, A2}, {{Goal, Subst}, S, R, T}, Proceed, Cont, Table, DB).


%% lookup_l2({P, A}, Frame, Proceed, Cont, Table, DB) ->
%%     case dict:find({P, A}, Table) of
%% 	{ok, {Answers, Lookups}} ->
%% 	    Table1 = dict:store({P, A}, {Answers, [Frame|Lookups]}, Table),
%% 	    {[ {Answer, Frame} || Answer <- Answers] ++ Proceed,
%% 	     Cont,
%% 	     Table1};
%% 	error ->
%% 	    Choices = proplists:get_all_values(P, DB),
%% 	    {Proceed,
%% 	     [{Choices, P, A}|Cont],
%% 	     dict:store({P,A}, {[], [Frame]}, Table)}
%%     end.


%% cont_l2([], Table, DB) ->
%%     Table;
%% cont_l2([{[], _, _}|Rest], Table, DB) ->
%%     cont_l2(Rest, Table, DB);
%% cont_l2([{[H|T], P, A}|Rest], Table, DB) ->
%%     {Proceed, Cont, Table1} = call_l2(H, P, A, [], [{T,P,A}|Rest], Table, DB),
%%     {Cont1, Table2} = proceed_l2(Proceed, Cont, Table1, DB),
%%     cont_l2(Cont1, Table2, DB).


%% proceed_l2([], Cont, Table, DB) ->
%%     {Cont, Table};
%% proceed_l2([{Answer, {{Goal, Subst}, S, R, Subgoals}}|Rest], Cont, Table, DB) ->
%%     S1 = subst_of(Answer),
%%     R1 = [{K, subst(V, S1)} || {K,V} <- R ],
%%     {Proceed, Cont1, Table1} = success_l2({Goal, Subst}, R1++S, Subgoals, Rest, Cont, Table, DB),
%%     proceed_l2(Proceed, Cont1, Table1, DB).


%% call_l2({rule, Head, Body}, P, A, Proceed, Cont, Table, DB) ->
%%     case unify_list(A, Head, []) of
%% 	false ->
%% 	    {[], Cont, Table};
%% 	S ->
%% 	    S1 = [ {K,V} || {K,V} <- S, reified(K) ],
%% 	    S2 = [ {K,V} || {K,V} <- S, not reified(K) ],
%% 	    success_l2({{P, A}, S1}, S2, Body, Proceed, Cont, Table, DB)
%%     end.





%% -type table() :: dict(table_key(), {Answers, Lookups, Complete})

query_l3(Goals, DB) ->
    {S, C} =
	lists:foldr(
	  fun ({_P,A}, {S,C}) ->
		  {_,S1,C1} = reify_list(A, S, C),
		  {S1,C1}
	  end,
	  {[], 0},
	  Goals),

    R = [{V,K} || {K,V} <- S],
    A = [{var, I} || I <- lists:seq(0,C-1)],

    Table =
	cont_l3(
	  [{[{rule, [ subst(X,R) || X <- A], Goals}], root, A}],
	  dict:store({root, A}, {[], [], false}, dict:new()),
	  DB),
    {Answers, [], _Complete} = dict:fetch({root, A}, Table),
    [ [{K, subst(V, subst_of(Answer))} || {K, V} <- S ] || Answer <- Answers ].


%% Frame: {{Goal, Subst}, S, R, Subgoals}
%% {Goal, Subst}, S, Subgoals, Table, DB


success_l3({Goal, Subst}, S, [], Proceed, Cont, Table, DB) ->
    S1 = [ {K, subst(V, S)} || {K, V} <- Subst],
    {Answers, Lookups, Complete} = dict:fetch(Goal, Table),
    Answer = answer_of(S1),
    case lists:member(Answer, Answers) of
	true ->
	    {Proceed, Cont, Table};
	false ->
	    Table1 = dict:store(Goal, {[Answer|Answers], Lookups, Complete}, Table),

	    {[ {Answer, Frame} || Frame <- Lookups ] ++ Proceed,
	     Cont,
	     Table1}
    end;
success_l3({Goal, Subst}, S, [{P, A}|T], Proceed, Cont, Table, DB) ->
    A1 = walk(A, S),
    {A2, R} = reify(A1),
    lookup_l3({P, A2}, {{Goal, Subst}, S, R, T}, Proceed, Cont, Table, DB).


lookup_l3({P, A}, Frame, Proceed, Cont, Table, DB) ->
    case dict:find({P, A}, Table) of
	{ok, {Answers, Lookups, Complete}} ->
	    Table1 =
		case Complete of
		    true ->
			Table;
		    false ->
			dict:store({P, A}, {Answers, [Frame|Lookups], Complete}, Table)
		end,
	    {[ {Answer, Frame} || Answer <- Answers] ++ Proceed,
	     Cont,
	     Table1};
	error ->
	    Choices = proplists:get_all_values(P, DB),
	    {Proceed,
	     [{Choices, P, A}|Cont],
	     dict:store({P,A}, {[], [Frame], false}, Table)}
    end.


trace_l3([], Found, _Table) ->
    Found;
trace_l3(Delta, Found, Table) ->
    Reachable =
	lists:append(
	  lists:map(
	    fun (Goal) ->
		    {_, Lookups, _} = dict:fetch(Goal, Table),
		    [ G || {{G, _}, _, _, _} <- Lookups]
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

    trace_l3(NewDelta, NewDelta ++ Found, Table).


trace_l3(Root, Table) ->
    trace_l3(Root, Root, Table).


mark_complete_l3(Cont, Table) ->
    Root = [ {P,A} || {_, P, A} <- Cont],
    Working = [ Goal || {Goal, {_, _, false}} <- dict:to_list(Table)],
    Live = trace_l3(Root, Table),
    Dead = [ Goal || Goal <- Working, not lists:member(Goal, Live) ],

    lists:foldl(
      fun (Goal, Tab) ->
	      {Answer, Lookups, false} = dict:fetch(Goal, Tab),
	      dict:store(Goal, {Answer, [], true}, Tab)
      end,
      Table,
      Dead).


cont_l3([], Table, DB) ->
    Table;
cont_l3([{[], P, A}|Rest], Table, DB) ->
    Table1 = mark_complete_l3(Rest, Table),
    cont_l3(Rest, Table1, DB);
cont_l3([{[H|T], P, A}|Rest], Table, DB) ->
    {Proceed, Cont, Table1} = call_l3(H, P, A, [], [{T,P,A}|Rest], Table, DB),
    {Cont1, Table2} = proceed_l3(Proceed, Cont, Table1, DB),
    cont_l3(Cont1, Table2, DB).


proceed_l3([], Cont, Table, DB) ->
    {Cont, Table};
proceed_l3([{Answer, {{Goal, Subst}, S, R, Subgoals}}|Rest], Cont, Table, DB) ->
    S1 = subst_of(Answer),
    R1 = [{K, subst(V, S1)} || {K,V} <- R ],
    {Proceed, Cont1, Table1} = success_l3({Goal, Subst}, R1++S, Subgoals, Rest, Cont, Table, DB),
    proceed_l3(Proceed, Cont1, Table1, DB).


call_l3({rule, Head, Body}, P, A, Proceed, Cont, Table, DB) ->
    case unify_list(A, Head, []) of
	false ->
	    {[], Cont, Table};
	S ->
	    S1 = [ {K,V} || {K,V} <- S, reified(K) ],
	    S2 = [ {K,V} || {K,V} <- S, not reified(K) ],
	    success_l3({{P, A}, S1}, S2, Body, Proceed, Cont, Table, DB)
    end.





test(l0) ->
    DB = load_db("e(a,b). f(b,c). g(X,Y):e(X,Z),f(Z,Y)."),
    Q = parse_query("g(X,Y)."),
    [[{{var, 'Y'}, c}, {{var, 'X'}, a}]] = query_l3(Q, DB);
test(l1) ->
    DB = load_db("c(X,Y):e(X,Z),e(Z,Y). e(a,b). e(b,c)."),
    Q = parse_query("c(X,Y)."),
    [[{{var, 'Y'}, c}, {{var, 'X'}, a}]] = query_l3(Q, DB);
test(l2) ->
    DB = load_db("c(X,Y): c(X,Z), e(Z,Y). c(X,Y): e(X,Y). e(a,b). e(b,c)."),
    Q = parse_query("c(X,Y)."),
    [[{{var, 'Y'}, c}, {{var, 'X'}, b}],
     [{{var, 'Y'}, c}, {{var, 'X'}, a}],
     [{{var, 'Y'}, b}, {{var, 'X'}, a}]
    ] = query_l3(Q, DB);
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
     [{{var, 'X'}, a}]] = query_l3(Q, DB).


test() ->
    test(l0),
    test(l1),
    test(l2),
    test(l3).
