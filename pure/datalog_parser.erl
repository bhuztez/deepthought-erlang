-module(datalog_parser).

-export([term/1, goal/2, clause/2, parse_query/1, parse_clause/1]).

term({atom, _, Atom}) ->
    Atom;
term({var, _, Var}) ->
    {var, Var}.


pred_name(F, A) ->
    list_to_atom(
      atom_to_list(F) ++ "/" ++ integer_to_list(length(A))).


goal(F, A) ->
    {pred_name(F, A), A}.


clause({F, A}, Body) ->
    {clause, {F, A}, Body}.


parse_query(S) ->
    {ok, Tokens, _} = datalog_lexer:string(S),
    {ok, Query} = datalog_query_parser:parse(Tokens),
    Query.

parse_clause(S) ->
    {ok, Tokens, _} = datalog_lexer:string(S),
    {ok, Clauses} = datalog_clause_parser:parse(Tokens),
    Clauses.
