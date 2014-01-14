-module(datalog_parser).

-export([term/1, nfa/2, fa/2, clause/2, parse_query/1, parse_clause/1, make_atom/2]).


make_atom(TokenLine, TokenChars) ->
    case TokenChars of
        "not" ->
	    {token, {'not', TokenLine}};
	_ ->
	    {token, {atom, TokenLine, list_to_atom(TokenChars)}}
    end.


term({atom, _, Atom}) ->
    Atom;
term({var, _, Var}) ->
    {var, Var}.


pred_name(F, A) ->
    list_to_atom(
      atom_to_list(F) ++ "/" ++ integer_to_list(length(A))).


nfa(N, {F, A}) ->
    {N, F, A}.


fa(F, A) ->
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
