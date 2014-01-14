Definitions.

Atom = [a-z][0-9a-zA-Z_]*
Var  = [A-Z_][0-9a-zA-Z_]*
WS   = [\000-\s]+
Sym  = [\[\](),:\.]

Rules.

{Atom} : datalog_parser:make_atom(TokenLine, TokenChars).
{Var}  : {token, {var, TokenLine, list_to_atom(TokenChars)}}.
{Sym}  : {token, {list_to_atom(TokenChars), TokenLine}}.
{WS}   : skip_token.

Erlang code.