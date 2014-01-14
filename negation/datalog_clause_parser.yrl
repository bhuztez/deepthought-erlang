Nonterminals clauses clause nfas nfa fa term terms.
Terminals not atom var '(' ')' '.' ':' ','.
Rootsymbol clauses.


clauses -> clauses clause:
    '$1' ++ ['$2'].


clauses -> '$empty':
    [].


clause -> fa '.':
    datalog_parser:clause('$1', []).


clause -> fa ':' nfas '.':
    datalog_parser:clause('$1', '$3').


nfas -> nfas ',' nfa:
    '$1' ++ ['$3'].


nfas -> nfa:
    ['$1'].


nfa -> not fa:
    datalog_parser:nfa(false, '$2').


nfa -> fa:
    datalog_parser:nfa(true, '$1').


fa -> atom '(' terms ')':
    datalog_parser:fa(datalog_parser:term('$1'), '$3').


terms -> terms ',' term:
    '$1' ++ ['$3'].


terms -> term:
    ['$1'].


term -> atom:
    datalog_parser:term('$1').


term -> var:
    datalog_parser:term('$1').


Erlang code.
