Nonterminals clauses clause fas fa term terms.
Terminals atom var '(' ')' '.' ':' ','.
Rootsymbol clauses.


clauses -> clauses clause:
    '$1' ++ ['$2'].


clauses -> '$empty':
    [].


clause -> fa '.':
    datalog_parser:clause('$1', []).


clause -> fa ':' fas '.':
    datalog_parser:clause('$1', '$3').


fas -> fas ',' fa:
    '$1' ++ ['$3'].


fas -> fa:
    ['$1'].


fa -> atom '(' terms ')':
    datalog_parser:goal(datalog_parser:term('$1'), '$3').


terms -> terms ',' term:
    '$1' ++ ['$3'].


terms -> term:
    ['$1'].


term -> atom:
    datalog_parser:term('$1').


term -> var:
    datalog_parser:term('$1').


Erlang code.
