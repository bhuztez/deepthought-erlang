datalog.beam: datalog_parser.beam datalog_unify.beam
datalog_parser.beam: datalog_lexer.beam datalog_clause_parser.beam datalog_query_parser.beam

%.erl: %.xrl
	erl -run leex file "$<" -run init stop -noshell

%.erl: %.yrl
	erl -run yecc file "$<" -run init stop -noshell

%.beam: %.erl
	erlc "$<"

clean:
	rm -f *.beam datalog_lexer.erl datalog_clause_parser.erl datalog_query_parser.erl
