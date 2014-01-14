%% term:
%%     atom | Var
-type datalog_term() :: atom()
		      | {'var', atom()}.
%% goal:
%%     f(atom, Var)
-type datalog_goal() :: {atom(), [datalog_term()]}.

%% clause:
%%     f(atom, Var):
%%         g(Var).
-type datalog_clause() :: {'clause', datalog_goal(), [datalog_goal()]}.
