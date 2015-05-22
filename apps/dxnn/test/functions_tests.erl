-module(functions_tests).
-compile(export_all).
-include_lib("eunit/include/eunit.hrl").

avg_test() ->
	?assertEqual(3.0, functions:avg([1,2,3,4,5])),
	?assertEqual(1.0, functions:avg([1,1,1])).

std_test() ->
	?assertEqual(true, abs(1.41421 - functions:std([1,2,3,4,5])) < 0.0001),
	?assertEqual(0.0, functions:std([5,5,5])).

