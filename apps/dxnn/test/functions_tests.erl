-module(functions_tests).
-compile(export_all).
-include_lib("eunit/include/eunit.hrl").

saturation_test() ->
	?assertEqual(1000, functions:saturation(1050)),
	?assertEqual(800, functions:saturation(800)),
	?assertEqual(-1000, functions:saturation(-1050)).

saturation_with_spread_test() ->
	?assertEqual(3, functions:saturation(5, 3)),
	?assertEqual(3, functions:saturation(3, 5)),
	?assertEqual(-3, functions:saturation(-5, 3)).

scale_test() ->
	?assertEqual([0, 0], functions:scale([2, 3], 5, 5)),
	?assertEqual([1.0, 5.0], functions:scale([4, 8], 2, 4)).

scale_dzone_test() ->
	?assertEqual(1.0, functions:scale_dzone(4, 2, 4, 0, 1)),
	?assertEqual(0, functions:scale_dzone(4, 2, 4, 0, 6)).

sgn_test() ->
	?assertEqual(1, functions:sgn(2)),
	?assertEqual(-1, functions:sgn(-2)),
	?assertEqual(0, functions:sgn(0)).

avg_test() ->
	?assertEqual(3.0, functions:avg([1,2,3,4,5])),
	?assertEqual(1.0, functions:avg([1,1,1])).

std_test() ->
	?assertEqual(true, abs(1.41421 - functions:std([1,2,3,4,5])) < 0.0001),
	?assertEqual(0.0, functions:std([5,5,5])).

