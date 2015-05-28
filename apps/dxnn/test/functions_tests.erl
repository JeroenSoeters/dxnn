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

bin_test() ->
	?assertEqual(0, functions:bin(-1)),
	?assertEqual(1, functions:bin(1)).

multiquadratic_test() ->
	?assertEqual(3.0016662039607267, functions:multiquadratic(3)).

linear_test() ->
	?assertEqual(1, functions:linear(1)),
	?assertEqual(-99, functions:linear(-99)).

quadratic_test() ->
	?assertEqual(4, functions:quadratic(2)),
	?assertEqual(-4, functions:quadratic(-2)).

gaussian_test() ->
	?assertEqual(0.36787944096289676, functions:gaussian(-1)),
	?assertEqual(3.7200757651350987e-44, functions:gaussian(150)).

sqrt_test() ->
	?assertEqual(2.0, functions:sqrt(4)),
	?assertEqual(-2.0, functions:sqrt(-4)).

log_test() ->
	?assertEqual(0, functions:log(0)),
	?assertEqual(-0.6931471805599453, functions:log(-2)),
	?assertEqual(0.6931471805599453, functions:log(2)).

sigmoid_test() ->
	?assertEqual(-0.9999092042631098, functions:sigmoid(-15)),
	?assertEqual(0.0, functions:sigmoid(0)),
	?assertEqual(0.9051482537985267, functions:sigmoid(3)),
	?assertEqual(0.9999092042631097, functions:sigmoid(25)).

sigmoid1_test() ->
	?assertEqual(0.5, functions:sigmoid1(1)).

avg_test() ->
	?assertEqual(3.0, functions:avg([1,2,3,4,5])),
	?assertEqual(1.0, functions:avg([1,1,1])).

std_test() ->
	?assertEqual(true, abs(1.41421 - functions:std([1,2,3,4,5])) < 0.0001),
	?assertEqual(0.0, functions:std([5,5,5])).

