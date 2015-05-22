-module(benchmarker_test).
-include_lib("eunit/include/eunit.hrl").
-compile(export_all).

setup_benchmarker() ->
	benchmarker:go(test_morphology, [2,3]).

start_benchmarker_test() ->
	{setup,
	 fun benchmarker:go/0,
	 fun benchmarker_is_registered/0}.

benchmarker_is_registered() ->
	?assertNot(undefined == whereis(benchmarker)).

fuzzy_match(A,B,L) ->
	<<AT:L/binary, _/binary>> = <<A/float>>,
	<<BT:L/binary, _/binary>> = <<B/float>>,
	io:format("matching ~p with ~p", [AT,BT]),
	AT == BT.

