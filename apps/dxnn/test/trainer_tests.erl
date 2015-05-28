-module(trainer_tests).
-include_lib("eunit/include/eunit.hrl").

xor_simulation_test_ignore() ->
	register(benchmarker, self()),
	trainer:go(xor_mimic, [2], 10, inf, inf),
	receive
		{_Pid, BestFitness, _TotEvals, _TotCycles, _TotTime} ->
			io:format("^^^^ Best Fitness:~p~n ", [BestFitness]),
			?assert((BestFitness > 150) and (BestFitness < 240))
	after 
		2000 ->
			?assert(false)
	end.
		
