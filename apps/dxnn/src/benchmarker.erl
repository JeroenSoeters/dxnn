-module(benchmarker).
-compile(export_all).
-include("records.hrl").
-define(TOTAL_RUNS, 100).
-define(MAX_ATTEMPTS, 5).
-define(EVAL_LIMIT, inf).
-define(FITNESS_TARGET, inf).

go(Morphology, HiddenLayerDensities) ->
	go(Morphology, HiddenLayerDensities, ?TOTAL_RUNS).
go(Morphology, HiddenLayerDensities, TotalRuns) ->
	go(Morphology, HiddenLayerDensities, TotalRuns, ?MAX_ATTEMPTS, ?EVAL_LIMIT, ?FITNESS_TARGET).
go(Morphology, HiddenLayerDensities, TotalRuns, MaxAttempts, EvalLimit, FitnessTarget) ->
	PId = spawn(benchmarker, loop, [Morphology, HiddenLayerDensities, TotalRuns, MaxAttempts, EvalLimit, FitnessTarget, [], [], [], []]),
	register(benchmarker, PId).
% The benchmarker is started through the go/2, go/3 or go/6 function. The parameters the benchmark uses can be specified through the macros, and then used by executing go2/ or go/3 for which the researcher simply specifies the Morphology and the HiddenLayerDensities. The go/2 and go/3 functions execute go/6 function with default parameters. The benchmarker can also be started through gp/6 using which the researcher can manually specify all the parameters. Before dropping into the main loop, go/6 registers the benchmarker process so that the trainer can send it the performance stats when it finishes.

loop(Morphology, _HiddenLayerDensities, 0, _MaxAttempts, _EvalLimit, _FitnessTarget, FitnessAcc, EvalsAcc, CyclesAcc, TimeAcc) ->
	io:format("Benchmark results for:~p~n", [Morphology]),
	io:format("Fitness:~n Max:~p~n Min:~p~n Avg:~p~n Std:~p~n", [lists:max(FitnessAcc), lists:min(FitnessAcc), functions:avg(FitnessAcc), functions:std(FitnessAcc)]),
  io:format("Evals:~n Max:~p~n Min:~p~n Avg:~p~n Std:~p~n", [lists:max(EvalsAcc), lists:min(EvalsAcc), functions:avg(EvalsAcc), functions:std(EvalsAcc)]),
	io:format("Cycles:~n Max:~p~n Min:~p~n Avg:~p~n Std:~p~n", [lists:max(CyclesAcc), lists:min(CyclesAcc), functions:avg(CyclesAcc), functions:std(CyclesAcc)]),
	io:format("Time:~n Max:~p~n Min:~p~n Avg:~p~n Std:~p~n", [lists:max(TimeAcc), lists:min(TimeAcc), functions:avg(TimeAcc), functions:std(TimeAcc)]);
loop(Morphology, HiddenLayerDensities, BenchmarkIndex, MaxAttempts, EvalLimit, FitnessTarget, FitnessAcc, EvalsAcc, CyclesAcc, TimeAcc) ->
	Trainer_PId = trainer:go(Morphology, HiddenLayerDensities, MaxAttempts, EvalLimit, FitnessTarget),
	receive
		{Trainer_PId, Fitness, Evals, Cycles, Time} ->
			loop(Morphology, HiddenLayerDensities, BenchmarkIndex - 1, MaxAttempts, EvalLimit, FitnessTarget, [Fitness|FitnessAcc], [Evals|EvalsAcc], [Cycles|CyclesAcc], [Time|TimeAcc]);
		terminate ->
			loop(Morphology, HiddenLayerDensities, 0, MaxAttempts, EvalLimit, FitnessTarget, FitnessAcc, EvalsAcc, CyclesAcc, TimeAcc)
	end.
% Once the benchmarker is started, it drops into its main loop. The main loop spawns the trainer and waits for it to finish optimizing the BB system. after which it sends to the benchmarker the performance based statistics. The benchmarker accumulates these performance statistics in lists, rerunning the trainer TotRuns number of times. Once the benchmarker has ran the trainer TotRun number of times, indicated to be so when the BenchmarkIndex reaches 0, it calculates the Max, Min, Avarage and Standard Deviation values for every statistic list it accumulated.
