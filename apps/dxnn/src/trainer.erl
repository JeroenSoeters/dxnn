-module(trainer).
-compile(export_all).
-include("records.hrl").
-define(MAX_ATTEMPTS, 5).
-define(EVAL_LIMIT, inf).
-define(FITNESS_TARGET, inf).

go(Morphology, HiddenLayerDensities) ->
	go(Morphology, HiddenLayerDensities, ?MAX_ATTEMPTS, ?EVAL_LIMIT, ?FITNESS_TARGET).
go(Morphology, HiddenLayerDensities, MaxAttempts, EvalLimit, FitnessTarget) ->
	Pid = spawn(trainer, loop, [Morphology, HiddenLayerDensities, FitnessTarget, {1, MaxAttempts}, {0, EvalLimit}, {0, best}, experimental, 0, 0]),
	register(trainer, Pid),
	Pid.
%The function go/2 is executed to start the training process based on the Morphology and HiddenLayerDensities specified. The go/2 function uses a defailt values for the Max_Attempts, Eval_Limit, and Fitness_Target parameters, which makes the training purely on the Max_Attempts value. Function go/5 allows for all the stopping conditions to be specified.

loop(Morphology, _HiddenLayerDensities, FT, {AttemptAcc, MA}, {EvalAcc, EL}, {BestFitness, BestG}, _ExpG, CAcc, TAcc) when (AttemptAcc>=MA) or (EvalAcc>=EL) or (BestFitness>=FT) ->
	genotype:print(BestG),
	io:format("Morphology:~p Best Fitness:~p EvalAcc:~p~n", [Morphology, BestFitness, EvalAcc]),
	unregister(trainer),
	case whereis(benchmarker) of
		undefined ->
			ok;
		Pid ->
			io:format("benchmarker found!"),
			Pid ! {self(), BestFitness, EvalAcc, CAcc, TAcc}
	end;
loop(Morphology, HiddenLayerDensities, FT, {AttemptAcc, MA}, {EvalAcc, EvalLimit}, {BestFitness, BestG}, ExpG, CAcc, TAcc) ->
	genotype:construct(ExpG, Morphology, HiddenLayerDensities),
	Agent_Pid = exoself:map(ExpG),
	receive
		{Agent_Pid, Fitness, Evals, Cycles, Time} ->
			U_EvalAcc = EvalAcc+Evals,
			U_CAcc = CAcc+Cycles,
			U_TAcc = TAcc+Time,
			case Fitness > BestFitness of
				true ->
					file:rename(ExpG, BestG), 
					?MODULE:loop(Morphology, HiddenLayerDensities, FT, {1, MA}, {U_EvalAcc, EvalLimit}, {Fitness, BestG}, ExpG, U_CAcc, U_TAcc);
				false ->
					?MODULE:loop(Morphology, HiddenLayerDensities, FT, {AttemptAcc+1, MA}, {U_EvalAcc, EvalLimit}, {BestFitness, BestG}, ExpG, U_CAcc, U_TAcc)
			end;
		terminate ->
			io:format("Trainer terminated:~n"),
			genotype:print(BestG),
			io:format("Morphology:~p Best Fitness:~p EvalAcc:~p~n", [Morphology, BestFitness, EvalAcc])
	end.
%loop/7 generates new NNs and trains them until a stopping condition is reached. Once any one of the stopping conditions is reached, the trainier prints to the screen the genotype, the morphological name of the organism being trained, the best fitness score achieved and the number of evaluations taken to find this fitness score.
