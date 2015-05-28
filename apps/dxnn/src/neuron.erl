-module(neuron).
-compile(export_all).
-include("records.hrl").
-define(DELTA_MULTIPLIER, math:pi()*2).
-define(SAT_LIMIT, math:pi()*2).
-define(RO_SIGNAL, 0).

gen(ExoselfPid, Node) ->
	spawn(Node, ?MODULE, prep, [ExoselfPid]).

prep(ExoselfPid) ->
	random:seed(now()),
	receive
		{ExoselfPid, {Id, CortexPid, AF, InputIdsPlusWeights, OutputPids, RecursiveOutputPids}} ->
			fanout(RecursiveOutputPids, {self(), forward, [?RO_SIGNAL]}),
			loop(Id, ExoselfPid, CortexPid, AF, {InputIdsPlusWeights, InputIdsPlusWeights}, OutputPids, RecursiveOutputPids, 0)
	end.
%When gen/2 is executed it spawns the neuron element and immediately begins to wait for its initial state message.

loop(Id, ExoselfPid, CortexPid, AF, {[{Input_PId, Weights}|InputIdsPlusWeights], MInputIdsPlusWeights}, OutputPids, RecursiveOutputPids, Acc) ->
	receive
		{Input_PId, forward, Input} ->
			Result = dot(Input, Weights, 0),
			loop(Id, ExoselfPid, CortexPid, AF, {InputIdsPlusWeights, MInputIdsPlusWeights}, OutputPids, RecursiveOutputPids, Result+Acc);
		{ExoselfPid, weight_backup} ->
      put(weights, MInputIdsPlusWeights),
		  loop(Id, ExoselfPid, CortexPid, AF, {[{Input_PId, Weights}|InputIdsPlusWeights], MInputIdsPlusWeights}, OutputPids, RecursiveOutputPids, Acc);
		{ExoselfPid, weight_restore} ->
			RInputIdsPlusWeights = get(weights),
			loop(Id, ExoselfPid, CortexPid, AF, {RInputIdsPlusWeights, RInputIdsPlusWeights}, OutputPids, RecursiveOutputPids, Acc);
		{ExoselfPid, weight_perturb} ->
			PInputIdsPlusWeights = perturb_PIdPs(MInputIdsPlusWeights),
			loop(Id, ExoselfPid, CortexPid, AF, {PInputIdsPlusWeights, PInputIdsPlusWeights}, OutputPids, RecursiveOutputPids, Acc);
		{ExoselfPid, reset_prep} ->
			neuron:flush_buffer(),
			ExoselfPid ! {self(), ready},
			receive
				{ExoselfPid, reset} ->
					fanout(RecursiveOutputPids, {self(), forward, [?RO_SIGNAL]})
			end,
			loop(Id, ExoselfPid, CortexPid, AF, {MInputIdsPlusWeights, MInputIdsPlusWeights}, OutputPids, RecursiveOutputPids, 0);
		{ExoselfPid, get_backup} ->
			ExoselfPid ! {self(), Id, MInputIdsPlusWeights},
			loop(Id, ExoselfPid, CortexPid, AF, {[{Input_PId, Weights}|InputIdsPlusWeights], MInputIdsPlusWeights}, OutputPids, RecursiveOutputPids, Acc);
		{ExoselfPid, terminate} ->
			ok
	end;
loop(Id, ExoselfPid, CortexPid, AF, {[Bias], MInputIdsPlusWeights}, OutputPids, RecursiveOutputPids, Acc) ->
	Output = functions:AF(Acc+Bias),
	[Output_PId ! {self(), forward, [Output]} || Output_PId <- OutputPids],
	loop(Id, ExoselfPid, CortexPid, AF, {MInputIdsPlusWeights, MInputIdsPlusWeights}, OutputPids, RecursiveOutputPids, 0);
loop(Id, ExoselfPid, CortexPid, AF, {[], MInputIdsPlusWeights}, OutputPids, RecursiveOutputPids, Acc) ->
	Output = neuron:AF(Acc),
	[Output_PId ! {self(), forward, [Output]} || Output_PId <- OutputPids],
	loop(Id, ExoselfPid, CortexPid, AF, {MInputIdsPlusWeights, MInputIdsPlusWeights}, OutputPids, RecursiveOutputPids, 0).

dot([I|Input],[W|Weights],Acc) ->
	dot(Input,Weights,I*W+Acc);
dot([],[],Acc) ->
	Acc.
%The neuron process waits for vector signals form all the processes that it's connected from, taking the dot product of the input and weight vectors, and then adding it to the accumulator. Once all the signals form Input_PIds are received, the accumulator contains the dot product to which the neuron then adds the bias and executes the actuvation function on. After fanning out the output signal, the neuron again returns to waiting for incoming signals. When the neuron receives the {CortexPid,get_backup} message, it forwards to the cortex its full MInputIdsPlusWeights list and its Id. Once the training/learning algorithm is added to the system, the MInputPs would contain a full set of the most recent and updated version of the weights.

fanout([Pid|Pids], Message) ->
	Pid ! Message,
	fanout(Pids, Message);
fanout([], _Message) ->
	true.

flush_buffer() ->
	receive
		_ ->
			flush_buffer()
	after 0 ->
		done
	end.

perturb_PIdPs(InputIdsPlusWeights) ->
	Tot_Weights = lists:sum([length(Weights) || {_InputPId, Weights} <- InputIdsPlusWeights]),
	MP = 1/math:sqrt(Tot_Weights),
	perturb_PIdPs(MP, InputIdsPlusWeights, []).
perturb_PIdPs(MP, [{Input_PId, Weights}|InputIdsPlusWeights], Acc) ->
	U_Weights = perturb_weights(MP, Weights, []),
	perturb_PIdPs(MP, InputIdsPlusWeights, [{Input_PId, U_Weights}|Acc]);
perturb_PIdPs(MP, [Bias], Acc) ->
	U_Bias = case random:uniform() < MP of
		true -> sat((random:uniform()-0.5) * ?DELTA_MULTIPLIER + Bias, -?SAT_LIMIT, ?SAT_LIMIT);
		false -> Bias
	end,
	lists:reverse([U_Bias|Acc]);
perturb_PIdPs(_MP, [], Acc) ->
	lists:reverse(Acc).
%perturb_PIdPs/1 first calculates the probability that a weight will be perturbed, the probability being the inverse quare root of the total number of weights in the neuron. The function then drops into perturb_PIdPs/3 which executes perturb_weights/3 for every set of weights associated with a particular Input_PId in the InputPIdPs list. If bias is in the weights list, it is reached last and perturbed just as any other weight, based on the probability. nAfterwards, the perturbed and inverted version of the InputIdsPlusWeights is reversed back to the proper order and returned to the calling function.

perturb_weights(MP, [W|Weights], Acc) ->
	U_W = case random:uniform() < MP of
		true -> sat((random:uniform()-0.5) * ?DELTA_MULTIPLIER + W, -?SAT_LIMIT, ?SAT_LIMIT);
		false -> W
	end,
	perturb_weights(MP, Weights, [U_W|Acc]);
perturb_weights(_MP, [], Acc) ->
	lists:reverse(Acc).

sat(Val, Min, Max) ->
	if
		Val < Min -> Min;
		Val > Max -> Max;
		true -> Val
	end.
%perturb_weights/3 accepts a probability value, a list of weights and an empty list to act as an accumulator. The function then goes through the weight list perturbing each weight with a probability of MP. The weights are constrained to be within the range of -SAT_LIMIT and SAT_LIMIT through the use of the sat/3 funcion.
