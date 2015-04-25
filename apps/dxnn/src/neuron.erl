-module(neuron).
-compile(export_all).
-include("records.hrl").
-define(DELTA_MULTIPLIER, math:pi()*2).
-define(SAT_LIMIT, math:pi()*2).

gen(ExoSelf_PId, Node) ->
	spawn(Node, ?MODULE, prep, [ExoSelf_PId]).

prep(ExoSelf_PId) ->
	{V1, V2, V3} = now(),
	random:seed(V1, V2, V3),
	receive
		{ExoSelf_PId, {Id, Cx_PId, AF, Input_PIdPs, Output_PIds}} ->
			loop(Id, ExoSelf_PId, Cx_PId, AF, {Input_PIdPs, Input_PIdPs}, Output_PIds, 0)
	end.
%When gen/2 is executed it spawns the neuron element and immediately begins to wait for its initial state message.

loop(Id, ExoSelf_PId, Cx_PId, AF, {[{Input_PId, Weights}|Input_PIdPs], MInput_PIdPs}, Output_PIds, Acc) ->
	receive
		{Input_PId, forward, Input} ->
			Result = dot(Input, Weights, 0),
			loop(Id, ExoSelf_PId, Cx_PId, AF, {Input_PIdPs, MInput_PIdPs}, Output_PIds, Result+Acc);
		{ExoSelf_PId, weight_backup} ->
      put(weights, MInput_PIdPs),
		  loop(Id, ExoSelf_PId, Cx_PId, AF, {[{Input_PId, Weights}|Input_PIdPs], MInput_PIdPs}, Output_PIds, Acc);
		{ExoSelf_PId, weight_restore} ->
			RInput_PIdPs = get(weights),
			loop(Id, ExoSelf_PId, Cx_PId, AF, {RInput_PIdPs, RInput_PIdPs}, Output_PIds, Acc);
		{ExoSelf_PId, weight_perturb} ->
			PInput_PIdPs = perturb_PIdPs(MInput_PIdPs),
			loop(Id, ExoSelf_PId, Cx_PId, AF, {PInput_PIdPs, PInput_PIdPs}, Output_PIds, Acc);
		{ExoSelf_PId, get_backup} ->
			ExoSelf_PId ! {self(), Id, MInput_PIdPs},
			loop(Id, ExoSelf_PId, Cx_PId, AF, {[{Input_PId, Weights}|Input_PIdPs], MInput_PIdPs}, Output_PIds, Acc);
		{ExoSelf_PId, terminate} ->
			ok
	end;
loop(Id, ExoSelf_PId, Cx_PId, AF, {[Bias], MInput_PIdPs}, Output_PIds, Acc) ->
	Output = neuron:AF(Acc+Bias),
	[Output_PId ! {self(), forward, [Output]} || Output_PId <- Output_PIds],
	loop(Id, ExoSelf_PId, Cx_PId, AF, {MInput_PIdPs, MInput_PIdPs}, Output_PIds, 0);
loop(Id, ExoSelf_PId, Cx_PId, AF, {[], MInput_PIdPs}, Output_PIds, Acc) ->
	Output = neuron:AF(Acc),
	[Output_PId ! {self(), forward, [Output]} || Output_PId <- Output_PIds],
	loop(Id, ExoSelf_PId, Cx_PId, AF, {MInput_PIdPs, MInput_PIdPs}, Output_PIds, 0).

	dot([I|Input],[W|Weights],Acc) ->
		dot(Input,Weights,I*W+Acc);
	dot([],[],Acc) ->
		Acc.
%The neuron process waits for vector signals form all the processes that it's connected from, taking the dot product of the input and weight vectors, and then adding it to the accumulator. Once all the signals form Input_PIds are received, the accumulator contains the dot product to which the neuron then adds the bias and executes the actuvation function on. After fanning out the output signal, the neuron again returns to waiting for incoming signals. When the neuron receives the {Cx_PId,get_backup} message, it forwards to the cortex its full MInput_PIdPs list and its Id. Once the training/learning algorithm is added to the system, the MInputPs would contain a full set of the most recent and updated version of the weights.

	tanh(Val) ->
		math:tanh(Val).
%Though in this implementation the neuron has only the tanh/1 function available to it, we will later extend the system to allow different neurons to use different activation functions.

  perturb_PIdPs(Input_PIdPs) ->
		Tot_Weights = lists:sum([length(Weights) || {_InputPId, Weights} <- Input_PIdPs]),
		MP = 1/math:sqrt(Tot_Weights),
		perturb_PIdPs(MP, Input_PIdPs, []).
	perturb_PIdPs(MP, [{Input_PId, Weights}|Input_PIdPs], Acc) ->
		U_Weights = perturb_weights(MP, Weights, []),
		perturb_PIdPs(MP, Input_PIdPs, [{Input_PId, U_Weights}|Acc]);
	perturb_PIdPs(MP, [Bias], Acc) ->
		U_Bias = case random:uniform() < MP of
			true -> sat((random:uniform()-0.5) * ?DELTA_MULTIPLIER + Bias, -?SAT_LIMIT, ?SAT_LIMIT);
			false -> Bias
		end,
		lists:reverse([U_Bias|Acc]);
	perturb_PIdPs(_MP, [], Acc) ->
		lists:reverse(Acc).
%perturb_PIdPs/1 first calculates the probability that a weight will be perturbed, the probability being the inverse quare root of the total number of weights in the neuron. The function then drops into perturb_PIdPs/3 which executes perturb_weights/3 for every set of weights associated with a particular Input_PId in the InputPIdPs list. If bias is in the weights list, it is reached last and perturbed just as any other weight, based on the probability. nAfterwards, the perturbed and inverted version of the Input_PIdPs is reversed back to the proper order and returned to the calling function.

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
