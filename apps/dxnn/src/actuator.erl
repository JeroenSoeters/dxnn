-module(actuator).
-compile(export_all).

gen(ExoSelf_PId, Node) ->
	spawn(Node, ?MODULE, prep, [ExoSelf_PId]).

prep(ExoSelf_PId) ->
	receive
		{ExoSelf_PId, Id, Cx_PId, Scape, ActuatorName, Fanin_PIds} ->
			loop(Id, ExoSelf_PId, Cx_PId, Scape, ActuatorName, {Fanin_PIds, Fanin_PIds}, [])
	end.
%When gen/2 is executed it spawns the actuator element and immediately begins to wait for its initial state message.

loop(Id, ExoSelf_PId, Cx_PId, Scape, AName, {[From_PId|Fanin_PIds], MFanin_PIds}, Acc) ->
	receive
		{From_PId, forward, Input} ->
			loop(Id, ExoSelf_PId, Cx_PId, Scape, AName, {Fanin_PIds, MFanin_PIds}, lists:append(Input, Acc));
		{ExoSelf_PId, terminate} ->
			ok
	end;
loop(Id, ExoSelf_PId, Cx_PId, Scape, AName, {[], MFanin_PIds}, Acc) ->
	{Fitness, HaltFlag} = actuator:AName(lists:reverse(Acc), Scape), 
	Cx_PId ! {self(), sync, Fitness, HaltFlag}, 
	loop(Id, ExoSelf_PId, Cx_PId, Scape, AName, {MFanin_PIds, MFanin_PIds}, []).
%The actuator process gathers the control signals from the neurons, appending them to the accumulator. The order in which the signals are accumulated into a vector is in the same order as the neuron ids are stored within NIds. Once all the signals have been gathered, the actuator sends the cortex the sync signal, executes its function, and then again begins to wait for the neural signals form teh output layer by resetting the Fanin_PIds from the second copy of the list.

pts(Result) ->
	io:format("actuator:pts(Result):~p~n", [Result]).
%The pts actuation function simply prints to the screen the vector passed to it.

xor_SendOutput(Output, Scape) ->
	Scape ! {self(), action,  Output},
	receive
		{Scape, Fitness, HaltFlag} ->
			{Fitness, HaltFlag}
	end.
%xor_SendOutput/2 function simply forwards the Output vector to the XOR simulator and then waits for the resulting Fitness and HaltFlag message from the scape.
