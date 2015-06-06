-module(test_helpers).
-compile(export_all).

create_sequence_generator([X|Xs]) ->
	spawn(?MODULE, sequence_generator, [lists:append([X|Xs], [eos])]).

next_item_from_sequence(GeneratorPid) ->
	GeneratorPid ! {self(), next},
	receive
		N -> N
	end.

sequence_generator([X|Xs]) ->
	receive
		{Pid, next} ->
			Pid ! X,
			sequence_generator(Xs)
	end.

in_transaction(F) ->
	{atomic, Result} = mnesia:transaction(F),
	Result.

find_neuron(NeuronId) ->
	find_node(neuron, NeuronId).

find_sensor(SensorId) ->
	find_node(sensor, SensorId).

find_actuator(ActuatorId) -> 
	find_node(actuator, ActuatorId).

find_agent(AgentId) ->
	find_node(agent, AgentId).

find_cortex(CortexId) ->
	find_node(cortex, CortexId).

find_node(NodeType, NodeId) ->
	F = fun() ->
		genotype:read({NodeType, NodeId})
	end,
	{atomic, Node} = mnesia:transaction(F),
	Node.
