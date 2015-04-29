-module(genotype_mutator).
-include("records.hrl").
-compile(export_all).

%% doc based on the node types it dispatches to the correct link function.
create_link_between_elements(AgentId, FromElement, ToElement) ->
	case {FromElement, ToElement} of
		{{_FromId, neuron}, {_ToId, neuron}} ->
			create_link_between_neurons(AgentId, FromElement, ToElement);
		{{_FromId, sensor}, {_ToId, neuron}} ->
			create_link_between_sensor_and_neuron(AgentId, FromElement, ToElement);
		{{_FromId, neuron}, {_ToId, actuator}} ->
			create_link_between_neuron_and_actuator(AgentId, FromElement, ToElement)
	end.

create_link_between_neurons(AgentId, FromId, ToId) ->
	Generation = get_generation(AgentId),
	FromNeuron = genotype:read({neuron, FromId}),
 	UpdatedFromNeuron = link_from_neuron(FromNeuron, ToId, Generation),
	genotype:write(UpdatedFromNeuron),
	ToNeuron = genotype:read({neuron, ToId}),
	UpdatedToNeuron = link_to_neuron(FromId, ToNeuron, 1, Generation),
	genotype:write(UpdatedToNeuron).

create_link_between_sensor_and_neuron(AgentId, SensorId, NeuronId) ->
	Generation = get_generation(AgentId),
	Sensor = genotype:read({sensor, SensorId}),
	UpdatedSensor = link_from_sensor(Sensor, NeuronId),
	genotype:write(UpdatedSensor),
	Neuron = genotype:read({neuron, NeuronId}),
	UpdatedNeuron = link_to_neuron(SensorId, Neuron, Sensor#sensor.vl, Generation),
	genotype:write(UpdatedNeuron).

create_link_between_neuron_and_actuator(AgentId, NeuronId, ActuatorId) ->
	Generation = get_generation(AgentId),
	Actuator = genotype:read({actuator, ActuatorId}),
	UpdatedActuator = link_to_actuator(Actuator, NeuronId),
	genotype:write(UpdatedActuator),
	Neuron = genotype:read({neuron, NeuronId}),
	UpdatedNeuron = link_from_neuron(Neuron, ActuatorId, Generation),
	genotype:write(UpdatedNeuron).

link_from_sensor(Sensor, NeuronId) ->
	FanoutIds = Sensor#sensor.fanout_ids,
	case lists:member(NeuronId, FanoutIds) of
		true ->
			exit("******** ERROR: link_from_sensor cannot add ~p to fanout of ~p as it is already connected",
				[NeuronId, Sensor#sensor.id]);
		false ->
			Sensor#sensor{fanout_ids = [NeuronId|FanoutIds]}
	end.

link_to_actuator(Actuator, NeuronId) ->
	FaninIds = Actuator#actuator.fanin_ids,
	case lists:member(NeuronId, FaninIds) of
		true ->
			exit("******** ERROR: link_to_actuator cannot add ~p to fanin of ~p as it is already connected",
				[NeuronId, Actuator#actuator.id]);
		false ->
			Actuator#actuator{fanin_ids = [NeuronId|FaninIds]}
	end.

link_from_neuron(FromNeuron, ToId, Generation) ->
	{{FromLayerIndex, _}, _} = FromNeuron#neuron.id,
	{{ToLayerIndex, _}, _} = ToId,
	OutputIds = FromNeuron#neuron.output_ids,
	RecursiveOutputIds = FromNeuron#neuron.recursive_output_ids,
	case lists:member(ToId, OutputIds) of
		true ->
			exit("******** ERROR: link_from_neuron cannot add ~p to output of ~p as it is already connected",
				[ToId, FromNeuron#neuron.id]);
		false ->
			{UpdatedOutputIds, UpdatedRecursiveOutputIds} = case ToLayerIndex >= FromLayerIndex of
				true ->
					{[ToId|OutputIds], [ToId|RecursiveOutputIds]};
				false ->
					{[ToId|OutputIds], RecursiveOutputIds}
			end,
			FromNeuron#neuron{
				output_ids = UpdatedOutputIds,
				recursive_output_ids = UpdatedRecursiveOutputIds,
				generation = Generation
			}
	end.

link_to_neuron(FromNeuronId, ToNeuron, VectorLength, Generation) ->
	InputIdsPlusWeights = ToNeuron#neuron.input_ids_plus_weights,
	case lists:keymember(FromNeuronId, 1, InputIdsPlusWeights) of
		true ->
			exit("******** ERROR: link_to_neuron cannot add ~p to input of ~p as it is already connected",
				[FromNeuronId, ToNeuron#neuron.id]);
		false ->
			UpdatedInputIdsPlusWeights = 
				[{FromNeuronId, genotype:create_neural_weights(VectorLength)}|InputIdsPlusWeights],
			ToNeuron#neuron{
				input_ids_plus_weights = UpdatedInputIdsPlusWeights,
				generation = Generation
			}
	end.

cut_link_between_elements(AgentId, FromElement, ToElement) ->
	case {FromElement, ToElement} of
		{{_FromId, neuron}, {_ToId, neuron}} ->
			cut_link_between_neurons(AgentId, FromElement, ToElement);
		{{_FromId, sensor}, {_ToId, neuron}} ->
			cut_link_between_sensor_and_neuron(AgentId, FromElement, ToElement);
		{{_FromId, neuron}, {_ToId, actuator}} ->
			cut_link_between_neuron_and_actuator(AgentId, FromElement, ToElement)
	end.

cut_link_between_neurons(AgentId, FromNeuronId, ToNeuronId) ->
	Generation = get_generation(AgentId),
	FromNeuron = genotype:read({neuron, FromNeuronId}),
	UpdatedFromNeuron = cut_link_from_neuron(FromNeuron, ToNeuronId, Generation),
	genotype:write(UpdatedFromNeuron),
	ToNeuron = genotype:read({neuron, ToNeuronId}),
	UpdatedToNeuron = cut_link_to_neuron(ToNeuron, FromNeuronId, Generation),
	genotype:write(UpdatedToNeuron).

cut_link_between_sensor_and_neuron(AgentId, SensorId, NeuronId) ->
	Generation = get_generation(AgentId),
	Sensor = genotype:read({sensor, SensorId}),
	UpdatedSensor = cut_link_from_sensor(Sensor, NeuronId),
	genotype:write(UpdatedSensor),
	Neuron = genotype:read({neuron, NeuronId}),
	UpdatedNeuron = cut_link_to_neuron(Neuron, SensorId, Generation),
	genotype:write(UpdatedNeuron).

cut_link_between_neuron_and_actuator(AgentId, NeuronId, ActuatorId) ->
	Generation = get_generation(AgentId),
	Actuator = genotype:read({actuator, ActuatorId}),
	UpdatedActuator = cut_link_to_actuator(Actuator, NeuronId),
	genotype:write(UpdatedActuator),
	Neuron = genotype:read({neuron, NeuronId}),
	UpdatedNeuron = cut_link_from_neuron(Neuron, ActuatorId, Generation),
	genotype:write(UpdatedNeuron).

cut_link_from_neuron(FromNeuron, ToNeuronId, Generation) ->
	OutputIds = FromNeuron#neuron.output_ids,
	RecursiveOutputIds = FromNeuron#neuron.recursive_output_ids,
	case lists:member(ToNeuronId, OutputIds) of
		true ->
			UpdatedOutputIds = OutputIds -- [ToNeuronId],
			UpdatedRecursiveOutputIds = RecursiveOutputIds -- [ToNeuronId],
			U = FromNeuron#neuron{
				output_ids = UpdatedOutputIds,
				recursive_output_ids = UpdatedRecursiveOutputIds,
				generation = Generation
			},
			U;
		false ->
			exit("******** ERROR: cut_link_from_neuron cannot remove ~p from output of ~p as it is not connected",
				[ToNeuronId, FromNeuron#neuron.id])
	end.

cut_link_to_neuron(ToNeuron, FromNeuronId, Generation) ->
	InputIdsPlusWeights = ToNeuron#neuron.input_ids_plus_weights,
	io:format("cutting link to neuron. from id: ~p~n idps: ~p~n", [FromNeuronId, InputIdsPlusWeights]),
	case lists:keymember(FromNeuronId, 1, InputIdsPlusWeights) of
		true ->
			UpdatedInputIdsPlusWeights = lists:keydelete(FromNeuronId, 1, InputIdsPlusWeights),
			ToNeuron#neuron{
				input_ids_plus_weights = UpdatedInputIdsPlusWeights,
				generation = Generation
			};
		false ->
			exit("******** ERROR: cut_link_to_neuron cannot remove ~p from input of ~p as it is not connected",
				[FromNeuronId, ToNeuron#neuron.id])
	end.

cut_link_from_sensor(Sensor, NeuronId) ->
	FanoutIds = Sensor#sensor.fanout_ids,
	case lists:member(NeuronId, FanoutIds) of
		true ->
			Sensor#sensor{fanout_ids = FanoutIds -- [NeuronId]};
		false ->
			exit("******** ERROR: cut_link_from_sensor cannot remove ~p from fanout of ~p as it is not connected",
				[NeuronId, Sensor#sensor.id])
	end.

cut_link_to_actuator(Actuator, NeuronId) ->
	FaninIds = Actuator#actuator.fanin_ids,
	case lists:member(NeuronId, FaninIds) of
		true ->
			Actuator#actuator{fanin_ids = FaninIds -- [NeuronId]};
		false ->
			exit("******** ERROR: cut_link_to_actuator cannot remove ~p from fanin of ~p as it is not connected",
				[NeuronId, Actuator#actuator.id])
	end.
%% cut_link_from_sensor, cut_link_from_neuron, cut_link_to_neuron, cut_link_to_actuator

get_generation(AgentId) ->
	Agent = genotype:read({agent, AgentId}),
	Agent#agent.generation.
