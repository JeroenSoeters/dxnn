-module(genotype_mutator).
-include("records.hrl").
-compile(export_all).

%% doc based on the node types it dispatches to the correct link function.
create_link_between_elements(AgentId, FromElement, ToElement) ->
	case {FromElement, ToElement} of
		{{_FromId, neuron}, {_ToId, neuron}} ->
			create_link_between_neurons(AgentId, FromElement, ToElement);
		{{_FromId, sensor}, {_ToId, neuron}} ->
			not_implemented;
		{{_FromId, neuron}, {_ToId, actuator}} ->
			not_implemented
	end.

create_link_between_neurons(AgentId, FromId, ToId) ->
	Agent = genotype:read({agent, AgentId}),
	Generation = Agent#agent.generation,
	FromNeuron = genotype:read({neuron, FromId}),
 	UpdatedFromNeuron = link_from_neuron(FromNeuron, ToId, Generation),
	genotype:write(UpdatedFromNeuron),
	ToNeuron = genotype:read({neuron, ToId}),
	UpdatedToNeuron = link_to_neuron(FromId, ToNeuron, 1, Generation),
	genotype:write(UpdatedToNeuron).
	
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
			exit("******** ERROR: link_from_neuron cannot add ~p to input of ~p as it is already connected",
				[FromNeuronId, ToNeuron#neuron.id]);
		false ->
			UpdatedInputIdsPlusWeights = 
				[{FromNeuronId, genotype:create_neural_weights(VectorLength)}|InputIdsPlusWeights],
			ToNeuron#neuron{
				input_ids_plus_weights = UpdatedInputIdsPlusWeights,
				generation = Generation
			}
	end.

cut_link_between_elements() ->
	not_implemented.



