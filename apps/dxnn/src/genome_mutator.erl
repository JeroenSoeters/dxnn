-module(genome_mutator).
-include("records.hrl").
-include_lib("eunit/include/eunit.hrl").
-compile(export_all).
-define(DELTA_MULTIPLIER, math:pi()*2).
-define(SAT_LIMIT, math:pi()*2).
-define(TIME_PROVIDER, fun() -> now() end).
-define(MUTATORS, [
	{mutate_weights, []},
	{add_bias, []},
	{mutate_af, []},
	{remove_bias, []},
	{add_outlink, []},
	{add_inlink, []},
	{add_sensorlink, []},
	{add_actuatorlink, []},
	{add_neuron, [?TIME_PROVIDER]},
	{outsplice, [?TIME_PROVIDER]},
	{add_sensor, [?TIME_PROVIDER]},
	{add_actuator, [?TIME_PROVIDER]}
]).

mutate(AgentId) ->
	random:seed(now()),
	F = fun() ->
		Agent = genotype:read({agent, AgentId}),
		NextGeneration = Agent#agent.generation + 1,
		genotype:write(Agent#agent{generation = NextGeneration}),
		apply_mutation_operators(AgentId),
		genotype:update_fingerprint(AgentId)
	end,
	mnesia:transaction(F).

apply_mutation_operators(AgentId) ->
	Agent = genotype:read({agent, AgentId}),
	Cortex = genotype:read({cortex, Agent#agent.cortex_id}),
	Neurons = length(Cortex#cortex.neuron_ids),
	Mutations = random:uniform(round(math:sqrt(Neurons))),
	io:format("Total number of neurons: ~p Performing ~p mutations on agent: ~p", [Neurons, Mutations, AgentId]),
	apply_mutation_operators(AgentId, Mutations).

apply_mutation_operators(_AgentId, 0) ->
	done;
apply_mutation_operators(AgentId, Index) ->
	Result = apply_mutation_operator(AgentId),
	case Result of
		{atomic, _} ->
			apply_mutation_operators(AgentId, Index - 1);
		Error ->
			io:format("~p~n Retrying with new mutation operator...~n", [Error]),
			apply_mutation_operators(AgentId, Index)
	end.

apply_mutation_operator(AgentId) ->
	F = fun() ->
		Mutators = ?MUTATORS,
		{Mutator, Args} = lists:nth(random:uniform(length(Mutators)), Mutators),
		io:format("Mutation Operator: ~p~n", [Mutator]),
		case Args of
			[] ->
				genome_mutator:Mutator(AgentId);
			_ ->
				(apply(genome_mutator, Mutator, Args))(AgentId)
		end
	end,
	mnesia:transaction(F).

%% ===================================================================
%% Mutation operators
%% ===================================================================

mutate_weights(AgentId) ->
	Agent = genotype:read({agent, AgentId}),
	Neuron = select_random_neuron(Agent),
	UpdatedNeuron = Neuron#neuron{
		input_ids_plus_weights = perturb_ids_plus_weights(Neuron#neuron.input_ids_plus_weights)
	},
	UpdatedAgent = Agent#agent{
		evo_hist = [{mutate_weights, Neuron#neuron.id}|Agent#agent.evo_hist]
	},
	genotype:write(UpdatedNeuron),
	genotype:write(UpdatedAgent).

perturb_ids_plus_weights(IdsPlusWeights) ->
	MP = 1/math:sqrt(length(IdsPlusWeights)),
	perturb_ids_plus_weights(MP, IdsPlusWeights, []).
perturb_ids_plus_weights(MP, [{Id, Weights}|IdsPlusWeights], Acc) ->
	UpdatedWeights = perturb_weights(MP, Weights, []),
	perturb_ids_plus_weights(MP, IdsPlusWeights, [{Id, UpdatedWeights}|Acc]);
perturb_ids_plus_weights(_MP, [], Acc) ->
	lists:reverse(Acc).

perturb_weights(MP, [W|Weights], Acc) ->
	UpdatedWeight = case random:uniform() < MP of
		true -> 
			sat((random:uniform()-0.5)*?DELTA_MULTIPLIER+W, -?SAT_LIMIT, ?SAT_LIMIT);
		false ->
			W
	end,
	perturb_weights(MP, Weights, [UpdatedWeight|Acc]);
perturb_weights(_MP, [], Acc) ->
	lists:reverse(Acc).
	  
sat(Val, Min, Max) ->
	if
		Val < Min -> Min;
		Val > Max -> Max;
		true -> Val
	end.

add_bias(AgentId) ->
	Agent = genotype:read({agent, AgentId}),
	Generation = Agent#agent.generation,
	Neuron = select_random_neuron(Agent),
	case lists:keymember(bias, 1, Neuron#neuron.input_ids_plus_weights) of
		true ->
			exit("******** ERROR: add_bias cannot add bias to neuron as it is already has a bias");
		false ->
			InputIdsPlusWeights = Neuron#neuron.input_ids_plus_weights,
			UpdatedInputIdsPlusWeights = lists:append(InputIdsPlusWeights, [{bias, [random:uniform()-0.5]}]),
			UpdatedNeuron = Neuron#neuron{
				input_ids_plus_weights = UpdatedInputIdsPlusWeights,
				generation = Generation
			},
			UpdatedAgent = Agent#agent{
				evo_hist = [{add_bias, Neuron#neuron.id}|Agent#agent.evo_hist]
			},
			genotype:write(UpdatedNeuron),
			genotype:write(UpdatedAgent)
	end.
			
remove_bias(AgentId) ->
	Agent = genotype:read({agent, AgentId}),
	Generation = Agent#agent.generation,
	Neuron = select_random_neuron(Agent),
	case lists:keymember(bias, 1, Neuron#neuron.input_ids_plus_weights) of
		false ->
			exit("******** ERROR: add_bias cannot remove bias from neuron as it is doesn't has a bias");
		true ->
			UpdatedInputIdsPlusWeights = lists:keydelete(bias, 1, Neuron#neuron.input_ids_plus_weights),
			UpdatedNeuron = Neuron#neuron{
				input_ids_plus_weights = UpdatedInputIdsPlusWeights,
				generation = Generation
			},
			UpdatedAgent = Agent#agent{
				evo_hist = [{remove_bias, Neuron#neuron.id}|Agent#agent.evo_hist]
			},
			genotype:write(UpdatedNeuron),
			genotype:write(UpdatedAgent)
	end.
		
mutate_af(AgentId) ->
	Agent = genotype:read({agent, AgentId}),
	Generation = Agent#agent.generation,
	Neuron = select_random_neuron(Agent),
	case (Agent#agent.constraint)#constraint.neural_afs -- [Neuron#neuron.af] of
		[] ->
			exit("******** ERROR: mutate_af cannot mutate activation function as there are no other activation functions available");
		ActivationFunctions ->
			UpdatedNeuron = Neuron#neuron{
				af = genotype:generate_activation_function(ActivationFunctions),
				generation = Generation
			},
			UpdatedAgent = Agent#agent{
				evo_hist = [{mutate_af, Neuron#neuron.id}|Agent#agent.evo_hist]
			},
			genotype:write(UpdatedNeuron),
			genotype:write(UpdatedAgent)
	end.

add_outlink(AgentId) ->
	Agent = genotype:read({agent, AgentId}),
	CortexId = Agent#agent.cortex_id,
	Cortex = genotype:read({cortex, CortexId}),
	Neuron = select_random_neuron(Agent),
	OutputIds = Neuron#neuron.output_ids,
	case (Cortex#cortex.neuron_ids ++ Cortex#cortex.actuator_ids) -- OutputIds of
		[] ->
			exit("******** ERROR: add_outlink cannot add outlink to neuron as it is already connected to all other elements");
		ElementIds ->
			ToElement = lists:nth(random:uniform(length(ElementIds)), ElementIds),
			create_link_between_elements(AgentId, Neuron#neuron.id, ToElement),
			UpdatedAgent = Agent#agent{
				evo_hist = [{add_outlink, Neuron#neuron.id, ToElement}|Agent#agent.evo_hist]
			},
			genotype:write(UpdatedAgent)
	end.

add_inlink(AgentId) ->
	Agent = genotype:read({agent, AgentId}),
	CortexId = Agent#agent.cortex_id,
	Cortex = genotype:read({cortex, CortexId}),
	Neuron = select_random_neuron(Agent),
	InputIds = [Id || {Id, _Weights} <- Neuron#neuron.input_ids_plus_weights],
	case (Cortex#cortex.sensor_ids ++ Cortex#cortex.neuron_ids) -- InputIds of 
		[] ->
			exit("******** ERROR: add_inlink cannot add inlink to neuron as it is already connected to all other elements");
		ElementIds ->
			FromElement = lists:nth(random:uniform(length(ElementIds)), ElementIds),
			create_link_between_elements(AgentId, FromElement, Neuron#neuron.id),
			UpdatedAgent = Agent#agent{
				evo_hist = [{add_inlink, FromElement, Neuron#neuron.id}|Agent#agent.evo_hist]
			},
			genotype:write(UpdatedAgent)
	end.

add_sensorlink(AgentId) ->
	Agent = genotype:read({agent, AgentId}),
	CortexId = Agent#agent.cortex_id,
	Cortex = genotype:read({cortex, CortexId}),
	SensorId = lists:nth(random:uniform(length(Cortex#cortex.sensor_ids)), Cortex#cortex.sensor_ids),
	Sensor = genotype:read({sensor, SensorId}),
	FanoutIds = Sensor#sensor.fanout_ids,
	case Cortex#cortex.neuron_ids -- FanoutIds of
		[] -> 
			exit("******** ERROR: add_sensor cannot add outlink to sensor as it is already connected to all neurons");
		NeuronIds ->
			NeuronId = lists:nth(random:uniform(length(NeuronIds)), NeuronIds),
			create_link_between_elements(AgentId, SensorId, NeuronId),
			UpdatedAgent = Agent#agent{
				evo_hist = [{add_sensorlink, Sensor#sensor.id, NeuronId}|Agent#agent.evo_hist]
			},
			genotype:write(UpdatedAgent)
	end.

add_actuatorlink(AgentId) ->
	Agent = genotype:read({agent, AgentId}),
	CortexId = Agent#agent.cortex_id,
	Cortex = genotype:read({cortex, CortexId}),
	ActuatorId = lists:nth(random:uniform(length(Cortex#cortex.actuator_ids)), Cortex#cortex.actuator_ids),
	Actuator = genotype:read({actuator, ActuatorId}),
	FaninIds = Actuator#actuator.fanin_ids,
	case Cortex#cortex.neuron_ids -- FaninIds of
		[] -> 
			exit("******** ERROR: add_actuator cannot add inlink to actuator as it is already connected to all neurons");
		NeuronIds ->
			NeuronId = lists:nth(random:uniform(length(NeuronIds)), NeuronIds),
			create_link_between_elements(AgentId, NeuronId, ActuatorId),
			UpdatedAgent = Agent#agent{
				evo_hist = [{add_actuatorlink, NeuronId, Actuator#actuator.id}|Agent#agent.evo_hist]
			},
			genotype:write(UpdatedAgent)
	end.

add_neuron(TimeProvider) ->
	fun(AgentId) ->
		Agent = genotype:read({agent, AgentId}),
		Generation = Agent#agent.generation,
		Pattern = Agent#agent.pattern,
		CortexId = Agent#agent.cortex_id,
		Cortex = genotype:read({cortex, CortexId}),
		NeuronIds = Cortex#cortex.neuron_ids,
		ActuatorIds = Cortex#cortex.actuator_ids,
		SensorIds = Cortex#cortex.sensor_ids,
		{TargetLayerIndex, TargetLayerNeuronIds} = lists:nth(random:uniform(length(Pattern)), Pattern),
		NewNeuronId = {{TargetLayerIndex, genotype:generate_unique_id(TimeProvider)}, neuron},
		UpdatedPattern = lists:keyreplace(TargetLayerIndex, 1, Agent#agent.pattern, 
			{TargetLayerIndex, [NewNeuronId|TargetLayerNeuronIds]}),
		SpeciesConstraint = Agent#agent.constraint,
		genotype:construct_neuron(CortexId, Generation, SpeciesConstraint, NewNeuronId, [], []),
		AvailableFromElements = SensorIds ++ NeuronIds,
		AvailableToElements = NeuronIds ++ ActuatorIds,
		FromElement = lists:nth(random:uniform(length(AvailableFromElements)), AvailableFromElements),
		ToElement = lists:nth(random:uniform(length(AvailableToElements)), AvailableToElements),
		create_link_between_elements(AgentId, FromElement, NewNeuronId),
		create_link_between_elements(AgentId, NewNeuronId, ToElement),
		genotype:write(Cortex#cortex{ neuron_ids = [NewNeuronId|NeuronIds] }),
		genotype:write(Agent#agent{
			pattern = UpdatedPattern,
			evo_hist = [{add_neuron, FromElement, NewNeuronId, ToElement}|Agent#agent.evo_hist]
		})
	end.

outsplice(TimeProvider) ->
	fun(AgentId) ->
		Agent = genotype:read({agent, AgentId}),
		Generation = Agent#agent.generation,
		Pattern = Agent#agent.pattern,
		CortexId = Agent#agent.cortex_id,
		Cortex = genotype:read({cortex, CortexId}),
		NeuronIds = Cortex#cortex.neuron_ids,
		FromNeuronId = lists:nth(random:uniform(length(NeuronIds)), NeuronIds),
		Neuron = genotype:read({neuron, FromNeuronId}),
		{{FromLayerIndex, _FromUID}, neuron} = FromNeuronId,
		AvailableIds = case [{{TargetLayerIndex, TargetUID}, TargetType} || 
			{{TargetLayerIndex, TargetUID}, TargetType}	<- Neuron#neuron.output_ids, TargetLayerIndex > FromLayerIndex] of
			[] ->
				exit("******** ERROR: outsplice cannot outsplice after neuron as there are no feed-forward output connections available");
			Ids ->
				Ids
		end,
		ToNeuronId = lists:nth(random:uniform(length(AvailableIds)), AvailableIds),
		{{ToLayerIndex, _ToUID}, _ToType} = ToNeuronId,
		NewNeuronLayerIndex = get_new_layer_index(FromLayerIndex, ToLayerIndex, next, Pattern),
		NewNeuronId = {{NewNeuronLayerIndex, genotype:generate_unique_id(TimeProvider)}, neuron},
		SpeciesConstraint = Agent#agent.constraint,
		genotype:construct_neuron(CortexId, Generation, SpeciesConstraint, NewNeuronId, [], []),
		cut_link_between_elements(AgentId, FromNeuronId, ToNeuronId),
		create_link_between_elements(AgentId, FromNeuronId, NewNeuronId),
		create_link_between_elements(AgentId, NewNeuronId, ToNeuronId),
		UpdatedPattern = case lists:keymember(NewNeuronId, 1, Pattern) of
			true ->
				{NewNeuronLayerIndex, IdsInLayer} = lists:keyfind(NewNeuronLayerIndex, 1, Pattern),
				lists:keyreplace(NewNeuronLayerIndex, 1, Pattern, {NewNeuronLayerIndex, [NewNeuronId|IdsInLayer]});
			false ->
				lists:sort([{NewNeuronLayerIndex, [NewNeuronId]}|Pattern])
		end,
		UpdatedAgent = Agent#agent{
			pattern = UpdatedPattern,
			evo_hist = [{outsplice, FromNeuronId, NewNeuronId, ToNeuronId}|Agent#agent.evo_hist]
		},
		UpdatedCortex = Cortex#cortex{
			neuron_ids = [NewNeuronId|Cortex#cortex.neuron_ids]
		},	
		genotype:write(UpdatedCortex),
		genotype:write(UpdatedAgent)
	end.

get_new_layer_index(LayerIndex, LayerIndex, _Direction, _Pattern) ->
	exit("******** ERROR: get_new_layer_index: both neurons have the same layer index");
get_new_layer_index(FromLayerIndex, ToLayerIndex, Direction, Pattern) ->
	NewLayerIndex = case Direction of
		next -> 
			get_next_layer_index(Pattern, FromLayerIndex, ToLayerIndex);
		prev ->
			get_previous_layer_index(lists:reverse(Pattern), FromLayerIndex, ToLayerIndex)
	end,
	NewLayerIndex.

get_next_layer_index([{FromLayerIndex, _LayerNeuronIds}], FromLayerIndex, ToLayerIndex) ->
	(FromLayerIndex + ToLayerIndex) / 2;
get_next_layer_index([{LayerIndex, _LayerNeuronIds}|Pattern], FromLayerIndex, ToLayerIndex) ->
	case LayerIndex == FromLayerIndex of
		true ->
			[{NextLayerIndex, _NextLayerNeuronIds}|_] = Pattern,
			case NextLayerIndex == ToLayerIndex of
				true ->
					(FromLayerIndex + ToLayerIndex) / 2;
				false ->
					NextLayerIndex
			end;
		false ->
			get_next_layer_index(Pattern, FromLayerIndex, ToLayerIndex)
	end.
	
get_previous_layer_index([{FromLayerIndex, _LayerNeuronIds}], FromLayerIndex, ToLayerIndex) ->
	(FromLayerIndex + ToLayerIndex) / 2;
get_previous_layer_index([{LayerIndex, _LayerNeuronIds}|Pattern], FromLayerIndex, ToLayerIndex) ->
	case LayerIndex == FromLayerIndex of
		true ->
			[{PreviousLayerIndex, _PreviousLayerNeuronIds}|_] = Pattern,
			case PreviousLayerIndex == ToLayerIndex of
				true ->
					(FromLayerIndex + ToLayerIndex) / 2;
				false ->
					PreviousLayerIndex
			end;
		false ->
			get_previous_layer_index(Pattern, FromLayerIndex, ToLayerIndex)
	end.

add_sensor(TimeProvider) ->
	fun(AgentId) ->
		Agent = genotype:read({agent, AgentId}),
		CortexId = Agent#agent.cortex_id,
		Cortex = genotype:read({cortex, CortexId}),
		SensorIds = Cortex#cortex.sensor_ids,
		SpeciesConstraint = Agent#agent.constraint,
		Morphology = SpeciesConstraint#constraint.morphology,
		case morphology:get_sensors(Morphology) --
			[(genotype:read({sensor, SensorId}))#sensor{id=undefined, cortex_id=undefined, fanout_ids=[], generation=undefined} 
				|| SensorId <- SensorIds] of
			[] ->
				exit("******** ERROR: add_sensor cannot add sensor as the NN is already using all available sensors");
			AvailableSensors ->
				NewSensorId = {{-1, genotype:generate_unique_id(TimeProvider)}, sensor},
				NewSensor = (lists:nth(random:uniform(length(AvailableSensors)), AvailableSensors))#sensor{
					id = NewSensorId,
					cortex_id = CortexId
				},
				genotype:write(NewSensor),
				NeuronIds = Cortex#cortex.neuron_ids,
				NeuronId = lists:nth(random:uniform(length(NeuronIds)), NeuronIds),
				create_link_between_elements(AgentId, NewSensorId, NeuronId),
				UpdatedCortex = Cortex#cortex{
					sensor_ids = [NewSensorId|Cortex#cortex.sensor_ids]
				},
				UpdatedAgent = Agent#agent{
					evo_hist = [{add_sensor, NewSensorId, NeuronId}|Agent#agent.evo_hist]
				},
				genotype:write(UpdatedAgent),
				genotype:write(UpdatedCortex)
		end
	end.
	
add_actuator(TimeProvider) ->
	fun(AgentId) ->
		Agent = genotype:read({agent, AgentId}),
		CortexId = Agent#agent.cortex_id,
		Cortex = genotype:read({cortex, CortexId}),
		ActuatorIds = Cortex#cortex.actuator_ids,
		SpeciesConstraint = Agent#agent.constraint,
		Morphology = SpeciesConstraint#constraint.morphology,
		case morphology:get_actuators(Morphology) --
			[(genotype:read({actuator, ActuatorId}))#actuator{id=undefined, cortex_id=undefined, fanin_ids=[], generation=undefined} 
				|| ActuatorId <- ActuatorIds] of
			[] ->
				exit("******** ERROR: add_actuator cannot add actuator as the NN is already using all available actuators");
			AvailableActuators ->
				NewActuatorId = {{-1, genotype:generate_unique_id(TimeProvider)}, actuator},
				NewActuator = (lists:nth(random:uniform(length(AvailableActuators)), AvailableActuators))#actuator{
					id = NewActuatorId,
					cortex_id = CortexId
				},
				genotype:write(NewActuator),
				NeuronIds = Cortex#cortex.neuron_ids,
				NeuronId = lists:nth(random:uniform(length(NeuronIds)), NeuronIds),
				create_link_between_elements(AgentId, NeuronId, NewActuatorId),
				UpdatedCortex = Cortex#cortex{
					actuator_ids = [NewActuatorId|Cortex#cortex.actuator_ids]
				},
				UpdatedAgent = Agent#agent{
					evo_hist = [{add_actuator, NewActuatorId, NeuronId}|Agent#agent.evo_hist]
				},
				genotype:write(UpdatedAgent),
				genotype:write(UpdatedCortex)
		end
	end.

select_random_neuron(Agent) ->
	CortexId = Agent#agent.cortex_id,
	Cortex = genotype:read({cortex, CortexId}),
	NeuronIds = Cortex#cortex.neuron_ids,
	NeuronId = lists:nth(random:uniform(length(NeuronIds)), NeuronIds),
	genotype:read({neuron, NeuronId}).

%% ===================================================================
%% Creating and cutting links
%% ===================================================================

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
	UpdatedSensor = link_from_sensor(Sensor, NeuronId, Generation),
	genotype:write(UpdatedSensor),
	Neuron = genotype:read({neuron, NeuronId}),
	UpdatedNeuron = link_to_neuron(SensorId, Neuron, Sensor#sensor.vl, Generation),
	genotype:write(UpdatedNeuron).

create_link_between_neuron_and_actuator(AgentId, NeuronId, ActuatorId) ->
	Generation = get_generation(AgentId),
	Actuator = genotype:read({actuator, ActuatorId}),
	UpdatedActuator = link_to_actuator(Actuator, NeuronId, Generation),
	genotype:write(UpdatedActuator),
	Neuron = genotype:read({neuron, NeuronId}),
	UpdatedNeuron = link_from_neuron(Neuron, ActuatorId, Generation),
	genotype:write(UpdatedNeuron).

link_from_sensor(Sensor, NeuronId, Generation) ->
	FanoutIds = Sensor#sensor.fanout_ids,
	case lists:member(NeuronId, FanoutIds) of
		true ->
			exit("******** ERROR: link_from_sensor cannot add to fanout as it is already connected");
		false ->
			Sensor#sensor{
				fanout_ids = [NeuronId|FanoutIds],
				generation = Generation
			}
	end.

link_to_actuator(Actuator, NeuronId, Generation) ->
	FaninIds = Actuator#actuator.fanin_ids,
	case length(FaninIds) >= Actuator#actuator.vl of
		true ->
			exit("******** ERROR: link_to_actuator cannot add to fanin as it is already connected");
		false ->
			Actuator#actuator{
				fanin_ids = [NeuronId|FaninIds],
				generation = Generation
			}
	end.

link_from_neuron(FromNeuron, ToId, Generation) ->
	{{FromLayerIndex, _}, _} = FromNeuron#neuron.id,
	{{ToLayerIndex, _}, _} = ToId,
	OutputIds = FromNeuron#neuron.output_ids,
	RecursiveOutputIds = FromNeuron#neuron.recursive_output_ids,
	case lists:member(ToId, OutputIds) of
		true ->
			exit("******** ERROR: link_from_neuron cannot add to output as it is already connected");
		false ->
			{UpdatedOutputIds, UpdatedRecursiveOutputIds} = case ToLayerIndex =< FromLayerIndex of
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
			exit("******** ERROR: link_to_neuron cannot add to input as it is already connected");
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
			exit("******** ERROR: cut_link_from_neuron cannot remove from output as it is not connected")
	end.

cut_link_to_neuron(ToNeuron, FromNeuronId, Generation) ->
	InputIdsPlusWeights = ToNeuron#neuron.input_ids_plus_weights,
	case lists:keymember(FromNeuronId, 1, InputIdsPlusWeights) of
		true ->
			UpdatedInputIdsPlusWeights = lists:keydelete(FromNeuronId, 1, InputIdsPlusWeights),
			ToNeuron#neuron{
				input_ids_plus_weights = UpdatedInputIdsPlusWeights,
				generation = Generation
			};
		false ->
			exit("******** ERROR: cut_link_to_neuron cannot remove from input as it is not connected")
	end.

cut_link_from_sensor(Sensor, NeuronId) ->
	FanoutIds = Sensor#sensor.fanout_ids,
	case lists:member(NeuronId, FanoutIds) of
		true ->
			Sensor#sensor{fanout_ids = FanoutIds -- [NeuronId]};
		false ->
			exit("******** ERROR: cut_link_from_sensor cannot remove from fanout as it is not connected")
	end.

cut_link_to_actuator(Actuator, NeuronId) ->
	FaninIds = Actuator#actuator.fanin_ids,
	case lists:member(NeuronId, FaninIds) of
		true ->
			Actuator#actuator{fanin_ids = FaninIds -- [NeuronId]};
		false ->
			exit("******** ERROR: cut_link_to_actuator cannot remove from fanin as it is not connected")
	end.
%% cut_link_from_sensor, cut_link_from_neuron, cut_link_to_neuron, cut_link_to_actuator

get_generation(AgentId) ->
	Agent = genotype:read({agent, AgentId}),
	Agent#agent.generation.

test_x() ->
	Result = mutate(test),
	case Result of
		{atomic, _} ->
			io:format("******** Mutation Succesful.~n");
		_ ->
			io:format("******** Mutation Failure: ~p~n", [Result])
	end.

test_x(AgentId, Mutator) ->
	F = fun() ->
		{M, Args} = lists:keyfind(Mutator, 1, ?MUTATORS),
		case Args of
			[] ->
				genome_mutator:M(AgentId);
			_ ->
				(apply(genome_mutator, M, Args))(AgentId)
		end,
		genotype:update_fingerprint(AgentId)
	end,
	mnesia:transaction(F).

long_test(N) when (N > 0) ->
	genotype:create_test_x(),
	short_test(N).

short_test(0) ->
	io:format("~nstarting exoself"),
	exoself:start(test, void);
short_test(N) ->
	test_x(),
	short_test(N-1).

