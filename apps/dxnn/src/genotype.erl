-module(genotype).
-compile(export_all).
-include("records.hrl").

%% doc The population montitor should have all the information with regards to the morphologies and species constraints under which the agent's genotype should be created. Thus construct_agent/3 is ran with the parameter SpeciesId to which this NN based system will belong, the AgentId that this NN based intelligent agent will have and the SpeciesConstraint that will define the list of activation functions and other parameters from which the seed agent can choose its parameters. In this function, first the generation is set to 0, since the agent is just created, then construct_cortex/3 is invoked, which creates the NN and returns itss CortexId. Ince the NN is created and the cortex's id is returned, we can fill out the information needed by the agent record and write it to the mnesia database.
construct_agent(SpeciesId, AgentId, SpeciesConstraint) ->
	random:seed(now()),
	Generation = 0,
	{CortexId, Pattern} = construct_cortex(AgentId, Generation, SpeciesConstraint),
	Agent = #agent{
		id = AgentId,
		cortex_id = CortexId,
		species_id = SpeciesId,
		constraint = SpeciesConstraint,
		generation = Generation,
		pattern = Pattern,
		evo_hist = []
	},
	write(Agent),
	update_fingerprint(AgentId).	 
	 
%% doc Generates a new CortexId, extracts tje morphology form the constraints record passed to it in SpeciesConstraints and then extracts the initial sensors and actuators from the morphology. After the sensors and actuators are extracted, the function calls construct_initial_neuro_layer/5, which creates a single layer of neurons connected from the specified sensors and to the specified actuators and then returns the ids of the created neurons. Finally the sensor and actuator ids are extracted and the cortex record is composed and written to the database.
construct_cortex(AgentId, Generation, SpeciesConstraint) ->
	CortexId = {{origin, generate_unique_id()}, cortex},
	Morphology = SpeciesConstraint#constraint.morphology,
	Sensors = [Sensor#sensor{id={{-1, generate_unique_id()}, sensor}, 
		cortex_id=CortexId} || Sensor <- morphology:get_init_sensors(Morphology)],
	Actuators = [Actuator#actuator{id={{1, generate_unique_id()}, actuator}, 
		cortex_id=CortexId} || Actuator <- morphology:get_init_actuators(Morphology)],
	NeuronIds = construct_initial_neuro_layer(CortexId, Generation, 
		SpeciesConstraint, Sensors, Actuators),
	SensorIds = [Sensor#sensor.id || Sensor <- Sensors],
	ActuatorIds = [Actuator#actuator.id || Actuator <- Actuators],
	Cortex = #cortex{
		id = CortexId,
		agent_id = AgentId,
		neuron_ids = NeuronIds,
		sensor_ids = SensorIds,
		actuator_ids = ActuatorIds
	},
	write(Cortex),
	{CortexId, [{0, NeuronIds}]}.

%% doc Creates a set of neurons for each actuator in the actuator list. The neurons are initialized in construct_initial_neurons/6 where they are connected to the actuator, and from a random subset of the sensors passed tp the function. construct_initial_neurons/6 returns the updated sensors. The actuator's fanin_ids is then updated to include the neuron ids that where connected to it. Once all the actuators have been connected, the sensors and actuators are written to the database and the set of newly created neuron ids is returned to the caller.
construct_initial_neuro_layer(CortexId, Generation, SpeciesConstraint, Sensors, Actuators) ->
	construct_initial_neuro_layer(CortexId, Generation, SpeciesConstraint, Sensors, Actuators, [], []).
construct_initial_neuro_layer(CortexId, Generation, SpeciesConstraint,Sensors, 
	[Actuator|Actuators], ActuatorsAcc, NeuronIdsAcc) ->
		NeuronIds = [{{0, Id}, neuron} || Id <- generate_unique_ids(Actuator#actuator.vl)],
		UpdatedSensors = construct_initial_neurons(CortexId, Generation, SpeciesConstraint, NeuronIds, 
			Sensors, Actuator),
		UpdatedActuator = Actuator#actuator{fanin_ids=NeuronIds},
		construct_initial_neuro_layer(CortexId, Generation, SpeciesConstraint, UpdatedSensors, Actuators, 
			[UpdatedActuator|ActuatorsAcc], lists:append(NeuronIds, NeuronIdsAcc));
construct_initial_neuro_layer(_CortexId, _Generation, _SpeciesConstraint, Sensors, [], 
	ActuatorsAcc, NeuronIdsAcc) ->
		[write(Sensor) || Sensor <- Sensors],
		[write(Actuator) || Actuator <- ActuatorsAcc],
		NeuronIdsAcc.

%% doc Accepts the list of sensors and a single actuator, conncets each neuron to the actuator and randomly chooses whether to connect it from all the sensors or a subset of the given sensors. Once all the neurons have been connected to the actuator and from the sensors, the updated sensors whose fanout_ids have been updated with the ids of the neruons are returned to the caller.
construct_initial_neurons(CortexId, Generation, SpeciesConstraint, [NeuronId|NeuronIds], Sensors, Actuator) ->
	case random:uniform() >= 0.5 of
		true ->
			Sensor = lists:nth(random:uniform(length(Sensors)), Sensors),
			UpdatedSensors = lists:keyreplace(Sensor#sensor.id, 2, Sensors, 
				Sensor#sensor{fanout_ids=[NeuronId|Sensor#sensor.fanout_ids]}),
			InputSpecs = [{Sensor#sensor.id, Sensor#sensor.vl}];
		false ->
			UpdatedSensors = [Sensor#sensor{fanout_ids=[NeuronId|Sensor#sensor.fanout_ids]} || Sensor <- Sensors],
			InputSpecs = [{Sensor#sensor.id, Sensor#sensor.vl} || Sensor <- Sensors]
	end,
	construct_neuron(CortexId, Generation, SpeciesConstraint, NeuronId, InputSpecs, [Actuator#actuator.id]),
	construct_initial_neurons(CortexId, Generation, SpeciesConstraint, NeuronIds, UpdatedSensors, Actuator);
construct_initial_neurons(_CortexId, _Generation, _SpeciesConstraint, [], Sensors, _Actuator) ->
	Sensors.

%% doc creates the input list from the tuples [{Id, Weights}...] using the vector lenght specified in the InputSpecs list. The activation function that the neuron uses is randomly chosen from the neural_afs list within the constraint record. cconstruct_neuron uses calculate_recursive_inputs/2 to extract the list of recursive connections from the OutputIds passed to it. Once the neuron record is filled in, it is saved to the database.
construct_neuron(CortexId, Generation, SpeciesConstraint, NeuronId, InputSpecs, OutputIds) ->
	InputIdsPlusWeights = create_input_id_plus_weight_tuples(InputSpecs),
	Neuron = #neuron{
		id = NeuronId,
		cortex_id = CortexId,
		generation = Generation,
		af = generate_activation_function(SpeciesConstraint#constraint.neural_afs),
		input_ids_plus_weights = InputIdsPlusWeights,
		output_ids = OutputIds,
		recursive_output_ids = calculate_recursive_output_ids(NeuronId, OutputIds)
	},
	write(Neuron).

create_input_id_plus_weight_tuples(InputSpecs) ->
	create_input_id_plus_weight_tuples(InputSpecs, []).
create_input_id_plus_weight_tuples([{InputId, InputVectorLength}|InputSpecs], Acc) ->
	Weights = create_neural_weights(InputVectorLength),
	create_input_id_plus_weight_tuples(InputSpecs, [{InputId, Weights}|Acc]);
create_input_id_plus_weight_tuples([], Acc) ->
	lists:reverse(Acc).

%% doc generates a list of random weights between -0.5 and 0.5
create_neural_weights(N) ->
	create_neural_weights(N, []).
create_neural_weights(0, Acc) ->
	lists:reverse(Acc);
create_neural_weights(N, Acc) ->
	create_neural_weights(N-1, [random:uniform()-0.5|Acc]).

%% doc accetps a list of activation function tags and returns a radomly chosen one. If the list is empty, tanh is returned as a default.
generate_activation_function(ActivationFunctions) ->
	case ActivationFunctions of
		[] ->
			tanh;
		Other ->
			lists:nth(random:uniform(length(Other)), Other)
	end.

calculate_recursive_output_ids(SelfId, [OutputId|OutputIds]) ->
	calculate_recursive_output_ids(SelfId, [OutputId|OutputIds], []).
calculate_recursive_output_ids(SelfId, [OutputId|OutputIds], Acc) ->
	case OutputId of
		{_, actuator} ->
			calculate_recursive_output_ids(SelfId, OutputIds, Acc);
		OutputId ->
			{{SelfLayerIndex, _}, _} = SelfId,
			{{OutputLayerIndex, _}, _} = OutputId,
			case SelfLayerIndex >= OutputLayerIndex of
				true ->
					calculate_recursive_output_ids(SelfId, OutputIds, [OutputId|Acc]);
				false ->
					calculate_recursive_output_ids(SelfId, OutputIds, Acc)
			end
	end;
calculate_recursive_output_ids(_SelfId, [], Acc) ->
	lists:reverse(Acc).

%% doc calculates the fingerprint of the agent, where the fingerprint is just a tuple of the various general features of the NN based system, a list of features that play some role in distinguishing it's genotype's general properties from those of other NN systems, Here, the fingerprint is composed of the generalized pattern (pattern minus the unique ids), the generalized evolution history (evolutionary history minis the unique ids of the elements), a generalized sensor set and a generalized actuator set of the agent in question.
update_fingerprint(AgentId) ->
	Agent = read({agent, AgentId}),
	Cortex = read({cortex, Agent#agent.cortex_id}),
	GeneralizedSensors = [(read({sensor, SensorId}))#sensor{id=undefined, cortex_id=undefined} 
		|| SensorId <- Cortex#cortex.sensor_ids],
	GeneralizedActuators = [(read({actuator, ActuatorId}))#actuator{id=undefined, cortex_id=undefined} 
		|| ActuatorId <- Cortex#cortex.actuator_ids],
	GeneralizedPattern = [{LayerIndex, length(LayerNeuronIds)} 
		|| {LayerIndex, LayerNeuronIds} <- Agent#agent.pattern],
	GeneralizedEvolutionaryHistory = generalize_evolutionary_history(Agent#agent.evo_hist),
	Fingerprint = {GeneralizedPattern, GeneralizedEvolutionaryHistory, GeneralizedSensors, GeneralizedActuators},
	write(Agent#agent{fingerprint = Fingerprint}).

%% doc generalizes the evolutionary history tuples by removing the unique element ids. Two neurons which are using exactly the same activation function, located in exactly the same layer and using exactly the same synaptic weights, will still have different unique ids. Thus these ids must be removed to produce a more general set of tuples. There are 3 types of tuples in the list, which 3, 2 and 1 element ids. The generalized history is returned to the caller.
generalize_evolutionary_history(History) ->
	generalize_evolutionary_history(History, []).
generalize_evolutionary_history(
	[{MutationOperator, 
	 {{LayerIndex1, _}, NodeType1}, 
	 {{LayerIndex2, _}, NodeType2}, 
	 {{LayerIndex3, _}, NodeType3}}|History],  
	Acc) ->
		generalize_evolutionary_history(
			History,
			[{MutationOperator, 
			 {LayerIndex1, NodeType1}, 
			 {LayerIndex2, NodeType2}, 
			 {LayerIndex3, NodeType3}}|Acc]);
generalize_evolutionary_history(
	[{MutationOperator, 
	 {{LayerIndex1, _}, NodeType1}, 
	 {{LayerIndex2, _}, NodeType2}}|History],
	Acc) ->
		generalize_evolutionary_history(
			History,
			[{MutationOperator, 
			 {LayerIndex1, NodeType1}, 
			 {LayerIndex2, NodeType2}}|Acc]);
generalize_evolutionary_history(
	[{MutationOperator, 
	 {{LayerIndex1, _}, NodeType1}}|History],
	Acc) ->
		generalize_evolutionary_history(
			History, 
			[{MutationOperator, 
			 {LayerIndex1, NodeType1}}|Acc]); 
generalize_evolutionary_history([], Acc) ->
	lists:reverse(Acc).

%% doc accepts the id of an agent and then deletes that agent's genotype. This function assumes that the id of the agent will be removed from the specie's agent_ids list, and any other needed clean up procedure will be performed by the calling function.
delete_agent(AgentId) ->
	Agent = read({agent, AgentId}),
	Cortex = read({cortex, Agent#agent.cortex_id}),
	[delete(sensor, Id) || Id  <- Cortex#cortex.sensor_ids],
	[delete(neuron, Id) || Id  <- Cortex#cortex.neuron_ids],
	[delete(actuator, Id) || Id  <- Cortex#cortex.actuator_ids],
	delete(cortex, Agent#agent.cortex_id),
	delete(agent, AgentId).

%% doc accepts the id of an agent, and then deletes that agent's genotype but ensures that the species to which the agent belongs has its agent_ids element updated. 
delete_agent(AgentId, safe) ->
	F = fun() ->
		Agent = read({agent, AgentId}),
		Species = read({species, Agent#agent.species_id}),
		AgentIds = Species#species.agent_ids,
		write(Species#species{agent_ids=lists:delete(AgentId, AgentIds)}),
		delete_agent(AgentId)
	end,
	Result = mnesia:transaction(F),
	io:format("delete_agent(AgentId, safe):~p Result:~p~n", [AgentId, Result]).

%% doc accepts an AgentId and CloneId as parameters and then clones the agent. The function first creates an ETS table to which it writes the ids of all the elements of the genotype and their correspondingly generated clone ids. Once all ids an clone ids have been generated, the function begins to clone the actual elements. clone_agent/2 first clones neurons, then sensors and finally the actuators. Once these elements are cloned, the function writes to the database the cloned versions oft he cortex and the agent,
clone_agent(AgentId, CloneId) ->
	F = fun() ->
		Agent = read({agent, AgentId}),
		Cortex = read({cortex, Agent#agent.cortex_id}),
		IdsAndCloneIds = ets:new(idsAndCloneIds, [set, private]),
		ets:insert(IdsAndCloneIds, {threshold, threshold}),
		ets:insert(IdsAndCloneIds, {AgentId, CloneId}),
		[CloneCortexId] = map_ids(IdsAndCloneIds, [Agent#agent.cortex_id]),
		CloneSensorIds = map_ids(IdsAndCloneIds, Cortex#cortex.sensor_ids),
		CloneNeuronIds = map_ids(IdsAndCloneIds, Cortex#cortex.neuron_ids),
		CloneActuatorIds = map_ids(IdsAndCloneIds, Cortex#cortex.actuator_ids),
		clone_neurons(IdsAndCloneIds, Cortex#cortex.neuron_ids),
		clone_sensors(IdsAndCloneIds, Cortex#cortex.sensor_ids),
		clone_actuators(IdsAndCloneIds, Cortex#cortex.actuator_ids),

		write(Cortex#cortex{
			id = CloneCortexId,
			agent_id = CloneId,
			sensor_ids = CloneSensorIds,
			neuron_ids = CloneNeuronIds,
			actuator_ids = CloneActuatorIds
		}),
		write(Agent#agent{
			id = CloneId,
			cortex_id = CloneCortexId
		}),
		ets:delete(IdsAndCloneIds)
	end,
	mnesia:transaction(F).

map_ids(IdsAndCloneIds, Ids) ->
	map_ids(IdsAndCloneIds, Ids, []).
map_ids(IdsAndCloneIds, [Id|Ids], Acc) ->
	CloneId = case Id of
		{{LayerIndex, _}, NodeType} -> % maps neuron ids and cortex ids
			{{LayerIndex, generate_unique_id()}, NodeType}; 
		{_, NodeType} -> % maps sensors and actuators
			{generate_unique_id(), NodeType}
	end,
	ets:insert(IdsAndCloneIds, {Id, CloneId}),
	map_ids(IdsAndCloneIds, Ids, [CloneId|Acc]);
map_ids(_IdsAndCloneIds, [], Acc) ->
	lists:reverse(Acc).

%% doc accepts as the input the name of the ets table and the list of sensor ids. It then goes throught all the ids, reads the sensor from the database and updates all its ids from their original values to their clone values stored in the ets table. Afterwards the new version of the sensor is written to the database, effectively cloning the original sernsor.
clone_sensors(IdsAndCloneIds, [SensorId|SensorIds]) ->
	Sensor = read({sensor, SensorId}),
	CloneSensorId = ets:lookup_element(IdsAndCloneIds, SensorId, 2),
	CloneCortexId = ets:lookup_element(IdsAndCloneIds, Sensor#sensor.cortex_id, 2),
	CloneFanoutIds = [ets:lookup_element(IdsAndCloneIds, FanoutId, 2) || FanoutId <- Sensor#sensor.fanout_ids],
	write(Sensor#sensor{
		id = CloneSensorId,
		cortex_id = CloneCortexId,
		fanout_ids = CloneFanoutIds
	}),
	clone_sensors(IdsAndCloneIds, SensorIds);
clone_sensors(_IdsAndCloneIds, []) ->
	done.

%% doc accepts as the input the name of the ets table and the list of sensor ids. It then goes throught all the ids, reads the actuator from the database and updates all its ids from their original values to their clone values stored in the ets table. Afterwards the new version of the actuator is written to the database, effectively cloning the original actuator.
clone_actuators(IdsAndCloneIds, [ActuatorId|ActuatorIds]) ->
	Actuator = read({actuator, ActuatorId}),
	CloneActuatorId = ets:lookup_element(IdsAndCloneIds, ActuatorId, 2),
	CloneCortexId = ets:lookup_element(IdsAndCloneIds, Actuator#actuator.cortex_id, 2),
	CloneFaninIds = [ets:lookup_element(IdsAndCloneIds, FaninId, 2) || FaninId <- Actuator#actuator.fanin_ids],
	write(Actuator#actuator{
		id = CloneActuatorId,
		cortex_id = CloneCortexId,
		fanin_ids = CloneFaninIds
	}),
	clone_actuators(IdsAndCloneIds, ActuatorIds);
clone_actuators(_IdsAndCloneIds, []) ->
	done.

clone_neurons(IdsAndCloneIds, [NeuronId|NeuronIds]) ->
	Neuron = read({neuron, NeuronId}),
	CloneNeuronId = ets:lookup_element(IdsAndCloneIds, NeuronId, 2),
	CloneCortexId = ets:lookup_element(IdsAndCloneIds, Neuron#neuron.cortex_id, 2),
	CloneInputIdsPlusWeights = [{ets:lookup_element(IdsAndCloneIds, Id, 2), Weights} 
		|| {Id, Weights} <- Neuron#neuron.input_ids_plus_weights],
	CloneOutputIds = [ets:lookup_element(IdsAndCloneIds, Id, 2) || Id <- Neuron#neuron.output_ids],
	CloneRecursiveOutputIds = 
		[ets:lookup_element(IdsAndCloneIds, Id, 2) || Id <- Neuron#neuron.recursive_output_ids],
	write(Neuron#neuron{
		id = CloneNeuronId,
		cortex_id = CloneCortexId,
		input_ids_plus_weights = CloneInputIdsPlusWeights,
		output_ids = CloneOutputIds,
		recursive_output_ids = CloneRecursiveOutputIds
	}),
	clone_neurons(IdsAndCloneIds, NeuronIds);
clone_neurons(_IdsAndCloneIds, []) ->
	done.

%% returns all nn related records based on the given AgentId
read_nn(AgentId) ->
	Agent = read({agent, AgentId}),
	Cortex = read({cortex, Agent#agent.cortex_id}),
	Sensors = [read({sensor, Id}) || Id <- Cortex#cortex.sensor_ids],
	Neurons = [read({neuron, Id}) || Id <- Cortex#cortex.neuron_ids],
	Actuators = [read({actuator, Id}) || Id <- Cortex#cortex.actuator_ids],
	{Agent, Cortex, Sensors, Neurons, Actuators}.

%% doc reads the key from the given mnesia table
read(TableAndKey) ->
	case mnesia:read(TableAndKey) of
		[] ->
			undefined;
		[Record] ->
			Record
	end.

%% doc accepts a record and writes it to the database
write(Record) ->
	mnesia:write(Record).

%% doc deletes the key from the given table
delete(Table, Key) ->
	mnesia:delete({Table, Key}).

print(AgentId) ->
	{Agent, Cortex, Sensors, Neurons, Actuators} = read_nn(AgentId),
	io:format("~p~n", [Agent]),
	io:format("~p~n", [Cortex]),
	[io:format("~p~n", [Sensor]) || Sensor <- Sensors],
	[io:format("~p~n", [Neuron]) || Neuron <- Neurons],
	[io:format("~p~n", [Actuator]) || Actuator <- Actuators].

construct(Morphology, HiddenLayerDensities) ->
	construct(ffnn, Morphology, HiddenLayerDensities).
construct(FileName, Morphology, HiddenLayerDensities) ->
	{V1, V2, V3} = now(),
	random:seed(V1, V2, V3),
	[S] = morphology:get_init_sensors(Morphology),
	[A] = morphology:get_init_actuators(Morphology),
	Output_VL = A#actuator.vl,
	LayerDensities = lists:append(HiddenLayerDensities,[Output_VL]),
	Cx_Id = cortex,

	Neurons = create_NeuroLayers(Cx_Id,S,A,LayerDensities),
	[InputLayer|_] = Neurons,
	[OutputLayer|_] = lists:reverse(Neurons),
	FL_NIds = [N#neuron.id || N <- InputLayer],
	LL_NIds = [N#neuron.id || N <- OutputLayer],
	NIds = [N#neuron.id || N <- lists:flatten(Neurons)],
	Sensor = S#sensor{cortex_id = Cx_Id, fanout_ids = FL_NIds},
	Actuator = A#actuator{cortex_id = Cx_Id, fanin_ids = LL_NIds},
	Cortex = create_Cortex(Cx_Id,[S#sensor.id],[A#actuator.id],NIds),
	Genotype = lists:flatten([Cortex,Sensor,Actuator|Neurons]),
	save_genotype(FileName,Genotype),
	Genotype.

%The construct_Genotype function accepts the name of the file to which we'll save the genotype, sensor name, actuator name and the hidden layer density parameters. We haev to generate unique Ids for every sensor and actuator. The sensor and actuator names are used as input to the create_Sensor and create_Actuator functions, which in turn generate the actual Sensor and Actuator representing tuples. We create unique Ids for sensors and actuators so that when in the future a NN uses 2 or more sensors or actuators of the same type, we will be able to differentiate between them using their Ids. After the Sensor and Actuator tuples are generated, we extract the NN's input and output vector lenghts from the sensor and actuator used by the system. The Input_VL is then used to specify how many weights the neurons in the input layer will need, and the Output_VL specifies how many neurons are in the output layer of the NN. After appending the HiddenLayerDensities to the now known number of neurons in the last layer to generate the fill LayerDensities list, we use the create_NeuroLayers function to generate the Neuron representing tuples. We then update the Sensor and Actuator records with proper fanin and fanout ids from the freshly created Neuron tuples, compose the Cortex, and write the genotype to file.

	create_Sensor(SensorName) ->
		case SensorName of
			rng ->
				#sensor{id={sensor,generate_id()},name=rng,vl=2};
			_ ->
				exit("System does not yet support a sensor by the name:~p",[SensorName])
		end.

	create_Actuator(ActuatorName) ->
		case ActuatorName of
			pts ->
				#actuator{id={actuator,generate_id()},name=pts,vl=1};
			_ ->
				exit("System does not yest support and actuator by the name:~p",[ActuatorName])
		end.
%Every sensor and actuator uses some kind of function associated with it, a function that either polls the environment for sensory signals (in the case of a sensor) or acts upon the environment (in the case of an actuator). It is the function that we need to define and program before it is used, and the name of the function is the same as the name of the sensor or actuator itself. For example, the create_Sensor/1 has specified only the rng sensor because that is the only sensor function we've finished developing. The rng function has it's on vl specification, whilc will determine the number of weights that a neuron will need to allocatie if it is to accept this sensor's output vector. The same principples apply to the create_Actuator function. Both, create_Sensor and create_Actuator function, given the name of the sensor or actuator, will return a record with all the specifications of that element, each with its own unique Id.

	create_NeuroLayers(Cx_Id,Sensor,Actuator,LayerDensities) ->
		Input_IdPs = [{Sensor#sensor.id,Sensor#sensor.vl}],
		Tot_Layers = length(LayerDensities),
		[FL_Neurons|Next_LDs] = LayerDensities,
		NIds = [{neuron, {1,Id}} || Id <- generate_ids(FL_Neurons,[])],
		create_NeuroLayers(Cx_Id,Actuator#actuator.id,1,Tot_Layers,Input_IdPs,NIds,Next_LDs,[]).
%The function create_NeuroLayers/4 preoares the initial step before starting the recursive create_NeuroLayers/7 function which will create all the Neuron records. We first generate the place holder Input Ids "Plus" (Input_IdPs), which are tuples composed of the Ids and the vector lengths of the incoming signals associated with them. The proper input_idps will have a weight list in the tuple instead of the vector length. Because we are only building the NNs each with only a single Sensor and Actuator, the IdP to the first layer is composed of the single Sensor Id with the vector lenght of its sensory signal, likewise in the case of the Actuator. We then generate unique ids for the neurons in the first layer, and drop into the recursive create_NeuroLayers/7 function.

	create_NeuroLayers(Cx_Id,Actuator_Id,LayerIndex,Tot_Layers,Input_IdPs,NIds,[Next_LD|LDs],Acc) ->
		Output_NIds = [{neuron,{LayerIndex+1,Id}} || Id <- generate_ids(Next_LD,[])],
		Layer_Neurons = create_NeuroLayer(Cx_Id,Input_IdPs,NIds,Output_NIds,[]),
		Next_InputIdPs = [{NId,1} || NId <- NIds],
		create_NeuroLayers(Cx_Id,Actuator_Id,LayerIndex+1,Tot_Layers,Next_InputIdPs,Output_NIds,LDs,[Layer_Neurons|Acc]);
	create_NeuroLayers(Cx_Id,Actuator_Id,Tot_Layers,Tot_Layers,Input_IdPs,NIds,[],Acc) ->
		Output_Ids = [Actuator_Id],
		Layer_Neurons = create_NeuroLayer(Cx_Id,Input_IdPs,NIds,Output_Ids,[]),
		lists:reverse([Layer_Neurons|Acc]).
%During the first iteration, the first layer neuron ids constructed in create_NeuroLayers/4 are held in the NIds variable. In create_NeuroLayers/8, with every iteration we generate the Output_NIds, which are teh Ids of the neurons in the next layer. The last layer is a special case which occurs when LayerIndex == Tot_Layers. Having the Input_IdPs, and the Output_NIds, we are able to construct a neuron record for every Id in NIds using the function create_NeuroLayer/5. The Ids of the constructed Output_NIds will become the NIds variable of the next iteration, and the Ids of the neurons in the current layer will be extended and become Next_InputIdPs. We then drop into the next iteration with the newly prepared Next_InputIdPs and Output_NIds. Finally, when we reach the last layer, the Output_Ids is the list containing a single Id of the Actuator element. We use the same function, create_NeuroLayer/5, to construct the last layer and return the result.

	create_NeuroLayer(Cx_Id,Input_IdPs,[Id|NIds],Output_Ids,Acc) ->
		Neuron = create_Neuron(Input_IdPs,Id,Cx_Id,Output_Ids),
		create_NeuroLayer(Cx_Id,Input_IdPs,NIds,Output_Ids,[Neuron|Acc]);
	create_NeuroLayer(_Cx_Id,_Input_IdPs,[],_Output_Ids,Acc) ->
		Acc.
%To create neurons from the same layer, all that is needed are the Ids for those neurons, a list of Input_IdPs for every neuron so that we can create the proper number of weights, and a list of Output_Ids. Since in our simple feed forward neural network all neurons are fully connected to the neurons in the next layer, the Input_IdPs and Output_Ids are the same for every neuron belonging to the same layer.

	create_Neuron(Input_IdPs,Id,Cx_Id,Output_Ids) ->
		Proper_InputIdPs = create_NeuralInput(Input_IdPs,[]),
		#neuron{id=Id,cortex_id=Cx_Id,af=tanh,input_ids_plus_weights=Proper_InputIdPs,output_ids=Output_Ids}.
	
	create_NeuralInput([{Input_Id, Input_VL}|Input_IdPs],Acc) ->
		Weights = create_NeuralWeights(Input_VL,[]),
		create_NeuralInput(Input_IdPs,[{Input_Id,Weights}|Acc]);
	create_NeuralInput([],Acc) ->
		lists:reverse([{bias,random:uniform()-0.5}|Acc]).

	create_NeuralWeights(0,Acc) ->
		Acc;
	create_NeuralWeights(Index,Acc) ->
		W = random:uniform()-0.5,
		create_NeuralWeights(Index-1,[W|Acc]).
%Each neuron record is composed by the create_Neuron/4 function. The create_Neuron/4 function creates the Input list from the tuples [{Id,Weights}...] using the vector lengths specified in the place holder Input_IdPs. The create_NeuralInput/2 function then uses create_NeuralWeights/2 to generate the random weights in the range of -0.5 ro 0.5, adding the bias to the end of the list.

	generate_unique_ids(N) ->
		generate_unique_ids(N, []).
	generate_unique_ids(0, Acc) ->
		lists:reverse(Acc);
	generate_unique_ids(N, Acc) ->
		generate_unique_ids(N-1, [generate_unique_id()|Acc]).

	generate_unique_id() ->
		generate_id(). % no clue why this needs to be renamed.. going with the flow..

	generate_ids(0,Acc) ->
		Acc;
	generate_ids(Index,Acc) ->
		Id = generate_id(),
		generate_ids(Index-1,[Id|Acc]).

	generate_id() ->
		{MegaSeconds,Seconds,MicroSeconds} = now(),
		1/(MegaSeconds*1000000 + Seconds + MicroSeconds/1000000).
%The generate_id()/0 creates a unique id using current time, the Id is a floating point value The generate_ids/2 function creates a list of unique Ids.

	create_Cortex(Cx_Id,S_Ids,A_Ids,NIds) ->
		#cortex{id=Cx_Id,sensor_ids=S_Ids,actuator_ids=A_Ids,neuron_ids=NIds}.
%The create_Cortex/4 function creates the record encoded genotypical representation of the cortex element. The Cortex element needs to know the Id of every Neuron, Sensor and Actuator in the NN.

	save_genotype(FileName,Genotype)->
		TId = ets:new(FileName, [public,set,{keypos,2}]),
		[ets:insert(TId,Element) || Element <- Genotype],
		ets:tab2file(TId,FileName).
%The save_genotype/2 function expects that the Genotype is a list composed of the neuron, sensor, actuator, cortex and exoself elements. The function creates a new ets table, writes all the element representing tuples from the Genotype list to the ets table, and then writes the ets table to file.

	save_to_file(Genotype,FileName) ->
		ets:tab2file(Genotype,FileName).
%The save_to_file/2 function saves the ets table bu the name Genotype to the file by the name FileName.

	load_from_file(FileName) ->
		{ok,TId} = ets:file2tab(FileName),
		TId.
%The load_form_file/1 loads an ets representing file by the name FileName, returning the ets table id to the caller.

	read(TId,Key) ->
		[R] = ets:lookup(TId,Key),
		R.
%The read/2 function reads a record associated with Key from the ets table with the id TId, returning the record R to the caller. It expects that only a single record exists with the specified Key.

	write(TId,R) ->
		ets:insert(TId,R).
%The function write/2 writes the record R to the ets table with the id TId.

	print(FileName, obsolete) ->
		Genotype = load_from_file(FileName),
		Cx = read(Genotype,cortex),
		SIds = Cx#cortex.sensor_ids,
		NIds = Cx#cortex.neuron_ids,
		AIds = Cx#cortex.actuator_ids,
		io:format("~p~n",[Cx]),
		[io:format("~p~n",[read(Genotype,Id)]) || Id <- SIds],
		[io:format("~p~n",[read(Genotype,Id)]) || Id <- NIds],
		[io:format("~p~n",[read(Genotype,Id)]) || Id <- AIds].
%The function print/1 reads a stored Genotype from the file FileName, and then prints to console all the elements making up the NN's genotype.


test_x() ->
	SpecieId = test,
	AgentId = test,
	CloneAgentId = test_clone,
	SpeciesConstraint = #constraint{},
	F = fun() ->
		construct_agent(AgentId, SpecieId, SpeciesConstraint),
		clone_agent(SpecieId, CloneAgentId),
		print(AgentId),
		print(CloneAgentId),
		delete_agent(AgentId),
		delete_agent(CloneAgentId)
	end,
	mnesia:transaction(F).

create_test_x() ->
	SpecieId = test,
	AgentId = test,
	SpeciesConstraint = #constraint{},
	F = fun() ->
		case read({agent, test}) of
			undefined ->
				construct_agent(SpecieId, AgentId, SpeciesConstraint),
				print(AgentId);
			_ ->
				delete_agent(AgentId),
				construct_agent(SpecieId, AgentId, SpeciesConstraint),
				print(AgentId)
			end
	end,
	mnesia:transaction(F).	
