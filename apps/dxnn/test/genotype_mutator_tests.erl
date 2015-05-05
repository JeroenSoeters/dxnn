-module(genotype_mutator_tests).
-compile(export_all).
-include_lib("eunit/include/eunit.hrl").
-include("../src/records.hrl").
-define(AGENT, test_agent).
-define(CORTEX, {{origin,10},cortex}). 
-define(SENSOR, {{-1, 20}, sensor}).
-define(ACTUATOR, {{1, 30}, actuator}).
-define(A, {{-0.5, 40}, neuron}).
-define(B, {{-0.5, 50}, neuron}).
-define(C, {{0, 60}, neuron}).
-define(D, {{0.5, 70}, neuron}).

genome_mutator_test_() ->
	{foreach,
	 fun setup/0,
	 fun teardown/1,
	 [fun ?MODULE:mutate_weights_test_/1,
	  fun ?MODULE:add_bias_test_/1,
	  fun ?MODULE:remove_bias_test_/1,
	  fun ?MODULE:mutate_af_test_/1,
	  fun ?MODULE:add_outlink_to_neuron_test_/1,
	  fun ?MODULE:add_outlink_to_actuator_test_/1,
	  fun ?MODULE:add_inlink_from_neuron_test_/1,
	  fun ?MODULE:add_sensorlink_test_/1,
	  fun ?MODULE:add_actuatorlink_test_/1,
	  fun ?MODULE:add_neuron_test_/1,
	  fun ?MODULE:outsplice_test_/1,
	  fun ?MODULE:add_sensor_test_/1,
	  fun ?MODULE:create_link_between_neurons_test_/1,
	  fun ?MODULE:create_link_between_sensor_and_neuron_test_/1,
	  fun ?MODULE:create_link_between_neuron_and_actuator_test_/1,
	  fun ?MODULE:cut_link_between_neurons_test_/1,
	  fun ?MODULE:cut_link_between_sensor_and_neuron_test_/1,
	  fun ?MODULE:cut_link_between_neuron_and_actuator_test_/1]}.

%% ===================================================================
%% Setup and teardown
%% ===================================================================

setup() ->
	%email_address:is_valid("jsoeters@thoughtworks.com"),

	case whereis(polis) of
		undefined ->
			mnesia:start(),
			polis:start();
		_ -> 
			polis:reset()
	end,
	F = fun() ->
		[mnesia:write(R) || R <- create_test_genotype()]
	end,
	mnesia:transaction(F),
	case whereis(random_meck) of
		undefined ->
			meck:new(random, [unstick]);
		_ ->
			meck:unload(random),
			meck:new(random, [unstick])
	end.

teardown(_) ->
	polis:stop(),
	meck:unload(random).

%% ===================================================================
%% Mutation operators
%% ===================================================================

mutate_weights_test_(_) ->
	% This will select neuron C from our cortex since we have 4 neurons.. Neuron Ci has 2 weights.
	% MP = 1/sqrt(2) is 0.7. We thus expect both weights to be changed as 1 > 0.7.
	meck:sequence(random, uniform, 1, [3]),
	meck:sequence(random, uniform, 0, [0.4, 0.6]),

	in_transaction(fun() ->	genotype_mutator:mutate_weights(?AGENT) end),	
	
	NeuronC = find_neuron(?C),
	Agent = find_agent(?AGENT),
	[LastMutation|_] = Agent#agent.evo_hist,

	[?_assert(lists:nth(1, NeuronC#neuron.input_ids_plus_weights) =/= 
		lists:nth(1, (lists:nth(6, create_test_genotype()))#neuron.input_ids_plus_weights)),	
	?_assert(lists:nth(2, NeuronC#neuron.input_ids_plus_weights) =/= 
		lists:nth(2, (lists:nth(6, create_test_genotype()))#neuron.input_ids_plus_weights)),
	?_assertEqual({mutate_weights, ?C}, LastMutation)].

add_bias_test_(_) ->
	meck:sequence(random, uniform, 1, [1]),
	meck:sequence(random, uniform, 0, [0.9]),

	in_transaction(fun() -> genotype_mutator:add_bias(?AGENT) end),

	NeuronA = find_neuron(?A),
	Agent = find_agent(?AGENT),
	[LastMutation|_] = Agent#agent.evo_hist,

	[?_assertEqual({bias, 0.4}, lists:last(NeuronA#neuron.input_ids_plus_weights)),
	?_assertEqual({add_bias, ?A}, LastMutation)].

remove_bias_test_(_) ->
	meck:sequence(random, uniform, 1, [4]),

	in_transaction(fun() -> genotype_mutator:remove_bias(?AGENT) end),

	NeuronD = find_neuron(?D),
	Agent = find_agent(?AGENT),
	[LastMutation|_] = Agent#agent.evo_hist,

	[?_assertNot(lists:keymember(bias, 1, NeuronD#neuron.input_ids_plus_weights)),
	?_assertEqual({remove_bias, ?D}, LastMutation)].

mutate_af_test_(_) ->
	% As we have 4 neurons so far and 4-1 = 3 neural afs to choose from we mock RandomInt to 
	% return 2 which results in changing the AF of neuron b to cos (as defined in records.hrl)
	% this test breaks as we add more activation functions in records.hrl.
	meck:sequence(random, uniform, 1, [2]),
	
	in_transaction(fun() -> genotype_mutator:mutate_af(?AGENT) end),

	NeuronB = find_neuron(?B),
	Agent = find_agent(?AGENT),	
	[LastMutation|_] = Agent#agent.evo_hist,

	[?_assertEqual(cos, NeuronB#neuron.af),	
	?_assertEqual({mutate_af, ?B}, LastMutation)].

add_outlink_to_neuron_test_(_) ->
	% We will connect neuron b to neuron d. The first call to random:uniform/1 returns 2 because b is
	% the second element, d is then the 3rd as c is removed from the list as it's already connected.
	meck:sequence(random, uniform, 1, [2, 3]),
	meck:sequence(random, uniform, 0, [0.2, 0.3]),

	in_transaction(fun() -> genotype_mutator:add_outlink(?AGENT) end),

	NeuronB = find_neuron(?B),
	NeuronD = find_neuron(?D),
	Agent = find_agent(?AGENT),
	[LastMutation|_] = Agent#agent.evo_hist,

	[?_assert(lists:member(?D, NeuronB#neuron.output_ids)),
	?_assert(lists:keymember(?B, 1, NeuronD#neuron.input_ids_plus_weights)),
	?_assertEqual({add_outlink, ?B, ?D}, LastMutation)].

add_outlink_to_actuator_test_(_) ->
	% We will connect neuron b to neuron d. The first call to random:uniform/1 returns 2 because b is
	% the second element, actuator is then the 4rd as c is removed from the list as it's already connected.
	meck:sequence(random, uniform, 1, [2, 4]),

	in_transaction(fun() -> genotype_mutator:add_outlink(?AGENT) end),

	NeuronB = find_neuron(?B),
	Actuator = find_actuator(?ACTUATOR),
	Agent = find_agent(?AGENT),
	[LastMutation|_] = Agent#agent.evo_hist,

	[?_assert(lists:member(?ACTUATOR, NeuronB#neuron.output_ids)),
	?_assert(lists:member(?B, Actuator#actuator.fanin_ids)),
	?_assertEqual({add_outlink, ?B, ?ACTUATOR}, LastMutation)].
	
add_inlink_from_neuron_test_(_) ->
	% First we select neuron a with index one. We then connect it to the  fourth index of the list
    % (sensor ids ++ neuron ids) -- input ids which is [a, b, c, d] so neuron d.
	meck:sequence(random, uniform, 1, [1, 4]),
	meck:sequence(random, uniform, 0, [0.4, 0.9]),

	in_transaction(fun() -> genotype_mutator:add_inlink(?AGENT) end),

	NeuronA = find_neuron(?A),
	NeuronD = find_neuron(?D),
	Agent = find_agent(?AGENT),
	[LastMutation|_] = Agent#agent.evo_hist,

	[?_assert(lists:member(?A, NeuronD#neuron.output_ids)),
	 ?_assert(lists:keymember(?D, 1, NeuronA#neuron.input_ids_plus_weights)),
	 ?_assertEqual({add_inlink, ?D, ?A}, LastMutation)].

add_inlink_from_sensor_test_(_) ->
	% First we select neuron d with index 4. We then connect it to the  first index of the list
    % (sensor ids ++ neuron ids) -- input ids which is [sensor, a, b, d] so the sensor.
	meck:sequence(random, uniform, 1, [4, 1]),
	meck:sequence(random, uniform, 0, [0.4, 0.9]),

	in_transaction(fun() -> genotype_mutator:add_inlink(?AGENT) end),

	Sensor = find_sensor(?SENSOR),
	NeuronD = find_neuron(?D),
	Agent = find_agent(?AGENT),
	[LastMutation|_] = Agent#agent.evo_hist,

	[?_assert(lists:member(?SENSOR, Sensor#sensor.fanout_ids)),
	 ?_assert(lists:keymember(?SENSOR, 1, NeuronD#neuron.input_ids_plus_weights)),
	 ?_assertEqual({add_inlink, ?SENSOR, ?D}, LastMutation)].

add_sensorlink_test_(_) ->
	% We connect the first sensor from the available list of sensors (there is only one) to the
	% first neuron from the available list of neurons [c, d] so neuron c.
	meck:sequence(random, uniform, 1, [1, 1]),
	meck:sequence(random, uniform, 0, [0.5, 0.6]),
	
	in_transaction(fun() -> genotype_mutator:add_sensorlink(?AGENT) end),

	Sensor = find_sensor(?SENSOR),
	NeuronC = find_neuron(?C),
	Agent = find_agent(?AGENT),
	[LastMutation|_] = Agent#agent.evo_hist,	

	[?_assert(lists:member(?C, Sensor#sensor.fanout_ids)),
	 ?_assert(lists:keymember(?SENSOR, 1, NeuronC#neuron.input_ids_plus_weights)),
	 ?_assertEqual({add_sensorlink, ?SENSOR, ?C}, LastMutation)].

add_actuatorlink_test_(_) ->
	% We connect the first actuator from the list (there is only one) to the first nueron of
	% the list of available neurons [a, b, c] which is neuron a.
	meck:sequence(random, uniform, 1, [1, 1]),

	in_transaction(fun() -> genotype_mutator:add_actuatorlink(?AGENT) end),
	
	NeuronA = find_neuron(?A),
	Actuator = find_actuator(?ACTUATOR),
	Agent = find_agent(?AGENT),
	[LastMutation|_] = Agent#agent.evo_hist,

	[?_assert(lists:member(?ACTUATOR, NeuronA#neuron.output_ids)),
	 ?_assert(lists:member(?A, Actuator#actuator.fanin_ids)),
	 ?_assertEqual({add_actuatorlink, ?A, ?ACTUATOR}, LastMutation)].

add_neuron_test_(_) ->
	LayerIndex = 0.5,
	Id = 80.0,
	NewNeuronId = {{LayerIndex, Id}, neuron},

	% The sequence of calls to random:uniform is as follows, the first call is to
	% select the layer where the new neuron will be added. We select the third (0.5).
	% The second call is construct_neuron selecting an activation function, we return the first.
	% The third call will be to select the presynaptic element from the available
	% sensors and neurons [sensor, a, b, c, d]. We return 2 (neuron a) and the last call will select
	% the postsynaptic element, for which we return 3 (neuron c).
	meck:sequence(random, uniform, 1, [3, 1, 2, 3]),
	% The first call to random:uniform/0 is made by generate_id to create a unique id
	% for the newly created neuron. The other calls are for pre- and postsynaptic weights.
	meck:sequence(random, uniform, 0, [0.6, 0.6]),

	% Will result in a generated unique id of 80
	FakeTimeProvider = fun() -> {0, 0.0125, 0} end,

	in_transaction(fun() -> genotype_mutator:add_neuron(?AGENT, FakeTimeProvider) end),
	
	NewNeuron = find_neuron(NewNeuronId),

	Agent = find_agent(?AGENT),
	{LayerIndex, NeuronsInLayer} =  lists:keyfind(LayerIndex, 1, Agent#agent.pattern),
	[LastMutation|_] = Agent#agent.evo_hist,
	Cortex = find_cortex(?CORTEX),

	[?_assertNot(NewNeuron == undefined),
	 ?_assert(lists:keymember(?A, 1, NewNeuron#neuron.input_ids_plus_weights)),
	 ?_assert(lists:member(?C, NewNeuron#neuron.output_ids)),
	 ?_assert(lists:member(NewNeuronId, NeuronsInLayer)),
	 ?_assert(lists:member(NewNeuronId, Cortex#cortex.neuron_ids)),
	 ?_assertEqual({add_neuron, ?A, NewNeuronId, ?C}, LastMutation)].

outsplice_test_(_) ->
	NewNeuronId = {{0.25, 80.0}, neuron},

	% The first result in the sequence is the index of the neuron that we're gonna connect from.
	% The second is the index in it's output ids (there is only one) where we are going to connect to.
	% The third result is the index of the random activation function for the new neuron.
	meck:sequence(random, uniform, 1, [3, 1, 1]),
	
	meck:sequence(random, uniform, 0, [0.6, 0.6]),
	
	FakeTimeProvider = fun() -> {0, 0.0125, 0} end,

	in_transaction(fun() -> genotype_mutator:outsplice(?AGENT, FakeTimeProvider) end),

	NewNeuron = find_neuron(NewNeuronId),
	NeuronC = find_neuron(?C),
	NeuronD = find_neuron(?D),
	Agent = find_agent(?AGENT),
	[LastMutation|_] = Agent#agent.evo_hist,
	Cortex = find_cortex(?CORTEX),

	[?_assert(lists:keymember(?C, 1, NewNeuron#neuron.input_ids_plus_weights)),
	 ?_assert(lists:member(?D, NewNeuron#neuron.output_ids)),
	 ?_assert(lists:member(NewNeuronId, NeuronC#neuron.output_ids)),
	 ?_assert(lists:keymember(NewNeuronId, 1, NeuronD#neuron.input_ids_plus_weights)),
	 ?_assertEqual({outsplice, ?C, NewNeuronId, ?D}, LastMutation),
	 ?_assert(lists:member({0.25, [NewNeuronId]}, Agent#agent.pattern)),
	 ?_assert(lists:member(NewNeuronId, Cortex#cortex.neuron_ids))].

add_sensor_test_(_) ->
	NewSensorId = {{-1, 80.0}, sensor},
	
	% The first call to random:uniform/1 will be to select a sensor from the morphology,
	% since there is only one unused sensor we return that one. The second call will be to
	% select a neuron from the list, we will connect neuron c, which is under index 3.  
	meck:sequence(random, uniform, 1, [1, 3]),
	
	meck:sequence(random, uniform, 0, [0.6, 0.6]),

	FakeTimeProvider = fun() -> {0, 0.0125, 0} end,

	in_transaction(fun() -> genotype_mutator:add_sensor(?AGENT, FakeTimeProvider) end),

	NewSensor = find_sensor(NewSensorId),
	NeuronC = find_neuron(?C),
	Cortex = find_cortex(?CORTEX),
	Agent = find_agent(?AGENT),
	[LastMutation|_] = Agent#agent.evo_hist,

	[?_assertNot(NewSensor == undefined),
	 ?_assert(lists:member(?C, NewSensor#sensor.fanout_ids)),
	 ?_assert(lists:keymember(NewSensorId, 1, NeuronC#neuron.input_ids_plus_weights)),
	 ?_assert(lists:member(NewSensorId, Cortex#cortex.sensor_ids)),
	 ?_assertEqual({add_sensor, NewSensorId, ?C}, LastMutation)].
	% add cortex and agent tests here...
	
%% ===================================================================
%% Creating links
%% ===================================================================

create_link_between_neurons_test_(_) ->
	meck:sequence(random, uniform, 0, [0.6]),

	create_link_between_elements(?A, ?B),
	
	NeuronA = find_neuron(?A),
	NeuronB = find_neuron(?B),
	
	[?_assert(lists:member(?B, NeuronA#neuron.output_ids)),
	?_assert(lists:member(?B, NeuronA#neuron.recursive_output_ids)),
	?_assert(lists:keymember(?A, 1, NeuronB#neuron.input_ids_plus_weights))].

create_link_between_sensor_and_neuron_test_(_) ->
	meck:sequence(random, uniform, 0, [0.6]),

    create_link_between_elements(?SENSOR, ?C),
	
	Sensor = find_sensor(?SENSOR),	
	NeuronC = find_neuron(?C),

	[?_assert(lists:member(?C, Sensor#sensor.fanout_ids)),
	?_assert(lists:keymember(?SENSOR, 1, NeuronC#neuron.input_ids_plus_weights))].

create_link_between_neuron_and_actuator_test_(_) ->
	meck:sequence(random, uniform, 0, [0.6]),

	create_link_between_elements(?B, ?ACTUATOR),

	NeuronB = find_neuron(?B),
	Actuator = find_actuator(?ACTUATOR),

	[?_assert(lists:member(?ACTUATOR, NeuronB#neuron.output_ids)),
	?_assert(lists:member(?B, Actuator#actuator.fanin_ids))].

%% ===================================================================
%% Cutting links
%% ===================================================================

cut_link_between_neurons_test_(_) ->
	cut_link_between_elements(?A, ?C),

	NeuronA = find_neuron(?A),
	NeuronC = find_neuron(?C),

	[?_assert(not lists:member(?C, NeuronA#neuron.output_ids)),
	?_assert(not lists:keymember(?A, 1, NeuronC#neuron.input_ids_plus_weights))].

cut_link_between_sensor_and_neuron_test_(_) ->
	cut_link_between_elements(?SENSOR, ?A),
	
	Sensor = find_sensor(?SENSOR),
	NeuronA = find_neuron(?A),

	[?_assert(not lists:member(?A, Sensor#sensor.fanout_ids)),
	?_assert(not lists:keymember(?SENSOR, 1, NeuronA#neuron.input_ids_plus_weights))].

cut_link_between_neuron_and_actuator_test_(_) ->
	cut_link_between_elements(?D, ?ACTUATOR),
	
	NeuronD = find_neuron(?D),
	Actuator = find_actuator(?ACTUATOR),

	[?_assert(not lists:member(?ACTUATOR, NeuronD#neuron.output_ids)),
	?_assert(not lists:member(?D, Actuator#actuator.fanin_ids))].

create_link_between_elements(From, To) ->
	in_transaction(fun() -> genotype_mutator:create_link_between_elements(?AGENT, From, To) end).

cut_link_between_elements(From, To) ->
	in_transaction(fun() ->	genotype_mutator:cut_link_between_elements(?AGENT, From, To) end).

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

in_transaction(Action) ->
	{atomic, _} = mnesia:transaction(Action).

%%      A  
%%    /   \
%%  s       C  - D -  a
%%    \   /
%%      B 
create_test_genotype() ->
	[#agent{id = ?AGENT,
			 generation = 0,
			 population_id = undefined,	
			 species_id = test,
			 cortex_id = {{origin,10},cortex},
			 fingerprint = {
				[{0,1}],
				[],
				[{sensor,undefined,undefined,xor_get_input,
					{private,xor_sim},
					2,
					[?A, ?B]}],
				[{actuator,undefined,undefined,xor_send_output,
					{private,xor_sim},
					1,
					[?D]}]},
			 constraint = #constraint{morphology = xor_mimic, neural_afs = [tanh,cos,gauss,abs]},
		     evo_hist = [],
			 fitness = undefined,
			 innovation_factor = undefined,
		     pattern = [{-0.5, [?A]}, {0, [?B,?C]}, {0.5, [?D]}]},
	 #cortex{
	 	id = {{origin,10},cortex},
		agent_id = ?AGENT,
		neuron_ids = [?A, ?B, ?C, ?D],
	    sensor_ids = [?SENSOR],
	    actuator_ids = [?ACTUATOR]},
	#sensor{
		id = ?SENSOR,
		cortex_id = {{origin,10},cortex},
		name = xor_get_input,
		scape = {private,xor_sim},
		vl = 2,
		fanout_ids = [?A, ?B]},
	#neuron{ % neuron A
		id = ?A,
		generation = 0,
		cortex_id = {{origin,10},cortex},
		af = gauss,
		input_ids_plus_weights = [{?SENSOR,
			  [0.43873748179197447,-0.17357135008390578]}],
		output_ids = [?C],
		recursive_output_ids = []},
	#neuron{ % neuron B
		id = ?B,
		generation = 0,
		cortex_id = {{origin,10},cortex},
		af = gauss,
		input_ids_plus_weights = [{?SENSOR,
			  [0.43873748179197447,-0.17357135008390578]}],
		output_ids = [?C],
		recursive_output_ids = []},
	#neuron{ % neuron C
		id = ?C,
		generation = 0,
		cortex_id = {{origin,10},cortex},
		af = gauss,
		input_ids_plus_weights = [
			{?A, [0.43873748179197447,-0.17357135008390578]},  
			{?B, [0.43873748179197447,-0.17357135008390578]}],
		output_ids = [?D],
		recursive_output_ids = []},
	#neuron{ % neuron D
		id = ?D,
		generation = 0,
		cortex_id = {{origin,10},cortex},
		af = gauss,
		input_ids_plus_weights = [
			{?C, [0.43873748179197447,-0.17357135008390578]},
			{bias, [0.4]}],
		output_ids = [?ACTUATOR],
		recursive_output_ids = []},
	 #actuator{
		id = ?ACTUATOR,
		cortex_id = {{origin,10},cortex},
		name = xor_send_output,
		scape = {private,xor_sim},
		vl = 2,
		fanin_ids = [?D]}].
