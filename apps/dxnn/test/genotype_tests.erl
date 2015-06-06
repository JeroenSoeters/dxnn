-module(genotype_tests).
-include_lib("eunit/include/eunit.hrl").
-include("../src/records.hrl").
-compile(export_all).
-define(AGENT, test_agent).
-define(SPECIES_ID, test_species).
-define(CORTEX, {{origin, 1.0}, cortex}).
-define(SENSOR, {{-1, 2.0}, sensor}).
-define(NEURON, {{0, 8.0}, neuron}).
-define(ACTUATOR, {{1, 4.0}, actuator}).
-define(CLONE, cloned_agent).
-define(CLONED_CORTEX, {{origin, 0.5}, cortex}).
-define(CLONED_SENSOR, {{-1, 5.0}, sensor}).
-define(CLONED_NEURON, {{0, 16.0}, neuron}).
-define(CLONED_ACTUATOR, {{1, 20.0}, actuator}).

genotype_test_() ->
	{foreach,
	 fun setup/0,
	 fun teardown/1,
	 [fun ?MODULE:construct_agent_test_/1,
      fun ?MODULE:construct_cortex_test_/1,
	  fun ?MODULE:construct_sensor_test_/1,
	  fun ?MODULE:construct_actuator_test_/1,
	  fun ?MODULE:construct_neuron_test_/1,
	  fun ?MODULE:clone_agent_test_/1,
	  fun ?MODULE:clone_cortex_test_/1,
	  fun ?MODULE:clone_actuator_test_/1,
	  fun ?MODULE:clone_sensor_test_/1,
      fun ?MODULE:clone_neuron_test_/1]}.

setup() ->
	mnesia:delete_schema({node()}),
	ok = polis:create(),

	case whereis(random_meck) of
		undefined ->
			meck:new(random, [unstick]);
		_ ->
			meck:unload(random),
			meck:new(random, [unstick])
	end,
	meck:sequence(random, seed, 1, [0]),
	% The first number returned is the probability of connecting the sensor to a single neuron, because this is < 0 the sensor will be connected to all neurons. The second and third are for the random weights.
	meck:sequence(random, uniform, 0, [0, 0.9, 1.0]),
	% This is the call for the activation function, which will result in the cos function being selected. The second result is the index of the neuron of which the af is going to be mutated, the third is the af selector (gauss).
	meck:sequence(random, uniform, 1, [2, 1, 2]),
	
	% These numbers will result in the Id's 1, 2, 4 and 8 for respectively the cortex_id, sensor_id, actuator_id and neuron_id
	Pid = test_helpers:create_sequence_generator([{0, 1, 0}, {0, 0.5, 0}, {0, 0.25, 0}, {0, 0.125, 0}, {0, 2, 0}, {0, 0.2, 0}, {0, 0.0625, 0}, {0, 0.05, 0}]),
	FakeTimeProvider = fun() -> test_helpers:next_item_from_sequence(Pid) end,

	test_helpers:in_transaction(fun() -> 
		genotype:construct_agent(?SPECIES_ID, ?AGENT, #constraint{}, FakeTimeProvider),
		% we apply one mutate_af mutation to generate an evolution history
		genome_mutator:test_x(?AGENT, mutate_af),
		genotype:clone_agent(?AGENT, ?CLONE, FakeTimeProvider)
	end).

teardown(_) ->
	meck:unload(random).

construct_agent_test_(_) ->
	Agent = test_helpers:find_agent(?AGENT),
	[?_assertNot(Agent == undefined),
	 ?_assertEqual(?CORTEX, Agent#agent.cortex_id),
	 ?_assertEqual(?SPECIES_ID, Agent#agent.species_id),
	 ?_assertEqual(0, Agent#agent.generation),
	 ?_assertEqual(#constraint{}, Agent#agent.constraint),
	 ?_assertEqual([{mutate_af, ?NEURON}], Agent#agent.evo_hist),
	 ?_assertEqual([{0, [?NEURON]}], Agent#agent.pattern)].

construct_cortex_test_(_) ->
	Cortex = test_helpers:find_cortex(?CORTEX),

	[?_assertNot(Cortex == undefined),
	 ?_assertEqual(?AGENT, Cortex#cortex.agent_id),
	 ?_assertEqual([?ACTUATOR], Cortex#cortex.actuator_ids)].

construct_sensor_test_(_) ->
	Sensor = test_helpers:find_sensor(?SENSOR),

	[?_assertNot(Sensor == undefined),
	 ?_assertEqual(?CORTEX, Sensor#sensor.cortex_id),
	 ?_assertEqual(0, Sensor#sensor.generation),
	 ?_assertEqual([?NEURON], Sensor#sensor.fanout_ids)].
		
construct_actuator_test_(_) ->
	Actuator = test_helpers:find_actuator(?ACTUATOR),

	[?_assertNot(Actuator == undefined),
	 ?_assertEqual(?CORTEX, Actuator#actuator.cortex_id),
	 ?_assertEqual(0, Actuator#actuator.generation),
	?_assertEqual([?NEURON], Actuator#actuator.fanin_ids)].

construct_neuron_test_(_) ->
	Neuron = test_helpers:find_neuron(?NEURON),

	[?_assertNot(Neuron == undefined),
	 ?_assertEqual(?CORTEX, Neuron#neuron.cortex_id), 
	 ?_assertEqual(0, Neuron#neuron.generation),
	 ?_assertEqual(gaussian, Neuron#neuron.af),
	 ?_assertEqual([{?SENSOR, [0.4, 0.5]}], Neuron#neuron.input_ids_plus_weights),
	 ?_assertEqual([?ACTUATOR], Neuron#neuron.output_ids),
	 ?_assertEqual([], Neuron#neuron.recursive_output_ids)].

clone_agent_test_(_) ->
	ClonedAgent = test_helpers:find_agent(?CLONE),
	
	[?_assertNot(ClonedAgent == undefined),
	 ?_assertEqual(?CLONED_CORTEX, ClonedAgent#agent.cortex_id)].

clone_cortex_test_(_) ->
	ClonedCortex = test_helpers:find_cortex(?CLONED_CORTEX),

	[?_assertNot(ClonedCortex == undefined),
	 ?_assertEqual(?CLONE, ClonedCortex#cortex.agent_id),
	 ?_assertEqual([?CLONED_SENSOR], ClonedCortex#cortex.sensor_ids),
	 ?_assertEqual([?CLONED_NEURON], ClonedCortex#cortex.neuron_ids),
	 ?_assertEqual([?CLONED_ACTUATOR], ClonedCortex#cortex.actuator_ids)].

clone_sensor_test_(_) ->
	ClonedSensor = test_helpers:find_sensor(?CLONED_SENSOR),

	[?_assertNot(ClonedSensor == undefined),
	 ?_assertEqual(?CLONED_CORTEX, ClonedSensor#sensor.cortex_id),
	 ?_assertEqual(0, ClonedSensor#sensor.generation),
	 ?_assertEqual([?CLONED_NEURON], ClonedSensor#sensor.fanout_ids)].

clone_actuator_test_(_) ->
	ClonedActuator = test_helpers:find_actuator(?CLONED_ACTUATOR),

	[?_assertNot(ClonedActuator == undefined),
	 ?_assertEqual(?CLONED_CORTEX, ClonedActuator#actuator.cortex_id),
	 ?_assertEqual(0, ClonedActuator#actuator.generation),
  	 ?_assertEqual([?CLONED_NEURON], ClonedActuator#actuator.fanin_ids)].

clone_neuron_test_(_) ->
	ClonedNeuron = test_helpers:find_neuron(?CLONED_NEURON),

	[?_assertNot(ClonedNeuron == undefined),
	 ?_assertEqual(?CLONED_CORTEX, ClonedNeuron#neuron.cortex_id), 
	 ?_assertEqual(0, ClonedNeuron#neuron.generation),
	 ?_assertEqual(gaussian, ClonedNeuron#neuron.af),
	 ?_assertEqual([{?CLONED_SENSOR, [0.4, 0.5]}], ClonedNeuron#neuron.input_ids_plus_weights),
	 ?_assertEqual([?CLONED_ACTUATOR], ClonedNeuron#neuron.output_ids),
	 ?_assertEqual([], ClonedNeuron#neuron.recursive_output_ids)].

construct_and_clone_agent(AgentId, CloneId) ->
	SpeciesConstraint = #constraint{},
	genotype:construct_agent(AgentId, test, SpeciesConstraint, fun now/0),
	genotype:clone_agent(AgentId, CloneId, fun now/0).

cleanup_agents(AgentId, CloneId) ->
	genotype:delete_agent(AgentId),
	genotype:delete_agent(CloneId).

