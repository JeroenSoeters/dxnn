-module(genotype_mutator_tests).
-compile(export_all).
-include_lib("eunit/include/eunit.hrl").
-include("../src/records.hrl").
-define(AGENT, test_agent).
-define(SENSOR, {{-1, 20}, sensor}).
-define(ACTUATOR, {{1, 30}, actuator}).
-define(A, {{-0.5, 40}, neuron}).
-define(B, {{-0.5, 50}, neuron}).
-define(C, {{0, 60}, neuron}).
-define(D, {{0.5, 70}, neuron}).

%% ===================================================================
%% Setup and teardown
%% ===================================================================

setup() ->
	polis:start(),
	F = fun() ->
		[mnesia:write(R) || R <- create_test_genotype()]
	end,
	mnesia:transaction(F).

teardown() ->
	polis:stop().


%% ===================================================================
%% Mutation operators
%% ===================================================================

mutate_weights_test() ->
	setup(),

	% This will select neuron C from our cortex since we have 4 neurons.. Neuron Ci has 2 weights.
	% MP = 1/sqrt(2) is 0.7. We thus expect both weights to be changed as 1 > 0.7.
	mutate_weights({fun() -> 0.6 end, fun(4) -> 3 end}),	
	
	NeuronC = find_neuron(?C),
	Agent = find_agent(?AGENT),
	[LastMutation|_] = Agent#agent.evo_hist,

	?assert(lists:nth(1, NeuronC#neuron.input_ids_plus_weights) =/= 
		lists:nth(1, (lists:nth(6, create_test_genotype()))#neuron.input_ids_plus_weights)),	

	?assert(lists:nth(2, NeuronC#neuron.input_ids_plus_weights) =/= 
		lists:nth(2, (lists:nth(6, create_test_genotype()))#neuron.input_ids_plus_weights)),
	
	?assert(LastMutation == {mutate_weights, ?C}),	

	teardown().

mutate_weights(Randomizer) ->
	in_transaction(fun() ->	genotype_mutator:mutate_weights(?AGENT, Randomizer) end).

add_bias_test() ->
	setup(),

	add_bias({fun() -> 0.9 end, fun(4) -> 1 end}),

	NeuronA = find_neuron(?A),
	Agent = find_agent(?AGENT),
	[LastMutation|_] = Agent#agent.evo_hist,

	?assertEqual({bias, 0.4}, lists:last(NeuronA#neuron.input_ids_plus_weights)),
	?assertEqual({add_bias, ?A}, LastMutation),

	teardown().

add_bias(Randomizer) ->
	in_transaction(fun() -> genotype_mutator:add_bias(?AGENT, Randomizer) end).

remove_bias_test() ->
	setup(),
	
	remove_bias({whatever, fun(4) -> 4 end}),

	NeuronD = find_neuron(?D),

	?assertNot(lists:keymember(bias, 1, NeuronD#neuron.input_ids_plus_weights)),

	teardown().

remove_bias(Randomizer) ->
	in_transaction(fun() -> genotype_mutator:remove_bias(?AGENT, Randomizer) end).

%% ===================================================================
%% Creating links
%% ===================================================================

create_link_between_neurons_test() ->
	setup(),

	create_link_between_elements(?A, ?B),
	
	NeuronA = find_neuron(?A),
	NeuronB = find_neuron(?B),
	
	?assert(lists:member(?B, NeuronA#neuron.output_ids)),
	?assert(lists:member(?B, NeuronA#neuron.recursive_output_ids)),
	?assert(lists:keymember(?A, 1, NeuronB#neuron.input_ids_plus_weights)),

	teardown().

create_link_betweem_sensor_and_neuron_test() ->
	setup(),

    create_link_between_elements(?SENSOR, ?C),
	
	Sensor = find_sensor(?SENSOR),	
	NeuronC = find_neuron(?C),

	?assert(lists:member(?C, Sensor#sensor.fanout_ids)),
	?assert(lists:keymember(?SENSOR, 1, NeuronC#neuron.input_ids_plus_weights)),

	teardown().

create_link_between_neuron_and_actuator_test() ->
	setup(),

	create_link_between_elements(?B, ?ACTUATOR),

	NeuronB = find_neuron(?B),
	Actuator = find_actuator(?ACTUATOR),

	?assert(lists:member(?ACTUATOR, NeuronB#neuron.output_ids)),
	?assert(lists:member(?B, Actuator#actuator.fanin_ids)),

	teardown().

%% ===================================================================
%% Cutting links
%% ===================================================================

cut_link_between_neurons_test() ->
	setup(),

	cut_link_between_elements(?A, ?C),

	NeuronA = find_neuron(?A),
	NeuronC = find_neuron(?C),

	?assert(not lists:member(?C, NeuronA#neuron.output_ids)),
	?assert(not lists:keymember(?A, 1, NeuronC#neuron.input_ids_plus_weights)),	

	teardown().

cut_link_between_sensor_and_neuron_test() ->
	setup(),

	cut_link_between_elements(?SENSOR, ?A),
	
	Sensor = find_sensor(?SENSOR),
	NeuronA = find_neuron(?A),

	?assert(not lists:member(?A, Sensor#sensor.fanout_ids)),
	?assert(not lists:keymember(?SENSOR, 1, NeuronA#neuron.input_ids_plus_weights)),

	teardown().

cut_link_between_neuron_and_actuator_test() ->
	setup(),

	cut_link_between_elements(?D, ?ACTUATOR),
	
	NeuronD = find_neuron(?D),
	Actuator = find_actuator(?ACTUATOR),

	?assert(not lists:member(?ACTUATOR, NeuronD#neuron.output_ids)),
	?assert(not lists:member(?D, Actuator#actuator.fanin_ids)),

	teardown().

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
		     pattern = [{0,[?A,?B,?C,?D]}]},
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
		vl =1,
		fanin_ids = [?D]}].
