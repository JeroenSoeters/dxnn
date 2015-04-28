-module(genotype_mutator_tests).
-compile(export_all).
-include_lib("eunit/include/eunit.hrl").
-include("../src/records.hrl").
-define(SENSOR, {{-1, 20}, sensor}).
-define(ACTUATOR, {{1, 30}, actuator}).
-define(A, {{-0.5, 40}, neuron}).
-define(B, {{-0.5, 50}, neuron}).
-define(C, {{0, 60}, neuron}).
-define(D, {{0.5, 70}, neuron}).

setup() ->
	polis:start(),
	F = fun() ->
		[mnesia:write(R) || R <- create_test_genotype()],
		genotype_mutator:create_link_between_elements(test, ?A, ?B)
	end,
	mnesia:transaction(F).

teardown() ->
	polis:stop().

link_elements_test_ignore() ->
	{foreach,
	 fun() -> setup() end,
	 fun() -> polist:stop() end,
	 [fun() -> link_neuron_to_neuron_test() end]}.

link_neuron_to_neuron_test() ->
	setup(),

	create_link_between_elements(?A, ?B),
	
	NeuronA = find_neuron(?A),
	NeuronB = find_neuron(?B),
	
	?assert(lists:member(?B, NeuronA#neuron.output_ids)),
	?assert(lists:member(?B, NeuronA#neuron.recursive_output_ids)),
	?assert(lists:keymember(?A, 1, NeuronB#neuron.input_ids_plus_weights)),

	teardown().

link_sensor_to_neuron_test() ->
	setup(),

    create_link_between_elements(?SENSOR, ?C),
	
	Sensor = find_sensor(?SENSOR),	
	NeuronC = find_neuron(?C),

	?assert(lists:member(?C, Sensor#sensor.fanout_ids)),
	?assert(lists:keymember(?SENSOR, 1, NeuronC#neuron.input_ids_plus_weights)),

	teardown().

create_link_between_elements(From, To) ->
	F = fun() ->
		genotype_mutator:create_link_between_elements(test, From, To)
	end,
	mnesia:transaction(F).

find_neuron(NeuronId) ->
	find_node(neuron, NeuronId).

find_sensor(SensorId) ->
	find_node(sensor, SensorId).

find_node(NodeType, NodeId) ->
	F = fun() ->
		genotype:read({NodeType, NodeId})
	end,
	{atomic, Node} = mnesia:transaction(F),
	Node.

%%      A  
%%    /   \
%%  s       C  - D -  a
%%    \   /
%%      B 
create_test_genotype() ->
	[#agent{id = test,
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
		agent_id = test,
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
			{{{-1,40},neuron}, [0.43873748179197447,-0.17357135008390578]},  
			{{{-1,50},neuron}, [0.43873748179197447,-0.17357135008390578]}],
		output_ids = [?D],
		recursive_output_ids = []},
	#neuron{ % neuron D
		id = ?D,
		generation = 0,
		cortex_id = {{origin,10},cortex},
		af = gauss,
		input_ids_plus_weights = [
			{{{0.5,60},neuron}, [0.43873748179197447,-0.17357135008390578]}],
		output_ids = [?ACTUATOR],
		recursive_output_ids = []},
	 #actuator{
		id = ?ACTUATOR,
		cortex_id = {{origin,10},cortex},
		name = xor_send_output,
		scape = {private,xor_sim},
		vl =1,
		fanin_ids = [?C]}].
