-module(genotype_mutator_tests).
-compile(export_all).
-include_lib("eunit/include/eunit.hrl").
-include("../src/records.hrl").
-define(A, {{-0.5, 40}, neuron}).
-define(B, {{-0.5, 50}, neuron}).
-define(C, {{0, 60}, neuron}).
-define(D, {{0.5, 70}, neuron}).

link_neuron_to_neuron_test() ->
	polis:start(),
	F = fun() ->
		[mnesia:write(R) || R <- create_test_genotype()],
		genotype_mutator:create_link_between_elements(test, ?A, ?B)
	end,
	mnesia:transaction(F),

	NeuronA = find_neuron(?A),
	?assert(lists:member(?B, NeuronA#neuron.output_ids)),
	?assert(lists:member(?B, NeuronA#neuron.recursive_output_ids)),
	
	NeuronB = find_neuron(?B),
	?assert(lists:keymember(?A, 1, NeuronB#neuron.input_ids_plus_weights)),

	polis:stop().

find_neuron(NeuronId) ->
	F = fun() ->
		genotype:read({neuron, NeuronId})
	end,
	{atomic, Neuron} = mnesia:transaction(F),
	Neuron.

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
					[{{-0.5,40},neuron}, {{-0.5,50},neuron}]}],
				[{actuator,undefined,undefined,xor_send_output,
					{private,xor_sim},
					1,
					[{{0.5,70},neuron}]}]},
			 constraint = #constraint{morphology = xor_mimic, neural_afs = [tanh,cos,gauss,abs]},
		     evo_hist = [],
			 fitness = undefined,
			 innovation_factor = undefined,
		     pattern = [{0,[{{-0.5,40},neuron},{{-0.5,50},neuron},{{0,60},neuron},{{0.5,70},neuron}]}]},
	 #cortex{
	 	id = {{origin,10},cortex},
		agent_id = test,
		neuron_ids = [{{-0.5,40},neuron}, {{-0.5,50},neuron}, {{-0,60},neuron}, {{0.5,70},neuron}],
	    sensor_ids = [{{-1,20},sensor}],
	    actuator_ids = [{{1,30},actuator}]},
	#sensor{
		id = {{-1,20},sensor},
		cortex_id = {{origin,10},cortex},
		name = xor_get_input,
		scape = {private,xor_sim},
		vl = 2,
		fanout_ids = [{{-0.5,40},neuron}, {{-0.5,50},neuron}]},
	#neuron{ % neuron A
		id = {{-0.5,40},neuron},
		generation = 0,
		cortex_id = {{origin,10},cortex},
		af = gauss,
		input_ids_plus_weights = [{{{-1,30},sensor},
			  [0.43873748179197447,-0.17357135008390578]}],
		output_ids = [{{0,60},neuron}],
		recursive_output_ids = []},
	#neuron{ % neuron B
		id = {{-0.5,50},neuron},
		generation = 0,
		cortex_id = {{origin,10},cortex},
		af = gauss,
		input_ids_plus_weights = [{{{-1,30},sensor},
			  [0.43873748179197447,-0.17357135008390578]}],
		output_ids = [{{0,60},neuron}],
		recursive_output_ids = []},
	#neuron{ % neuron C
		id = {{0,60},neuron},
		generation = 0,
		cortex_id = {{origin,10},cortex},
		af = gauss,
		input_ids_plus_weights = [
			{{{-1,40},neuron}, [0.43873748179197447,-0.17357135008390578]},  
			{{{-1,50},neuron}, [0.43873748179197447,-0.17357135008390578]}],
		output_ids = [{{0.5,70},neuron}],
		recursive_output_ids = []},
	#neuron{ % neuron D
		id = {{0.5,70},neuron},
		generation = 0,
		cortex_id = {{origin,10},cortex},
		af = gauss,
		input_ids_plus_weights = [
			{{{0.5,60},neuron}, [0.43873748179197447,-0.17357135008390578]}],
		output_ids = [{{1,30},actuator}],
		recursive_output_ids = []},
	 #actuator{
		id = {{1,30},actuator},
		cortex_id = {{origin,10},cortex},
		name = xor_send_output,
		scape = {private,xor_sim},
		vl =1,
		fanin_ids = [{{0,60},neuron}]}].
