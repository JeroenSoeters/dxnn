-module(population_monitor_tests).
-compile(export_all).
-include_lib("eunit/include/eunit.hrl").
-include("../src/records.hrl").
-define(POPULATION, {p1, population}).
-define(SPECIES1, {s1, species}).
-define(SPECIES2, {s2, species}).
-define(AGENT1, {1, agent}).
-define(AGENT2, {2, agent}).
-define(AGENT3, {3, agent}).
-define(AGENT4, {4, agent}).
-define(AGENT5, {5, agent}).
-define(AGENT6, {6, agent}).
-define(AGENT7, {7, agent}).
-define(AGENT8, {8, agent}).
% this is quite brittle as this test data is tightly coupled to the implementation,
% should be fixed later, maybe by just mocking genotype.
-define(AGENT9, {9.0, agent}).
-define(AGENT10, {12.0, agent}).
-define(AGENT11, {15.0, agent}).
-define(AGENT12, {18.0, agent}).
-define(AGENT13, {21.0, agent}).
-define(CORTEX1, {c1, cortex}).
-define(CORTEX2, {c2, cortex}).
-define(CORTEX3, {c3, cortex}).
-define(CORTEX4, {c4, cortex}).
-define(CORTEX5, {c5, cortex}).
-define(CORTEX6, {c6, cortex}).
-define(CORTEX7, {c7, cortex}).
-define(CORTEX8, {c8, cortex}).

-record(state, {op_mode, population_id, active_agent_ids_and_pids=[], agent_ids=[], total_agents, agents_left, op_tag, agent_summaries=[], pop_gen=0, eval_acc=0, cycle_acc=0, time_acc=0, step_size, next_step, goal_status, selection_algorithm, population_limit, time_provider}).

population_monitor_test_() ->
	{foreach,
	 fun setup/0,
	 fun teardown/1,
	 [fun ?MODULE:init_test_/1,
	  fun ?MODULE:an_agent_terminated_test_/1,
	  fun ?MODULE:last_agent_terminated_then_continue_end_condition_not_reached_test_/1,
	  fun ?MODULE:last_agent_terminated_then_continue_end_condition_reached_test_/1,
	  fun ?MODULE:last_agent_terminated_then_done_test_/1,
	  fun ?MODULE:last_agent_terminated_then_pause_test_/1,
	  fun ?MODULE:pause_population_monitor_test_/1,
	  fun ?MODULE:resume_population_monitor_test_/1,
	  fun ?MODULE:stop_population_monitor_test_/1,
	  fun ?MODULE:shutdown_population_monitor_test_/1,
	  fun ?MODULE:extract_all_agent_ids_test_/1,
	  fun ?MODULE:calculate_neural_energy_cost_test_/1,
	  fun ?MODULE:construct_agent_summaries_test_/1,
	  fun ?MODULE:calculate_alotments_test_/1,
	  fun ?MODULE:calculate_species_fitness_test_/1,
	  fun ?MODULE:create_population_test_/1,
	  fun ?MODULE:mutate_population_test_/1,
	  fun ?MODULE:best_fitness_test_/1]}.

%% ===================================================================
%% Setup and teardown
%% ===================================================================

setup() ->
	mnesia:delete_schema({node()}),
	ok = polis:create(),
	in_transaction(fun() ->	[mnesia:write(R) || R <- create_test_population()] end),
	case whereis(exoself_meck) of
		undefined ->
			meck:new(exoself, [non_strict]);
		_ ->
			meck:unload(exoself),
			meck:new(exoself, [])
	end.

teardown(_) ->
	meck:unload(exoself).

%% ===================================================================
%% Population monitor
%% ===================================================================

init_test_(_) ->
	% Mock exoself:start/1 to just return an incrementing process id as we don't want to test the exoself here
	% but just test the population monitor in isolation.
	meck:sequence(exoself, start, 1, [1, 2, 3, 4, 5, 6, 7, 8]),

	{ok, State} = population_monitor:init({gt, ?POPULATION, competition}),

	[?_assertEqual(gt, State#state.op_mode),
	 ?_assertEqual(?POPULATION, State#state.population_id),
	 ?_assert(lists:member({?AGENT1, 5}, State#state.active_agent_ids_and_pids)),
	 ?_assert(lists:member({?AGENT2, 6}, State#state.active_agent_ids_and_pids)),
	 ?_assert(lists:member({?AGENT3, 7}, State#state.active_agent_ids_and_pids)),
	 ?_assert(lists:member({?AGENT4, 8}, State#state.active_agent_ids_and_pids)),
	 ?_assert(lists:member({?AGENT5, 1}, State#state.active_agent_ids_and_pids)),
	 ?_assert(lists:member({?AGENT6, 2}, State#state.active_agent_ids_and_pids)),
	 ?_assert(lists:member({?AGENT7, 3}, State#state.active_agent_ids_and_pids)),
	 ?_assert(lists:member({?AGENT8, 4}, State#state.active_agent_ids_and_pids)),
	 ?_assertEqual(8, State#state.total_agents),
	 ?_assertEqual(8, State#state.agents_left),
	 ?_assertEqual(continue, State#state.op_tag),
	 ?_assertEqual(competition, State#state.selection_algorithm),
	 ?_assertEqual(0, State#state.eval_acc),
	 ?_assertEqual(0, State#state.cycle_acc),
	 ?_assertEqual(0, State#state.time_acc)].

stop_population_monitor_test_(_) ->
	DummyAgent = fun() -> receive {_Pid, terminate} -> ok end end,
	Agent1Pid = spawn(DummyAgent),
	Agent2Pid = spawn(DummyAgent),

	{stop, normal, _State} = population_monitor:handle_call(
		{stop, normal},
		self(),
		#state{
			active_agent_ids_and_pids = [{?AGENT1, Agent1Pid}, {?AGENT2, Agent2Pid}]
		}),

	[?_assertNot(is_process_alive(Agent1Pid)),
	 ?_assertNot(is_process_alive(Agent2Pid))].

shutdown_population_monitor_test_(_) ->
	State = #state{},

	Result = population_monitor:handle_call({stop, shutdown}, self(),  State),
	?_assertEqual({stop, shutdown, State}, Result). 

an_agent_terminated_test_(_) ->
	{noreply, State} = population_monitor:handle_cast(
		{?AGENT1, terminated, 100, 1, 1, 1},
		#state{
			active_agent_ids_and_pids = [{?AGENT1, 1}, {?AGENT2, 2}, {?AGENT3, 3}],
			agents_left = 3,
			eval_acc = 1,
			cycle_acc = 2,
			time_acc = 3,
			selection_algorithm = competition
		}),

	[?_assertEqual([{?AGENT2, 2}, {?AGENT3, 3}], State#state.active_agent_ids_and_pids),
	 ?_assertEqual(2, State#state.agents_left),
	 ?_assertEqual(2, State#state.eval_acc),
	 ?_assertEqual(3, State#state.cycle_acc),
	 ?_assertEqual(4, State#state.time_acc)].
	
last_agent_terminated_then_continue_end_condition_not_reached_test_(_) ->
	% Mock exoself:start/1 to just return an incrementing process id as we don't want to test the exoself here
	% but just test the population monitor in isolation.
	meck:sequence(exoself, start, 1, [9, 10, 11, 12, 13, 14, 15, 16]),

	GeneratorPid = spawn(?MODULE, sequence_generator, [9]),

	FakeTimeProvider = fun() -> {0, 1/generate_number(GeneratorPid), 0} end,

	{noreply, State} = population_monitor:handle_cast(
		{?AGENT1, terminated, 4, 5, 6, 7},
		#state{
			op_tag = continue,
			pop_gen = 0,
			population_id = ?POPULATION,
			agent_ids = [?AGENT1, ?AGENT2, ?AGENT3, ?AGENT4, ?AGENT5, ?AGENT6, ?AGENT7, ?AGENT8],
			total_agents = 8,
			active_agent_ids_and_pids = [{?AGENT1, 1}],
			agents_left = 1,
			eval_acc = 1,
			cycle_acc = 2,
			time_acc = 3,
			selection_algorithm = competition,
			population_limit = 4,
			time_provider = FakeTimeProvider
		}),

	exit(GeneratorPid, normal),
	
	[?_assertEqual(
		ordsets:from_list([{?AGENT2, 16}, {?AGENT4, 13}, {?AGENT7, 9}, {?AGENT9, 14}, {?AGENT10, 15}, {?AGENT11, 10}, {?AGENT12, 11}, {?AGENT13, 12}]),
		ordsets:from_list(State#state.active_agent_ids_and_pids)),
	 ?_assertEqual(8, State#state.total_agents),
	 ?_assertEqual(8, State#state.agents_left),
	 ?_assertEqual(
		ordsets:from_list([?AGENT2, ?AGENT4, ?AGENT7, ?AGENT9, ?AGENT10, ?AGENT11, ?AGENT12, ?AGENT13]),
		ordsets:from_list(State#state.agent_ids)),
	 ?_assertEqual(1, State#state.pop_gen),
	 ?_assertEqual(6, State#state.eval_acc),
	 ?_assertEqual(8, State#state.cycle_acc),
	 ?_assertEqual(10, State#state.time_acc)].

last_agent_terminated_then_continue_end_condition_reached_test_(_) ->
	GeneratorPid = spawn(?MODULE, sequence_generator, [9]),

	FakeTimeProvider = fun() -> {0, 1/generate_number(GeneratorPid), 0} end,

	{stop, normal, State} = population_monitor:handle_cast(
		{?AGENT1, terminated, 4, 5, 6, 7},
		#state{
			op_tag = continue,
			pop_gen = 100, % triggers ending condition
			population_id = ?POPULATION,
			agent_ids = [?AGENT1, ?AGENT2, ?AGENT3, ?AGENT4, ?AGENT5, ?AGENT6, ?AGENT7, ?AGENT8],
			total_agents = 8,
			active_agent_ids_and_pids = [{?AGENT1, 1}],
			agents_left = 1,
			eval_acc = 1,
			cycle_acc = 2,
			time_acc = 3,
			selection_algorithm = competition,
			population_limit = 4,
			time_provider = FakeTimeProvider
		}),

	exit(GeneratorPid, normal),
	
	[?_assertEqual([{?AGENT1, 1}], State#state.active_agent_ids_and_pids),
	 ?_assertEqual(8, State#state.total_agents),
	 ?_assertEqual(8, State#state.agents_left),
	 ?_assertEqual(
		ordsets:from_list([?AGENT2, ?AGENT4, ?AGENT7, ?AGENT9, ?AGENT10, ?AGENT11, ?AGENT12, ?AGENT13]),
		ordsets:from_list(State#state.agent_ids)),
	 ?_assertEqual(101, State#state.pop_gen),
	 ?_assertEqual(6, State#state.eval_acc),
	 ?_assertEqual(8, State#state.cycle_acc),
	 ?_assertEqual(10, State#state.time_acc)].

last_agent_terminated_then_done_test_(_) ->
	GeneratorPid = spawn(?MODULE, sequence_generator, [9]),

	FakeTimeProvider = fun() -> {0, 1/generate_number(GeneratorPid), 0} end,

	{stop, normal, State} = population_monitor:handle_cast(
		{?AGENT1, terminated, 4, 5, 6, 7},
		#state{
			op_tag = done,
			pop_gen = 100, % triggers ending condition
			population_id = ?POPULATION,
			agent_ids = [?AGENT1, ?AGENT2, ?AGENT3, ?AGENT4, ?AGENT5, ?AGENT6, ?AGENT7, ?AGENT8],
			total_agents = 8,
			active_agent_ids_and_pids = [{?AGENT1, 1}],
			agents_left = 1,
			eval_acc = 1,
			cycle_acc = 2,
			time_acc = 3,
			selection_algorithm = competition,
			population_limit = 4,
			time_provider = FakeTimeProvider
		}),
	
	exit(GeneratorPid, normal),

	[?_assertEqual(0, State#state.agents_left),
	 ?_assertEqual(101, State#state.pop_gen),
	 ?_assertEqual(6, State#state.eval_acc),
	 ?_assertEqual(8, State#state.cycle_acc),
	 ?_assertEqual(10, State#state.time_acc)].

last_agent_terminated_then_pause_test_(_) ->
	GeneratorPid = spawn(?MODULE, sequence_generator, [9]),

	FakeTimeProvider = fun() -> {0, 1/generate_number(GeneratorPid), 0} end,

	{noreply, State} = population_monitor:handle_cast(
		{?AGENT1, terminated, 4, 5, 6, 7},
		#state{
			op_tag = pause,
			pop_gen = 100, % triggers ending condition
			population_id = ?POPULATION,
			agent_ids = [?AGENT1, ?AGENT2, ?AGENT3, ?AGENT4, ?AGENT5, ?AGENT6, ?AGENT7, ?AGENT8],
			total_agents = 8,
			active_agent_ids_and_pids = [{?AGENT1, 1}],
			agents_left = 1,
			eval_acc = 1,
			cycle_acc = 2,
			time_acc = 3,
			selection_algorithm = competition,
			population_limit = 4,
			time_provider = FakeTimeProvider
		}),
	
	exit(GeneratorPid, normal),
	
	[?_assertEqual(0, State#state.agents_left),
	 ?_assertEqual(101, State#state.pop_gen),
	 ?_assertEqual(6, State#state.eval_acc),
	 ?_assertEqual(8, State#state.cycle_acc),
	 ?_assertEqual(10, State#state.time_acc)].

pause_population_monitor_test_(_) ->
	{noreply, State} = population_monitor:handle_cast({op_tag, pause}, #state{op_tag = continue}),

	?_assertEqual(pause, State#state.op_tag).

resume_population_monitor_test_(_) ->
	% Mock exoself:start/1 to just return an incrementing process id as we don't want to test the exoself here
	% but just test the population monitor in isolation.
	meck:sequence(exoself, start, 1, [1, 2, 3, 4, 5, 6, 7, 8]),
	
	{noreply, State} = population_monitor:handle_cast(
		{op_tag, continue}, 
		#state{
			op_tag = pause,
			population_id = ?POPULATION
		}),

	[?_assertEqual(
		ordsets:from_list([{?AGENT1, 5}, {?AGENT2, 6}, {?AGENT3, 7}, {?AGENT4, 8}, {?AGENT5, 1}, {?AGENT6, 2}, {?AGENT7, 3}, {?AGENT8, 4}]),
		ordsets:from_list(State#state.active_agent_ids_and_pids)),
	 ?_assertEqual(8, State#state.total_agents),
	 ?_assertEqual(continue, State#state.op_tag)].

best_fitness_test_(_) ->
	BestFitness = population_monitor:best_fitness(?POPULATION),

	?_assertEqual(5, BestFitness).

calculate_species_fitness_test_(_) ->
	{Average, StandardDeviation, Minimum, Maximum} = population_monitor:calculate_species_fitness(?SPECIES2),
	
	[?_assertEqual(134.75, Average),
	 ?_assertEqual(211.13428783596473, StandardDeviation),
	 ?_assertEqual(4, Minimum),
	 ?_assertEqual(500, Maximum)].

create_population_test_(_) ->
	GeneratorPid = spawn(?MODULE, sequence_generator, [9]),

	FakeTimeProvider = fun() -> {0, 1/generate_number(GeneratorPid), 0} end,
	
	{atomic, _} = in_transaction(fun() -> population_monitor:create_population(
		create_pop_test,
		[#constraint{morphology=Morphology, neural_afs=NeuralAfs} || Morphology <- [xor_mimic], NeuralAfs <- [[tanh]]],
		2,
		FakeTimeProvider) end),
	
	exit(GeneratorPid, normal),
	
	Population = find_population(create_pop_test),
	Species = find_species(9.0),

	[?_assertNot(Population == undefined),
	 ?_assertEqual([9.0], Population#population.species_ids),
	 ?_assertNot(Species == undefined),
	 ?_assertEqual(create_pop_test, Species#species.population_id),
	 ?_assertEqual(origin, Species#species.fingerprint),
	 ?_assertEqual(#constraint{morphology=xor_mimic, neural_afs=[tanh]}, Species#species.constraint),
	 ?_assertEqual(2, length(Species#species.agent_ids))].

mutate_population_test_(_) ->
	GeneratorPid = spawn(?MODULE, sequence_generator, [9]),

	FakeTimeProvider = fun() -> {0, 1/generate_number(GeneratorPid), 0} end,
	
	{atomic, _} = population_monitor:mutate_population(?POPULATION, 4, competition, FakeTimeProvider),

	exit(GeneratorPid, normal),
	
	Species1 = find_species(?SPECIES1),
	Species2 = find_species(?SPECIES2),

	[?_assertNot(agent_exists(?AGENT1)),
	 ?_assert(agent_exists(?AGENT2)),
	 ?_assertNot(agent_exists(?AGENT3)),
	 ?_assert(agent_exists(?AGENT4)),
	 ?_assertNot(agent_exists(?AGENT5)),
	 ?_assertNot(agent_exists(?AGENT6)),
	 ?_assert(agent_exists(?AGENT7)),
	 ?_assertNot(agent_exists(?AGENT8)),
	 ?_assert(agent_exists(?AGENT9)),
	 ?_assert(agent_exists(?AGENT10)),
	 ?_assert(agent_exists(?AGENT11)),
	 ?_assert(agent_exists(?AGENT12)),
	 ?_assert(agent_exists(?AGENT13)),
	 ?_assertEqual(
		sets:from_list([?AGENT2, ?AGENT4, ?AGENT9, ?AGENT10]), 
		sets:from_list(Species1#species.agent_ids)),
	 ?_assertEqual(
		sets:from_list([?AGENT7, ?AGENT11, ?AGENT12, ?AGENT13]), 
		sets:from_list(Species2#species.agent_ids)),
	 ?_assertEqual([?AGENT2, ?AGENT4], Species1#species.champion_ids),
	 ?_assertEqual([?AGENT7, ?AGENT6], Species2#species.champion_ids),
	 ?_assertEqual({14.75, 10.256095748383007, 4, 25}, Species1#species.fitness),
	 ?_assertEqual({134.75, 211.13428783596473, 4, 500}, Species2#species.fitness),
	 ?_assertEqual(99, Species1#species.innovation_factor),
	 ?_assertEqual(0, Species2#species.innovation_factor)].

agent_exists(AgentId) ->
	F = fun() ->
		genotype:read({agent, AgentId}) 
	end,
	{atomic, Agent} = mnesia:transaction(F),
	not (Agent == undefined).

generate_number(GeneratorPid) ->
	GeneratorPid ! {self(), next},
	receive
		N -> N
	end.

sequence_generator(N) ->
	receive
		{Pid, next} ->
			Pid ! N,
			sequence_generator(N+1)
	end.

extract_all_agent_ids_test_(_) ->
	AgentIds = population_monitor:extract_agent_ids(?POPULATION, all),
	
	 [?_assert(lists:member(?AGENT1, AgentIds)),
	 ?_assert(lists:member(?AGENT2, AgentIds)),
	 ?_assert(lists:member(?AGENT3, AgentIds)),
	 ?_assert(lists:member(?AGENT4, AgentIds))].

calculate_neural_energy_cost_test_(_) ->
	NeuralEnergyCost = population_monitor:calculate_neural_energy_cost(?POPULATION),
		
	?_assertEqual(66.44444444444444, NeuralEnergyCost).

construct_agent_summaries_test_(_) ->
	AgentSummaries = population_monitor:construct_agent_summaries([?AGENT1, ?AGENT2]),
	
	?_assertEqual([{4, 1, ?AGENT1}, {25, 2, ?AGENT2}], AgentSummaries).

calculate_alotments_test_(_) ->
	{AlotmentsPlusAgentSummaries, EstimatedPopulationSize} = 
		population_monitor:calculate_alotments(population_monitor:construct_agent_summaries([?AGENT1, ?AGENT2]), 2),

	[?_assertEqual([{2.0, 4, 1, ?AGENT1}, {6.25, 25, 2, ?AGENT2}], AlotmentsPlusAgentSummaries),
	 ?_assertEqual(8.25, EstimatedPopulationSize)].

find_population(PopulationId) ->
	find_node(population, PopulationId).

find_species(SpeciesId) ->
	find_node(species, SpeciesId).

find_node(NodeType, NodeId) ->
	F = fun() ->
		genotype:read({NodeType, NodeId})
	end,
	{atomic, Node} = mnesia:transaction(F),
	Node.

in_transaction(Action) ->
	{atomic, _} = mnesia:sync_transaction(Action).

fake_time() ->
	dummy.

create_test_population() ->
	[#population{
		id = ?POPULATION,
		species_ids = [?SPECIES1, ?SPECIES2]
	 },
	 #species{
		id = ?SPECIES1,
		population_id = ?POPULATION,
		agent_ids = [?AGENT1, ?AGENT2, ?AGENT3, ?AGENT4],
		innovation_factor = 100,
		fitness = {2, 2, 2, 2}
	 },
	 #species{
		id = ?SPECIES2,
		population_id = ?POPULATION,
		agent_ids = [?AGENT5, ?AGENT6, ?AGENT7, ?AGENT8],
		innovation_factor = 10,
		fitness = {5, 5, 5, 5}
	 },
	 #agent{
		id = ?AGENT1,
		cortex_id = ?CORTEX1,
		fitness = 4,
		generation = 0
	 },
	 #agent{
		id = ?AGENT2,
		cortex_id = ?CORTEX2,
		fitness = 25,
		generation = 0
	 },
	 #agent{
		id = ?AGENT3,
		cortex_id = ?CORTEX3,
		fitness = 5,
		generation = 0
	 },
	 #agent{
		id = ?AGENT4,
		cortex_id = ?CORTEX4,
		fitness = 25,
		generation = 0
	 },
	 #agent{
		id = ?AGENT5,
		cortex_id = ?CORTEX5,
		fitness = 4,
		generation = 0
	 },
	 #agent{
		id = ?AGENT6,
		cortex_id = ?CORTEX6,
		fitness = 30,
		generation = 0
	 },
	 #agent{
		id = ?AGENT7,
		cortex_id = ?CORTEX7,
		fitness = 500,
		generation = 0
	 },
	 #agent{
		id = ?AGENT8,
		cortex_id = ?CORTEX8,
		fitness = 5,
		generation = 0
	 },
	 #cortex{
		id = ?CORTEX1,
		neuron_ids = [{{0, n1}, neuron}]
	 },
	 #cortex{
		id = ?CORTEX2,
		neuron_ids = [{{0, n2}, neuron}, {{0, n3}, neuron}]
	 },
	 #cortex{
		id = ?CORTEX3,
		neuron_ids = [{{0, n4}, neuron}]
	 },
	 #cortex{
		id = ?CORTEX4,
		neuron_ids = [{{0, n5}, neuron}]
	 },
	 #cortex{
		id = ?CORTEX5,
		neuron_ids = [{{0, n6}, neuron}]
	 },
	 #cortex{
		id = ?CORTEX6,
		neuron_ids = [{{0, n7}, neuron}]
	 },
	 #cortex{
		id = ?CORTEX7,
		neuron_ids = [{{0, n8}, neuron}]
	 },
	 #cortex{
		id = ?CORTEX8,
		neuron_ids = [{{0, n9}, neuron}]
	 },
	 #neuron{
		id = {{0, n5}, neuron},
		cortex_id = ?CORTEX4
	 },
	 #neuron{
		id = {{0, n8}, neuron},
		cortex_id = ?CORTEX7
	 }].
