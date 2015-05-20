-module(population_monitor_tests).
-compile(export_all).
-include_lib("eunit/include/eunit.hrl").
-include("../src/records.hrl").
-define(POPULATION, p1).
-define(SPECIES1, s1).
-define(SPECIES2, s2).
-define(AGENT1, {a1, agent}).
-define(AGENT2, {a2, agent}).
-define(AGENT3, {a3, agent}).
-define(AGENT4, {a4, agent}).
-define(AGENT5, {a5, agent}).
-define(AGENT6, {a6, agent}).
-define(AGENT7, {a7, agent}).
-define(AGENT8, {a8, agent}).
-define(CORTEX1, {c1, cortex}).
-define(CORTEX2, {c2, cortex}).
-define(CORTEX3, {c3, cortex}).
-define(CORTEX4, {c4, cortex}).
-define(CORTEX5, {c5, cortex}).
-define(CORTEX6, {c6, cortex}).
-define(CORTEX7, {c7, cortex}).
-define(CORTEX8, {c8, cortex}).

-record(state, {op_mode, population_id, active_agent_ids_and_pids=[], agent_ids=[], total_agents, agents_left, op_tag, agent_summaries=[], pop_gen=0, eval_acc=0, cycle_acc=0, time_acc=0, step_size, next_step, goal_status, selection_algorithm}).

population_monitor_test_() ->
	{foreach,
	 fun setup/0,
	 fun teardown/1,
	 [fun ?MODULE:init_test_/1,
	  fun ?MODULE:an_agent_terminated_test_/1,
	  fun ?MODULE:extract_all_agent_ids_test_/1,
	  fun ?MODULE:calculate_neural_energy_cost_test_/1,
	  fun ?MODULE:construct_agent_summaries_test_/1,
	  fun ?MODULE:calculate_alotments_test_/1,
	  fun ?MODULE:mutate_population_test_/1]}.

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

	?debugFmt("\n >>> State: ~p <<< \n", [State]),

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
	
	not_implemented.

last_agent_terminated_then_continue_end_condition_reached_test_(_) ->
	not_implemented.

last_agent_terminated_then_pause_test_(_) ->
	not_implemented.

last_agent_terminated_then_done_test_(_) ->
	not_implemented.

mutate_population_test_(_) ->
	FakeTimeProvider = fun() -> {0, 8, 0} end,
	
	population_monitor:mutate_population(),

	not_implemented.

extract_all_agent_ids_test_(_) ->
	AgentIds = population_monitor:extract_agent_ids(?POPULATION, all),
	
	 [?_assert(lists:member(?AGENT1, AgentIds)),
	 ?_assert(lists:member(?AGENT2, AgentIds)),
	 ?_assert(lists:member(?AGENT3, AgentIds)),
	 ?_assert(lists:member(?AGENT4, AgentIds))].

calculate_neural_energy_cost_test_(_) ->
	NeuralEnergyCost = population_monitor:calculate_neural_energy_cost(?POPULATION),
		
	?_assertEqual(67.0, NeuralEnergyCost).

construct_agent_summaries_test_(_) ->
	AgentSummaries = population_monitor:construct_agent_summaries([?AGENT1, ?AGENT2]),
	
	?_assertEqual([{4, 1, ?AGENT1}, {30, 2, ?AGENT2}], AgentSummaries).

calculate_alotments_test_(_) ->
	{AlotmentsPlusAgentSummaries, EstimatedPopulationSize} = 
		population_monitor:calculate_alotments(population_monitor:construct_agent_summaries([?AGENT1, ?AGENT2]), 2),

	[?_assertEqual([{2.0, 4, 1, ?AGENT1}, {7.5, 30, 2, ?AGENT2}], AlotmentsPlusAgentSummaries),
	 ?_assertEqual(9.5, EstimatedPopulationSize)].

in_transaction(Action) ->
	{atomic, _} = mnesia:sync_transaction(Action).

create_test_population() ->
	[#neuron{
		id = {{1, 1}, neuron}
	 },
	 #population{
		id = ?POPULATION,
		species_ids = [?SPECIES1, ?SPECIES2]
	 },
	 #species{
		id = ?SPECIES1,
		population_id = ?POPULATION,
		agent_ids = [?AGENT1, ?AGENT2, ?AGENT3, ?AGENT4]
	 },
	 #species{
		id = ?SPECIES2,
		population_id = ?POPULATION,
		agent_ids = [?AGENT5, ?AGENT6, ?AGENT7, ?AGENT8]
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
		fitness = 30,
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
	 }].
