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

-record(state, {op_mode, population_id, active_agent_ids_and_pids=[], agent_ids=[], total_agents, agents_left, op_tag, agent_summaries=[], pop_gen=0, eval_acc=0, cycle_acc=0, time_acc=0, step_size, next_step, goal_status, selection_algorithm}).

population_monitor_test_() ->
	{foreach,
	 fun setup/0,
	 fun teardown/1,
	 [fun ?MODULE:init_test_/1,
	  fun ?MODULE:an_agent_terminated_test_/1,
	  fun ?MODULE:extract_all_agent_ids_test_/1]}.

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
	meck:sequence(exoself, start, 1, [1, 2, 3, 4]),

	{ok, State} = population_monitor:init({gt, ?POPULATION, competition}),

	[?_assertEqual(gt, State#state.op_mode),
	 ?_assertEqual(?POPULATION, State#state.population_id),
	 ?_assert(lists:member({?AGENT1, 3}, State#state.active_agent_ids_and_pids)),
	 ?_assert(lists:member({?AGENT2, 4}, State#state.active_agent_ids_and_pids)),
	 ?_assert(lists:member({?AGENT3, 1}, State#state.active_agent_ids_and_pids)),
	 ?_assert(lists:member({?AGENT4, 2}, State#state.active_agent_ids_and_pids)),
	 ?_assertEqual(4, State#state.total_agents),
	 ?_assertEqual(4, State#state.agents_left),
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

extract_all_agent_ids_test_(_) ->
	AgentIds = population_monitor:extract_agent_ids(?POPULATION, all),
	
	 [?_assert(lists:member(?AGENT1, AgentIds)),
	 ?_assert(lists:member(?AGENT2, AgentIds)),
	 ?_assert(lists:member(?AGENT3, AgentIds)),
	 ?_assert(lists:member(?AGENT4, AgentIds))].

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
		agent_ids = [?AGENT1, ?AGENT2]
	 },
	 #species{
		id = ?SPECIES2,
		population_id = ?POPULATION,
		agent_ids = [?AGENT3, ?AGENT4]
	 }].
