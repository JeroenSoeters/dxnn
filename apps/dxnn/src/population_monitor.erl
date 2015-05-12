-module(population_monitor).
-include("records.hrl").
-include_lib("eunit/include/eunit.hrl").
% API
-export([start_link/1, start_link/0, start/1, start/0, stop/0, init/2]).
% gen server callbacks
-export([init/1]).
%-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3, create_mutant_agent_copy/1, test/0, create_species/3, continue/2, continue/3, init_population/1, extract_agent_ids/2, delete_population/1]).
%-behaviour(gen_server).

% Population monitor options and parameters
-define(SELECTION_ALGORITHM, competition).
-define(EFF, 0.05).
-define(INIT_CONSTRAINTS,
	[#constraint{morphology=Morhology, neural_afs=NeuralAfs} || Morphology <- [xor_mimic], NeuralAfs <- [[tanh]]]).
-define(SURVIVAL_PERCENTAGE, 0.5).
-define(SPECIES_SIZE_LIMIT, 10).
-define(INIT_SPECIES_SIZE, 10).
-define(INIT_POPULATION_ID, test).
-define(OP_MODE, gt).
-define(INIT_POLIS, mathema).
-define(GENERATION_LIMIT, 100).
-define(EVALUATIONS_LIMIT, 100000).
-define(DIVERSITY_COUNT_STEP, 500).
-define(GEN_UID, genotype:generate_unique_id()).
-define(CHAMPION_COUNT_STEP, 500).
-define(FITNESS_GOAL, inf).
-record(state, {op_mode, population_id, active_agent_ids_and_pids=[], agent_ids=[], total_agents, agents_left, op_tag, agent_summaries=[], pop_gen=0, eval_acc=0, cycle_acc=0, time_acc=0, step_size, next_step, goal_status, selection_algorithm}).

start_link(StartParameters) ->
	gen_server:start_link(?MODULE, StartParameters, []).

start(StartParameters) ->
	gen_server:start_link(?MODULE, StartParameters, []).

start_link() ->
	gen_server:start_link(?MODULE, [], []).

start() ->
	gen_server:start_link(?MODULE, [], []).

stop() ->
	gen_server:cast(monitor, {stop, normal}).

init(Pid, InitState) ->
	gen_server:cast(Pid, {init, InitState}).


%% ===================================================================
%% gen_server callbacks
%% ===================================================================

init(Parameters) ->
	%process_flag(trap_exit, true),
	%register(monitor, self()),
	io:format("******** Population monitor started with parameters:~p~n", [Parameters]),
	{OpMode, PopulationId, SelectionAlgorithm} = Parameters,
	AgentIds = extract_agent_ids(PopulationId, all),
	ActiveAgentIdsAndPids = summon_agents(OpMode, AgentIds),
	State = #state{
		op_mode = OpMode,
		population_id = PopulationId,
		active_agent_ids_and_pids = ActiveAgentIdsAndPids,
		total_agents = length(AgentIds),
		agents_left = length(AgentIds),
		op_tag = continue,
		selection_algorithm = SelectionAlgorithm
	},
	{ok, State}.

extract_agent_ids(PopulationId, AgentType) ->
	Population = genotype:dirty_read({population, PopulationId}),
	SpeciesIds = Population#population.species_ids,
	case AgentType of
		champion ->
			not_implemented;
		all ->
			extract_all_agent_ids(SpeciesIds)
	end.

extract_all_agent_ids(SpeciesIds) ->
	extract_all_agent_ids(SpeciesIds, []).
extract_all_agent_ids([SpeciesId|SpeciesIds], Acc) ->
	Species = genotype:dirty_read({species, SpeciesId}),
	extract_all_agent_ids(SpeciesIds, lists:append(Species#species.agent_ids, Acc));
extract_all_agent_ids([], Acc) ->
	Acc.

summon_agents(OpMode, AgentIds) ->
	summon_agents(OpMode, AgentIds, []).
summon_agents(OpMode, [AgentId|AgentIds], Acc) ->
	Pid = exoself:start(AgentId),
	summon_agents(OpMode, AgentIds, [{AgentId, Pid}|Acc]);
summon_agents(_OpMode, [], Acc) ->
	Acc.
