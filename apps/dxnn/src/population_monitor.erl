-module(population_monitor).
-include("records.hrl").
-include_lib("eunit/include/eunit.hrl").
% API
-export([start_link/1, start_link/0, start/1, start/0, stop/0, init/2]).
% gen server callbacks
-export([init/1, handle_cast/2, extract_agent_ids/2]).
%-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3, create_mutant_agent_copy/1, test/0, create_species/3, continue/2, continue/3, init_population/1, extract_agent_ids/2, delete_population/1]).
%-behaviour(gen_server).
% exporting just for tests?
-export([extract_agent_ids/2, calculate_neural_energy_cost/1, construct_agent_summaries/1, calculate_alotments/2, mutate_population/3]).

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

handle_cast({AgentId, terminated, Fitness, Evals, Cycles, Time}, State) 
	when State#state.selection_algorithm == competition ->
	PopulationId = State#state.population_id,
	OpTag = State#state.op_tag,
	AgentsLeft = State#state.agents_left,
	OpMode = State#state.op_mode,
	UpdatedEvalAcc = State#state.eval_acc + Evals,
	UpdatedCycleAcc = State#state.cycle_acc + Cycles,
	UpdatedTimeAcc = State#state.time_acc + Time,
	case (AgentsLeft - 1) =< 0 of
		true ->
			% mutate population
			% UpdatedGeneration ++
			% when continue...
				% read species
				% foreach species read fitness
				% calculate best fitness
				% when stopping condition not reached...
					% extract agent ids
					% update agent ids and pids by summoning agents
					% update state
					{noreply, State};
		false ->
			UpdatedActiveAgentIdsAndPids = lists:keydelete(AgentId, 1, State#state.active_agent_ids_and_pids),
			{noreply, State#state{ 
				active_agent_ids_and_pids = UpdatedActiveAgentIdsAndPids,
				agents_left = AgentsLeft - 1,
				eval_acc = UpdatedEvalAcc,
				cycle_acc = UpdatedCycleAcc,
				time_acc = UpdatedTimeAcc
			}}
	end.

mutate_population(PopulationId, PopulationLimit, SelectionAlgorithm) ->
	% calclulate neural energy cost
	% in transaction do
		% read species (from population)
		% mutate species
	not_implemented.

mutate_species(SpeciesId, PopulationLimit, NeuralEnergyCost, SelectionAlgorithm) ->
	% read species
	% calculate species fitness
	% construct and sort agent summaries
	% when competition...
		% calculate # survivors
		% SDX = something
		% properly sort agent summaries ?!?! WTF
		% calculate valid agent summaries
		% calculate invalid agent summaries
		% obtain invalid agent ids
		% delete invalid agents
		% extract top agent ids
		% new agent ids with competition algorithm
	not_implemented.

calclulate_species_fitness(SpeciesId) ->
	not_implemented.

construct_agent_summaries(AgentIds) ->
	construct_agent_summaries(AgentIds, []).
construct_agent_summaries([AgentId|AgentIds], Acc) ->
	Agent = genotype:dirty_read({agent, AgentId}),
	Cortex = genotype:dirty_read({cortex, Agent#agent.cortex_id}),
	construct_agent_summaries(AgentIds, [{Agent#agent.fitness, length(Cortex#cortex.neuron_ids), AgentId}|Acc]);
construct_agent_summaries([], Acc) ->
	lists:reverse(Acc).

competition(SortedAgentSummaries, PopulationLimit, NeuralEnergyCost) ->
	% calculate alotments p and next generation size estimate
	% calculate normalizer
	% gather survivors
	not_implemented.

calculate_neural_energy_cost(PopulationId) ->
	AgentIds = extract_agent_ids(PopulationId, all),
	Agents = [genotype:dirty_read({agent, AgentId}) || AgentId <- AgentIds],
	TotalFitness = lists:sum([Agent#agent.fitness || Agent <- Agents]),
	TotalNeurons = lists:sum([get_neuron_count(Agent#agent.cortex_id) || Agent <- Agents]),
	TotalFitness / TotalNeurons.

get_neuron_count(CortexId) ->
	length((genotype:dirty_read({cortex, CortexId}))#cortex.neuron_ids).

calculate_alotments(SortedAgentSummaries, NeuralEnergyCost) ->
	calculate_alotments(SortedAgentSummaries, NeuralEnergyCost, [], 0).
calculate_alotments([{Fitness, TotalNeurons, AgentId}|SortedAgentSummaries], NeuralEnergyCost, Acc, EstimatedPopulationSizeAcc) ->
	NeuralAlotment = Fitness / NeuralEnergyCost,
	MutantAlotment = NeuralAlotment / TotalNeurons,
	UpdatedEstimatedPopulationSizeAcc = EstimatedPopulationSizeAcc + MutantAlotment,
	calculate_alotments(SortedAgentSummaries, NeuralEnergyCost, [{MutantAlotment, Fitness, TotalNeurons, AgentId}|Acc], UpdatedEstimatedPopulationSizeAcc); 
calculate_alotments([], _NeuralEnergyCost, Acc, EstimatedPopulationSize) ->
	{lists:reverse(Acc), EstimatedPopulationSize}.

	
	
