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
-export([extract_agent_ids/2, calculate_neural_energy_cost/1, construct_agent_summaries/1, calculate_alotments/2, calculate_species_fitness/1, mutate_population/4, best_fitness/1]).

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
-record(state, {op_mode, population_id, active_agent_ids_and_pids=[], agent_ids=[], total_agents, agents_left, op_tag, agent_summaries=[], pop_gen=0, eval_acc=0, cycle_acc=0, time_acc=0, step_size, next_step, goal_status, selection_algorithm, population_limit, time_provider}).
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
			{atomic, _} = mutate_population(PopulationId, State#state.population_limit, competition, State#state.time_provider),
			UpdatedPopGen = State#state.pop_gen + 1,
			io:format("Population Generation:~p Ended.~n~n~n", [UpdatedPopGen]),
			case OpTag of 
				continue ->
					% read species
					% foreach species read fitness
					% calculate best fitness
					case (UpdatedPopGen >= ?GENERATION_LIMIT) or (State#state.eval_acc >= ?EVALUATIONS_LIMIT) or (best_fitness(PopulationId) > ?FITNESS_GOAL) of
						true ->
							not_implemented;
						false ->
							% update agent ids and pids by summoning agents
							AgentIds = extract_agent_ids(PopulationId, all),
							ActiveAgentIdsAndPids = summon_agents(OpMode, AgentIds),
							TotalAgents = length(AgentIds),
							UpdatedState = State#state{
								active_agent_ids_and_pids = ActiveAgentIdsAndPids,
								agent_ids = AgentIds,
								total_agents = TotalAgents,
								agents_left = TotalAgents,
								pop_gen = UpdatedPopGen,
								eval_acc = UpdatedEvalAcc,
								cycle_acc = UpdatedCycleAcc,
								time_acc = UpdatedTimeAcc
							},
							{noreply, UpdatedState}
					end
			end;
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

best_fitness(PopulationId) ->
	SpeciesIds = (genotype:dirty_read({population, PopulationId}))#population.species_ids,
	FitnessScores = [(genotype:dirty_read({species, SpeciesId}))#species.fitness || SpeciesId <- SpeciesIds],
	?debugFmt("\nFitness scores: ~p\n", [SpeciesIds]),
	lists:nth(1, lists:reverse(lists:sort([MaxFitness || {_, _, _, MaxFitness} <- FitnessScores]))).

mutate_population(PopulationId, PopulationLimit, SelectionAlgorithm, TimeProvider) ->
	NeuralEnergyCost = population_monitor:calculate_neural_energy_cost(PopulationId),
	F = fun() ->
		Population = genotype:read({population, PopulationId}),
		SpeciesIds = Population#population.species_ids,
		[mutate_species(Id, PopulationLimit, NeuralEnergyCost, SelectionAlgorithm, TimeProvider) || Id <- SpeciesIds]
	end,
	{atomic, _} = mnesia:transaction(F).

mutate_species(SpeciesId, PopulationLimit, NeuralEnergyCost, SelectionAlgorithm, TimeProvider) ->
	Species = genotype:dirty_read({species, SpeciesId}),
	{AvgFitness, StdFitness, MinFitness, MaxFitness} = calculate_species_fitness(SpeciesId),
	SortedAgentSummaries = lists:reverse(lists:sort(construct_agent_summaries(Species#species.agent_ids))),
	io:format("Selection Algorirthm:~p~n", [SelectionAlgorithm]),
	case SelectionAlgorithm of
		competition ->
			TotalSurvivors = round(length(SortedAgentSummaries) * ?SURVIVAL_PERCENTAGE),
			SDX = [{Fitness/math:pow(TotalNeurons, ?EFF), {Fitness, TotalNeurons, AgentId}} ||
				{Fitness, TotalNeurons, AgentId} <- SortedAgentSummaries],
			ProperlySortedAgentSummaries = [Summary || {_, Summary} <- SDX],
			ValidAgentSummaries = lists:sublist(ProperlySortedAgentSummaries, TotalSurvivors),
			InvalidAgentSummaries = ProperlySortedAgentSummaries -- ValidAgentSummaries,
			[genotype:delete_agent(AgentId) || {_, _, AgentId} <- InvalidAgentSummaries],
			io:format("Valid_AgentSummaries:~p~n",[ValidAgentSummaries]),
			io:format("Invalid_AgentSummaries:~p~n",[InvalidAgentSummaries]), 
			ChampionSummaries = lists:sublist(ValidAgentSummaries, 3),
			{_, _, ChampionIds} = lists:unzip3(ChampionSummaries),
			io:format("NeuralEnergyCost:~p~n",[NeuralEnergyCost]),
			NewAgentIds = competition(ValidAgentSummaries, PopulationLimit, NeuralEnergyCost, TimeProvider)
	end,
	{FitnessScores, _, _} = lists:unzip3(SortedAgentSummaries),
		[TopFitness|_] = FitnessScores,
		UpdatedInnovationFactor = case TopFitness > Species#species.innovation_factor of
			true ->
				0;
			false ->
				Species#species.innovation_factor-1
		end,
	UpdatedSpecies = Species#species{
		agent_ids = NewAgentIds,
		champion_ids = ChampionIds,
		fitness = {AvgFitness, StdFitness, MinFitness, MaxFitness},
		innovation_factor = UpdatedInnovationFactor
	},
	genotype:write(UpdatedSpecies).

calculate_species_fitness(SpeciesId) ->
	Species = genotype:dirty_read({species, SpeciesId}),
	SortedFitnessScores = lists:sort(gather_fitness_scores(Species#species.agent_ids, [])),
	Average = functions:avg(SortedFitnessScores),
	StandardDeviation = functions:std(SortedFitnessScores),
	[Minimum|_] = SortedFitnessScores,
	[Maximum|_] = lists:reverse(SortedFitnessScores),
	{Average, StandardDeviation, Minimum, Maximum}.

gather_fitness_scores([AgentId|AgentIds], Acc) ->
	Agent = genotype:dirty_read({agent, AgentId}),
	gather_fitness_scores(AgentIds, [Agent#agent.fitness|Acc]);
gather_fitness_scores([], Acc) ->
	Acc.

construct_agent_summaries(AgentIds) ->
	construct_agent_summaries(AgentIds, []).
construct_agent_summaries([AgentId|AgentIds], Acc) ->
	Agent = genotype:dirty_read({agent, AgentId}),
	Cortex = genotype:dirty_read({cortex, Agent#agent.cortex_id}),
	construct_agent_summaries(AgentIds, [{Agent#agent.fitness, length(Cortex#cortex.neuron_ids), AgentId}|Acc]);
construct_agent_summaries([], Acc) ->
	lists:reverse(Acc).

competition(SortedAgentSummaries, PopulationLimit, NeuralEnergyCost, TimeProvider) ->
	{Alotments, EstimatedPopulationSize} = calculate_alotments(SortedAgentSummaries, NeuralEnergyCost),
	Normalizer = EstimatedPopulationSize / PopulationLimit,
	io:format("Population size normalizer:~p~n", [Normalizer]),
	gather_survivors(Alotments, Normalizer, TimeProvider).

gather_survivors(Alotments, Normalizer, TimeProvider) ->
	gather_survivors(Alotments, Normalizer, TimeProvider, []).
gather_survivors([{MutantAlotment, Fitness, TotalNeurons, AgentId}|Alotments], Normalizer, TimeProvider, Acc) ->
	NormalizedMutantAlotment = round(MutantAlotment/Normalizer),
	io:format("Agent_Id:~p Normalized MutantAlotment:~p~n", [AgentId, NormalizedMutantAlotment]),
	SurvivorAgentIds = case NormalizedMutantAlotment >= 1 of
		true ->
			MutantAgentIds = case NormalizedMutantAlotment >= 2 of
				true ->
					[create_mutant_agent_copy(AgentId, TimeProvider) || _ <- lists:seq(1, NormalizedMutantAlotment -1)];
				false ->
					[]
			end,
			[AgentId|MutantAgentIds];
		false ->
			io:format("Deleting agent:~p~n", [AgentId]),
			genotype:delete_agent(AgentId),
			[]	
	end,
	gather_survivors(Alotments, Normalizer, TimeProvider, lists:append(SurvivorAgentIds, Acc));
gather_survivors([], _Normalizer, _TimeProvider, Acc) ->
	io:format("New Population:~p PopSize:~p~n", [Acc, length(Acc)]),
	Acc. 

create_mutant_agent_copy(AgentId, TimeProvider) ->
	CloneId = genotype:clone_agent(AgentId, TimeProvider),
	io:format("AgentClone_Id:~p~n", [CloneId]),
	genome_mutator:mutate(CloneId),
	CloneId.

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
	
