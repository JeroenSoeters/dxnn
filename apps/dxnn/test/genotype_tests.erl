-module(genotype_tests).
-include_lib("eunit/include/eunit.hrl").
-include("../src/records.hrl").

construct_and_clone_test() ->
	polis:start(),
	F = fun() ->
		AgentId = test,
		CloneId = test_clone,
		construct_and_clone_agent(AgentId, CloneId),
		Agent = genotype:read({agent, AgentId}),
		Clone = genotype:read({agent, CloneId}),
		cleanup_agents(AgentId, CloneId),
		?assert(Agent#agent.fingerprint == Clone#agent.fingerprint)
	end,
	polis:stop().

construct_and_clone_agent(AgentId, CloneId) ->
	SpeciesConstraint = #constraint{},
	genotype:construct_agent(AgentId, test, SpeciesConstraint),
	genotype:clone_agent(AgentId, CloneId).

cleanup_agents(AgentId, CloneId) ->
	genotype:delete_agent(AgentId),
	genotype:delete_agent(CloneId).
