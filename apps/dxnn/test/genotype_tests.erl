-module(genotype_tests).
-include_lib("eunit/include/eunit.hrl").
-include("../src/records.hrl").

genotype_test_xxx() ->
	polis:start(),
	
	SpecieId = test,
	AgentId = test,
	ClonedAgentId = test_clone,
	SpeciesConstraint = #constraint{},
	%mnesia:transaction(fun() ->
	%	genotype:construct_agent(SpecieId, AgentId, SpeciesConstraint),
	%	genotype:clone_agent(genotype:clone_agent) end),

	polis:stop().

create_agent_test_xxx() ->
	%AgentId = test,
	%genotype:construct_agent(SpecieId, AgentId, SpeciesConstraint),
	%{Agent, Cortex} = genotype:read_agent(AgentId),
	?assert(stuff_on_agent),
	?assert(stuff_on_cortex),
	?assert(true).
