-record(sensor, {id, cortex_id, name, scape, vl, fanout_ids=[], generation}).
-record(actuator, {id, cortex_id, name, scape, vl, fanin_ids=[], generation}).
-record(neuron, {id, generation, cortex_id, af, input_ids_plus_weights=[], output_ids=[], recursive_output_ids=[]}).
-record(cortex, {id, agent_id, neuron_ids=[], sensor_ids=[], actuator_ids=[]}).
-record(agent, {id, generation, population_id, species_id, cortex_id, fingerprint, constraint, evo_hist=[], fitness, innovation_factor=0, pattern=[]}).
-record(species, {id, population_id, fingerprint, constraint, agent_ids=[], champion_ids=[], fitness, innovation_factor=0}).
-record(population, {id, polis_id, species_ids=[], morphologies=[], innovation_factor}).
-record(constraint, {
	morphology=xor_mimic, 
	connection_architecture=recurrent, % recurrent|feed_forward
	neural_afs=[tanh, cos, gaussian, absolute]
}).
