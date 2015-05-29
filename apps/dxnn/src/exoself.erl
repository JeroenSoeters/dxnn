-module(exoself).
-compile(export_all).
-include("records.hrl").
-record(state, {file_name, genotype, ids_and_pid, cortex_pid, sensor_pids, neuron_pids, actuator_pids, best_fitness, tot_evals, tot_cycles}). 
-define(MAX_ATTEMPTS, 50).

start(AgentId) ->
	case whereis(monitor) of
		undefined ->
			io:format("start(Agent_Id):: 'monitor' is not registered~n");
		Pid ->
			start(AgentId, Pid)
	end.
start(AgentId, PopulationMonitorPid) ->
	spawn(exoself, prep, [AgentId, PopulationMonitorPid]).

prep(AgentId, PopulationMonitorPid) ->
	random:seed(now()),
	IdsAndPids = ets:new(ids_and_pids, [set, private]),
	Agent = genotype:dirty_read({agent, AgentId}),
	Cortex = genotype:dirty_read({cortex, Agent#agent.cortex_id}),
	SensorIds   = Cortex#cortex.sensor_ids,
	ActuatorIds = Cortex#cortex.actuator_ids,
	NeuronIds   = Cortex#cortex.neuron_ids,
	ScapePids = spawn_scapes(IdsAndPids, SensorIds, ActuatorIds),
	spawn_cerebral_units(IdsAndPids, cortex, [Cortex#cortex.id]),
	spawn_cerebral_units(IdsAndPids, sensor, SensorIds),
	spawn_cerebral_units(IdsAndPids, actuator, ActuatorIds),
	spawn_cerebral_units(IdsAndPids, neuron, NeuronIds),
	link_sensors(SensorIds, IdsAndPids),
	link_actuators(ActuatorIds, IdsAndPids),
	link_neurons(NeuronIds, IdsAndPids),
	{SensorPids, NeuronPids, ActuatorPids} = link_cortex(Cortex, IdsAndPids),
	CortexPid = ets:lookup_element(IdsAndPids, Cortex#cortex.id, 2),
	loop(AgentId, PopulationMonitorPid, IdsAndPids, CortexPid, SensorPids, NeuronPids, ActuatorPids, ScapePids, 0, 0, 0, 0, 1).
%Once the FileName and the genotype are dropped into the prep/2 function the function uses the current time to create a new random seed. Then the cortex is extracted from the genotype and the Sensor, Actuator and Neural Ids are extracted from it. The sensors and actuators are dropped into the spawn_scapes/4, which extracts the scapes that need to be spawned, and then spawns them. Afterwards, the sensor, actuator, neuron and the cortex elements are spawned. Then the expself process sends these spawned elements the Pids of the elements they are connected to, thus linking all the elements together into a proper interconnected structure. The cortex element is the last one to be linked, because once it receives the message from the exoself with all the data, it immediately starts synchronizing the NN by prompting the sensors to action. Afterwards, prep/2 drops into the exoself's main process loop.

loop(AgentId, PopulationMonitorPid, IdsAndPids, CortexPid, SensorPids, NeuronPids, ActuatorPids, ScapePids, HighestFitness, EvalAcc, CycleAcc, TimeAcc, Attempt) ->
	receive
		{CortexPid, evaluation_completed, Fitness, Cycles, Time}->
			{UpdatedHighestFitness, UpdatedAttempt} = case Fitness > HighestFitness of
				true ->
					[NeuronPid ! {self(), weight_backup} || NeuronPid <- NeuronPids],
					{Fitness, 0};
				false ->
					Perturbed_NeuronPids = get(perturbed),
					[NeuronPid ! {self(), weight_restore} || NeuronPid <- Perturbed_NeuronPids],
					{HighestFitness, Attempt+1}
			end,
			[Pid ! {self(), reset_prep} || Pid <- NeuronPids],
			gather_acks(length(NeuronPids)),
			[Pid ! {self(), reset} || Pid <- NeuronPids],
			case UpdatedAttempt >= ?MAX_ATTEMPTS of
				true -> %end training
					UpdatedCycleAcc = CycleAcc + Cycles,
	  				UpdatedTimeAcc = TimeAcc + Time,
					Agent = genotype:dirty_read({agent, AgentId}),
					genotype:write(Agent#agent{fitness = UpdatedHighestFitness}),
					backup_genotype(IdsAndPids, NeuronPids),
					terminate_phenotype(CortexPid, SensorPids, NeuronPids, ActuatorPids, ScapePids),
					io:format("~nCortex:~p finished training. Genotype has been backed up.~n Fitness:~p~n TotEvaluations:~p~n TotCycles:~p~n TimeAcc:~p~n", [CortexPid, UpdatedHighestFitness, EvalAcc, UpdatedCycleAcc, UpdatedTimeAcc]),
					gen_server:cast(PopulationMonitorPid, {self(), terminated, UpdatedHighestFitness, EvalAcc + 1, UpdatedCycleAcc, UpdatedTimeAcc});
				false -> %continue training
					TotNeurons = length(NeuronPids),
					MP = 1/math:sqrt(TotNeurons),
					PerturbNeuronPids = [Pid || Pid <- NeuronPids, random:uniform() < MP],
					put(perturbed, PerturbNeuronPids),
					[NeuronPid ! {self(), weight_perturb} || NeuronPid <- PerturbNeuronPids],
					CortexPid ! {self(), reactivate},
					loop(AgentId, PopulationMonitorPid, IdsAndPids, CortexPid, SensorPids, NeuronPids, ActuatorPids, ScapePids, UpdatedHighestFitness, EvalAcc+1, CycleAcc+Cycles, TimeAcc+Time, UpdatedAttempt)
			end
	end.
%The main process loop waits for the NN to complete the task, receive its fitness score, and send Exoself the: {CortexPid,evaluation_completed,Fitness,Cycles,Time} message. The message contains all the information about that particular evaluation, the acquired fitness score, the number of total Sense-Think-Act cycles executed and the time it took to complete the evaluations. The exoself then compares the Fitness to the one it has on record (if any) and based on that decides whether to revert the previously perturbed neurons back to their original state or not. If the new Fitness is lower, then the perturbed neurons are contacted and their weights are reverted. If the new Fitness is greater than the one stored on record, then the NN is backed up to file, and the variable EvalAcc is reset to 0. Finally, depending on whether the NN has failed to improve its fitness Max_Attempts number of times, the exoself decides whether anpther pertubation attempt is warranted. If it is warranted, then the exoself chooses which neurons to mutate by randomly choosing each neuron with the probability of 1/sqrt(TotNeurons) where TotNeurons is the total number of neurons in the neural network. The exoself saves the Pids of those chosen neurons to process dictionary, and then sends those neurons a signal that they should perturb their weights. Finally it tells cortex to reactivate and start syncing the sensors and actuators again. But of the NN has failed to improve its fitness for Max_Attempts number of times, if EvalAcc > Max_Attempts, then the exoself terminates all the elements in the NN and if there is a registered process by the name trainer, the exoself sends it the HighestFitness score that its NN achieved and the number of total evaluations it took to achieve it.

spawn_cerebral_units(IdsAndPids, CerebralUnitType, [Id|Ids]) ->
	Pid = CerebralUnitType:gen(self(), node()),
	ets:insert(IdsAndPids,{Id, Pid}), 
	ets:insert(IdsAndPids, {Pid, Id}),
	spawn_cerebral_units(IdsAndPids, CerebralUnitType, Ids);
spawn_cerebral_units(_IdsAndPids, _CerebralUnitType, []) ->
	true.
%We spawn the process for each element based on its type: CerebralUnitType, and the gen function that belongs to the CerebralUnitType module. We then enter the {Id,Pid} tuple into our ETS table for later use.

spawn_scapes(IdsAndPids, SensorIds, ActuatorIds) ->
	SensorScapes = [(genotype:dirty_read({sensor, Id}))#sensor.scape || Id <- SensorIds],
	ActuatorScapes = [(genotype:dirty_read({actuator, Id}))#actuator.scape || Id <- ActuatorIds],
	UniqueScapes = SensorScapes ++ (ActuatorScapes -- SensorScapes),
	SN_Tuples = [{scape:gen(self(), node()), ScapeName} || {private, ScapeName} <- UniqueScapes],
	[ets:insert(IdsAndPids, {ScapeName, Pid}) || {Pid, ScapeName} <- SN_Tuples],
	[ets:insert(IdsAndPids, {Pid, ScapeName}) || {Pid, ScapeName} <- SN_Tuples],
	[Pid ! {self(), ScapeName} || {Pid, ScapeName} <- SN_Tuples],
	[Pid || {Pid, _ScapeName} <- SN_Tuples].
%spawn_scapes/4 first extracts all the scape names from sensors and actuators, then builds a set of unique scapes, and then finally sctracts and spawns the private scapes. The public scapes are not spawned since they are independent of the NN, and should already be running. The reason for extracting the list of unique scapes is because if both a sensor and an actuator are pointing to the same scape, then that means they will interfere with the same scape, and it does not mean that each one should spawn its own scape of the same name. Afterwards we use the IdsAndPids ETS table to create a map from scape Pid to scape name, and from scape name to scape Pid for later use. The function then sends each spawned scape a message composed of the exoself's Pid and the scape's name {self(),ScapeName}. Finally a spawned scape Pid list is composed and returned to the caller. 

link_sensors([SensorId|SensorIds], IdsAndPids) ->
	Sensor = genotype:dirty_read({sensor, SensorId}),
	SensorPid = ets:lookup_element(IdsAndPids, SensorId, 2),
	CortexPid = ets:lookup_element(IdsAndPids, Sensor#sensor.cortex_id, 2),
	SName = Sensor#sensor.name,
	FanoutIds = Sensor#sensor.fanout_ids,
	FanoutPids = [ets:lookup_element(IdsAndPids, Id, 2) || Id <- FanoutIds],
	Scape = case Sensor#sensor.scape of
		{private, ScapeName} ->
			ets:lookup_element(IdsAndPids, ScapeName, 2)
	end,
	SensorPid ! {self(), {SensorId, CortexPid, Scape, SName, Sensor#sensor.vl, FanoutPids}},
	link_sensors(SensorIds, IdsAndPids);
link_sensors([], _IdsAndPids) ->
	ok.

link_actuators([ActuatorId|ActuatorIds], IdsAndPids) ->
	Actuator = genotype:dirty_read({actuator, ActuatorId}),
	ActuatorPid = ets:lookup_element(IdsAndPids, ActuatorId, 2),
	CortexPid = ets:lookup_element(IdsAndPids, Actuator#actuator.cortex_id, 2),
	AName = Actuator#actuator.name,
	FaninIds = Actuator#actuator.fanin_ids,
	FaninPids = [ets:lookup_element(IdsAndPids, Id, 2) || Id <- FaninIds],
	Scape = case Actuator#actuator.scape of
		{private, ScapeName} ->
			ets:lookup_element(IdsAndPids, ScapeName, 2)
	end,
	ActuatorPid ! {self(), ActuatorId, CortexPid, Scape, AName, FaninPids},
	link_actuators(ActuatorIds, IdsAndPids);
link_actuators([], _IdsAndPids) ->
	ok.

link_neurons([NeuronId|NeuronIds], IdsAndPids) ->
	Neuron = genotype:dirty_read({neuron, NeuronId}),
	NeuronPid = ets:lookup_element(IdsAndPids, NeuronId, 2),
	CortexPid = ets:lookup_element(IdsAndPids, Neuron#neuron.cortex_id, 2),
	AFName = Neuron#neuron.af,
	InputIdsPlusWeights = Neuron#neuron.input_ids_plus_weights,
	OutputIds = Neuron#neuron.output_ids,
	RecursiveOutputIds = Neuron#neuron.recursive_output_ids,
	InputPidsPlusWeights = convert_IdPs2PidPs(IdsAndPids, InputIdsPlusWeights, []),
	OutputPids = [ets:lookup_element(IdsAndPids, Id, 2) || Id <- OutputIds],
	RecursiveOutputPids = [ets:lookup_element(IdsAndPids, Id, 2) || Id <- RecursiveOutputIds],
	NeuronPid ! {self(), {NeuronId, CortexPid, AFName, InputPidsPlusWeights, OutputPids, RecursiveOutputPids}},
	link_neurons(NeuronIds, IdsAndPids);
link_neurons([], _IdsAndPids) ->
	ok.

convert_IdPs2PidPs(_IdsAndPids, [{bias, Bias}], Acc) ->
	lists:reverse([Bias|Acc]);
convert_IdPs2PidPs(IdsAndPids, [{Id, Weights}|Fanin_IdPs], Acc) ->
	convert_IdPs2PidPs(IdsAndPids, Fanin_IdPs, [{ets:lookup_element(IdsAndPids, Id, 2), Weights}|Acc]);
convert_IdPs2PidPs(_IdsAndPids, [], Acc) ->
	lists:reverse(Acc).
%Rhw link_CerebralUnits/2 converts the Ids to Pids using the created IdsAndPids ETS table. At this point all the elements are spawned, and the processes are waiting for their inital states. convert_IdPs2PidPs/3 converts the IdPs tuples into tuples that use Pids instead of Ids, such that the Neuron will know which weights are to be associated with which incoming vector signals. The last element is the bias, which is added to the list in a non tuple form. Afterwards, the list is reversed to take its proper order.

link_cortex(Cortex, IdsAndPids) ->
	Cortex_Id = Cortex#cortex.id,
	CortexPid = ets:lookup_element(IdsAndPids, Cortex_Id, 2),
	SensorIds = Cortex#cortex.sensor_ids,
	ActuatorIds = Cortex#cortex.actuator_ids,
	NeuronIds = Cortex#cortex.neuron_ids,
	SensorPids = [ets:lookup_element(IdsAndPids, Id, 2) || Id <- SensorIds],
	ActuatorPids = [ets:lookup_element(IdsAndPids, Id, 2) || Id <- ActuatorIds],
	NeuronPids = [ets:lookup_element(IdsAndPids, Id, 2) || Id <- NeuronIds],
	CortexPid ! {self(), Cortex_Id, SensorPids, NeuronPids, ActuatorPids},
	{SensorPids, NeuronPids, ActuatorPids}.
%The cortex is initialized to its proper state just as other elements. Because we have not yet implemented a learning algorithm for our NN system, we need to specify when the NN should shutdown. We do this by specifying the total number of cycles the NN should execute before terminating, which is 1000 in this case.

backup_genotype(IdsAndPids, NeuronPids)->
	NeuronIdsNWeights = get_backup(NeuronPids, []),
	update_genotype(IdsAndPids, NeuronIdsNWeights), 
	io:format("~nFinished updating genotype").

	get_backup([NeuronPid|NeuronPids], Acc)->
		NeuronPid ! {self(), get_backup},
			receive
				{NeuronPid, NeuronId, WeightTuples}->
					get_backup(NeuronPids, [{NeuronId, WeightTuples}|Acc])
			end;
	get_backup([], Acc)->
		Acc.
%The backup_genotype/4 uses get_backup/2 to contact all the neurons in its NN and request for the neuron's Ids and their InputIdsPlusWeights. Once the updated InputIdsPlusWeights from all the neurons have been accumulated, they are passed through the update_genotype/3 function to produce the genotype with updated weights, which is then saved to file.

update_genotype(IdsAndPids, [{NeuronId, PidPs}|WeightPs]) ->
	Neuron = genotype:dirty_read({neuron, NeuronId}),
	io:format("PidPs:~p~n", [PidPs]),
	UpdatedInputIdsPlusWeights = convert_PidPs2IdPs(IdsAndPids, PidPs, []),
	UpdatedNeuron = Neuron#neuron{input_ids_plus_weights=UpdatedInputIdsPlusWeights},
	UpdatedGenotype = genotype:write(UpdatedNeuron),
	update_genotype(IdsAndPids, WeightPs);
update_genotype(_IdsAndPids, []) ->
	ok.

convert_PidPs2IdPs(IdsAndPids, [{Pid, Weights}|InputPidsPlusWeights], Acc) ->
	convert_PidPs2IdPs(IdsAndPids, InputPidsPlusWeights, [{ets:lookup_element(IdsAndPids, Pid, 2), Weights}|Acc]);
convert_PidPs2IdPs(_IdsAndPids, [Bias], Acc) ->
	lists:reverse([{bias, Bias}|Acc]);
convert_PidPs2IdPs(_IdsAndPids, [], Acc) ->
	lists:reverse(Acc).
%For every {NeuronId,PidPs} tuple the update_genotype/3 function extracts the neuron with the id: NeuronId and updates its weights. The convert_PidPs2IdPs/3 performs the conversion from PidPs to Ids of every {Pid,Weights} tuple in the InputPidsPlusWeights list. The updated Genotype is then returned back to the caller.

terminate_phenotype(CortexPid, SensorPids, NeuronPids, ActuatorPids, ScapePids) ->
	io:format("~nTerminating the phenotype:~nCx_PId:~p~nSPIds:~p~nNPIds:~p~nAPIds:~p~nScapePids:~p~n", [CortexPid, SensorPids,NeuronPids, ActuatorPids, ScapePids]),
	[Pid ! {self(), terminate} || Pid <- SensorPids], 
	[Pid ! {self(), terminate} || Pid <- NeuronPids],
	[Pid ! {self(), terminate} || Pid <- ActuatorPids],
	[Pid ! {self(), terminate} || Pid <- ScapePids],
	CortexPid ! {self(), terminate}.

gather_acks(0)->
	done;	
gather_acks(Index)->
	receive
		{_From,ready}->
			gather_acks(Index-1)
		after 100000 ->
			io:format("******** Not all acks received:~p~n",[Index])
	end.

