-module(exoself).
-compile(export_all).
-include("records.hrl").
-record(state,{file_name, genotype, ids_and_pid, cortex_pid, sensor_pids, neuron_pids, actuator_pids, best_fitness, tot_evals, tot_cycles}). 
-define(MAX_ATTEMPTS,50).

start(AgentId) ->
	case whereis(monitor) of
		undefined ->
			io:format("start(Agent_Id):: 'monitor' is not registered~n");
		Pid ->
			start(AgentId, Pid)
	end.
start(AgentId, PopulationMonitorPid) ->
	spawn(exoself, prep, [AgentId, PopulationMonitorPid]).

map() ->
	map(ffnn).
map(FileName) ->
	Genotype = genotype:load_from_file(FileName),
	spawn(exoself,prep,[FileName,Genotype]).

prep(FileName,Genotype) ->
	{V1,V2,V3} = now(),
	random:seed(V1,V2,V3),
	IdsAndPids = ets:new(idsNpids,[set,private]),
	Cx = genotype:read(Genotype,cortex),
	Sensor_Ids = Cx#cortex.sensor_ids,
	Actuator_Ids = Cx#cortex.actuator_ids,
	NIds = Cx#cortex.neuron_ids,
	ScapePIds = spawn_Scapes(IdsAndPids,Genotype,Sensor_Ids,Actuator_Ids),
	spawn_CerebralUnits(IdsAndPids,cortex,[Cx#cortex.id]),
	spawn_CerebralUnits(IdsAndPids,sensor,Sensor_Ids),
	spawn_CerebralUnits(IdsAndPids,actuator,Actuator_Ids),
	spawn_CerebralUnits(IdsAndPids,neuron,NIds),
	link_Sensors(Genotype,Sensor_Ids,IdsAndPids),
	link_Actuators(Genotype,Actuator_Ids,IdsAndPids),
	link_Neurons(Genotype,NIds,IdsAndPids),
	{SPIds,NPIds,APIds} = link_Cortex(Cx,IdsAndPids),
	Cx_PId = ets:lookup_element(IdsAndPids,Cx#cortex.id,2),
	loop(FileName,Genotype,IdsAndPids,Cx_PId,SPIds,NPIds,APIds,ScapePIds,0,0,0,0,1).
%Once the FileName and the genotype are dropped into the prep/2 function the function uses the current time to create a new random seed. Then the cortex is extracted from the genotype and the Sensor, Actuator and Neural Ids are extracted from it. The sensors and actuators are dropped into the spawn_Scapes/4, which extracts the scapes that need to be spawned, and then spawns them. Afterwards, the sensor, actuator, neuron and the cortex elements are spawned. Then the expself process sends these spawned elements the PIds of the elements they are connected to, thus linking all the elements together into a proper interconnected structure. The cortex element is the last one to be linked, because once it receives the message from the exoself with all the data, it immediately starts synchronizing the NN by prompting the sensors to action. Afterwards, prep/2 drops into the exoself's main process loop.

loop(FileName,Genotype,IdsAndPids,Cx_PId,SPIds,NPIds,APIds,ScapePIds,HighestFitness,EvalAcc,CycleAcc,TimeAcc,Attempt) ->
	receive
		{Cx_PId,evaluation_completed,Fitness,Cycles,Time}->
			{U_HighestFitness,U_Attempt} = case Fitness > HighestFitness of
				true ->
					[NPId ! {self(),weight_backup} || NPId <- NPIds],
					{Fitness,0};
				false ->
					Perturbed_NPIds = get(perturbed),
					[NPId ! {self(),weight_restore} || NPId <- Perturbed_NPIds],
					{HighestFitness,Attempt+1}
			end,
			case U_Attempt >= ?MAX_ATTEMPTS of
				true -> %end training
					U_CycleAcc = CycleAcc+Cycles,
  				U_TimeAcc = TimeAcc+Time,
					backup_genotype(FileName,IdsAndPids,Genotype,NPIds),
					terminate_phenotype(Cx_PId, SPIds, NPIds, APIds, ScapePIds),
					io:format("Cortex:~p finished training. Genotype has been backed up.~n Fitness:~p~n TotEvaluations:~p~n TotCycles:~p~n TimeAcc:~p~n",[Cx_PId,U_HighestFitness,EvalAcc,U_CycleAcc,U_TimeAcc]),
					case whereis(trainer) of
						undefined ->
							ok;
						PId ->
							PId ! {self(),U_HighestFitness,EvalAcc,U_CycleAcc,U_TimeAcc}
					end;
				false -> %continue training
					Tot_Neurons = length(NPIds),
					MP = 1/math:sqrt(Tot_Neurons),
					Perturb_NPIds = [PId || PId <- NPIds, random:uniform()<MP],
					put(perturbed,Perturb_NPIds),
					[NPId ! {self(),weight_perturb} || NPId <- Perturb_NPIds],
					Cx_PId ! {self(),reactivate},
					loop(FileName,Genotype,IdsAndPids,Cx_PId,SPIds,NPIds,APIds,ScapePIds,U_HighestFitness,EvalAcc+1,CycleAcc+Cycles,TimeAcc+Time,U_Attempt)
			end
	end.
%The main process loop waits for the NN to complete the task, receive its fitness score, and send Exoself the: {Cx_PId,evaluation_completed,Fitness,Cycles,Time} message. The message contains all the information about that particular evaluation, the acquired fitness score, the number of total Sense-Think-Act cycles executed and the time it took to complete the evaluations. The exoself then compares the Fitness to the one it has on record (if any) and based on that decides whether to revert the previously perturbed neurons back to their original state or not. If the new Fitness is lower, then the perturbed neurons are contacted and their weights are reverted. If the new Fitness is greater than the one stored on record, then the NN is backed up to file, and the variable EvalAcc is reset to 0. Finally, depending on whether the NN has failed to improve its fitness Max_Attempts number of times, the exoself decides whether anpther pertubation attempt is warranted. If it is warranted, then the exoself chooses which neurons to mutate by randomly choosing each neuron with the probability of 1/sqrt(Tot_Neurons) where Tot_Neurons is the total number of neurons in the neural network. The exoself saves the PIds of those chosen neurons to process dictionary, and then sends those neurons a signal that they should perturb their weights. Finally it tells cortex to reactivate and start syncing the sensors and actuators again. But of the NN has failed to improve its fitness for Max_Attempts number of times, if EvalAcc > Max_Attempts, then the exoself terminates all the elements in the NN and if there is a registered process by the name trainer, the exoself sends it the HighestFitness score that its NN achieved and the number of total evaluations it took to achieve it.

	spawn_Scapes(IdsAndPids,Genotype,Sensor_Ids,Actuator_Ids) ->
		Sensor_Scapes = [(genotype:read(Genotype,Id))#sensor.scape || Id <- Sensor_Ids],
		Actuator_Scapes = [(genotype:read(Genotype,Id))#actuator.scape || Id <- Actuator_Ids],
		Unique_Scapes = Sensor_Scapes ++ (Actuator_Scapes -- Sensor_Scapes),
		SN_Tuples = [{scape:gen(self(),node()),ScapeName} || {private,ScapeName} <- Unique_Scapes],
		[ets:insert(IdsAndPids,{ScapeName,PId}) || {PId,ScapeName} <- SN_Tuples],
		[ets:insert(IdsAndPids,{PId,ScapeName}) || {PId,ScapeName} <- SN_Tuples],
		[PId ! {self(),ScapeName} || {PId,ScapeName} <- SN_Tuples],
		[PId || {PId,_ScapeName} <- SN_Tuples].
	%spawn_Scapes/4 first extracts all the scape names from sensors and actuators, then builds a set of unique scapes, and then finally sctracts and spawns the private scapes. The public scapes are not spawned since they are independent of the NN, and should already be running. The reason for extracting the list of unique scapes is because if both a sensor and an actuator are pointing to the same scape, then that means they will interfere with the same scape, and it does not mean that each one should spawn its own scape of the same name. Afterwards we use the IdsAndPids ETS table to create a map from scape PId to scape name, and from scape name to scape PId for later use. The function then sends each spawned scape a message composed of the exoself's PId and the scape's name {self(),ScapeName}. Finally a spawned scape PId list is composed and returned to the caller. 

	spawn_CerebralUnits(IdsAndPids,CerebralUnitType,[Id|Ids]) ->
		PId = CerebralUnitType:gen(self(),node()),
		ets:insert(IdsAndPids,{Id,PId}),
		ets:insert(IdsAndPids,{PId,Id}),
		spawn_CerebralUnits(IdsAndPids,CerebralUnitType,Ids);
	spawn_CerebralUnits(_IdsAndPids,_CerebralUnitType,[]) ->
		true.
%We spawn the process for each element based on its type: CerebralUnitType, and the gen function that belongs to the CerebralUnitType module. We then enter the {Id,PId} tuple into our ETS table for later use.

	link_Sensors(Genotype,[SId|Sensor_Ids],IdsAndPids) ->
		R = genotype:read(Genotype,SId),
		SPId = ets:lookup_element(IdsAndPids,SId,2),
		Cx_PId = ets:lookup_element(IdsAndPids,R#sensor.cortex_id,2),
		SName = R#sensor.name,
		Fanout_Ids = R#sensor.fanout_ids,
		Fanout_PIds = [ets:lookup_element(IdsAndPids,Id,2) || Id <- Fanout_Ids],
		Scape = case R#sensor.scape of
			{private, ScapeName} ->
				ets:lookup_element(IdsAndPids, ScapeName, 2)
		end,
		SPId ! {self(), {SId, Cx_PId, Scape, SName, R#sensor.vl, Fanout_PIds}},
		link_Sensors(Genotype,Sensor_Ids,IdsAndPids);
	link_Sensors(_Genotype,[],_IdsAndPids) ->
		ok.

	link_Actuators(Genotype,[AId|Actuator_Ids],IdsAndPids) ->
		R = genotype:read(Genotype,AId),
		APId = ets:lookup_element(IdsAndPids,AId,2),
		Cx_PId = ets:lookup_element(IdsAndPids,R#actuator.cortex_id,2),
		AName = R#actuator.name,
		Fanin_Ids = R#actuator.fanin_ids,
		Fanin_PIds = [ets:lookup_element(IdsAndPids,Id,2) || Id <- Fanin_Ids],
		Scape = case R#actuator.scape of
			{private, ScapeName} ->
				ets:lookup_element(IdsAndPids, ScapeName, 2)
		end,
		APId ! {self(), AId, Cx_PId, Scape, AName, Fanin_PIds},
		link_Actuators(Genotype,Actuator_Ids,IdsAndPids);
	link_Actuators(_Genotype,[],_IdsAndPids) ->
		ok.

	link_Neurons(Genotype,[NId|Neuron_Ids],IdsAndPids) ->
		R = genotype:read(Genotype,NId),
		NPId = ets:lookup_element(IdsAndPids,NId,2),
		Cx_PId = ets:lookup_element(IdsAndPids,R#neuron.cortex_id,2),
		AFName = R#neuron.af,
		Input_IdPs = R#neuron.input_ids_plus_weights,
		Output_Ids = R#neuron.output_ids,
		Input_PIdPs = convert_IdPs2PIdPs(IdsAndPids,Input_IdPs,[]),
		Output_PIds = [ets:lookup_element(IdsAndPids,Id,2) || Id <- Output_Ids],
		NPId ! {self(),{NId,Cx_PId,AFName,Input_PIdPs,Output_PIds}},
		link_Neurons(Genotype,Neuron_Ids,IdsAndPids);
	link_Neurons(_Genotype,[],_IdsAndPids) ->
		ok.

	convert_IdPs2PIdPs(_IdsAndPids,[{bias,Bias}],Acc) ->
		lists:reverse([Bias|Acc]);
	convert_IdPs2PIdPs(IdsAndPids,[{Id,Weights}|Fanin_IdPs],Acc) ->
		convert_IdPs2PIdPs(IdsAndPids,Fanin_IdPs,[{ets:lookup_element(IdsAndPids,Id,2),Weights}|Acc]).
%Rhw link_CerebralUnits/2 converts the Ids to PIds using the created IdsAndPids ETS table. At this point all the elements are spawned, and the processes are waiting for their inital states. convert_IdPs2PIdPs/3 converts the IdPs tuples into tuples that use PIds instead of Ids, such that the Neuron will know which weights are to be associated with which incoming vector signals. The last element is the bias, which is added to the list in a non tuple form. Afterwards, the list is reversed to take its proper order.

	link_Cortex(Cx,IdsAndPids) ->
		Cx_Id = Cx#cortex.id,
		Cx_PId = ets:lookup_element(IdsAndPids,Cx_Id,2),
		SIds = Cx#cortex.sensor_ids,
		AIds = Cx#cortex.actuator_ids,
		NIds = Cx#cortex.neuron_ids,
		SPIds = [ets:lookup_element(IdsAndPids,Id,2) || Id <- SIds],
		APIds = [ets:lookup_element(IdsAndPids,Id,2) || Id <- AIds],
		NPIds = [ets:lookup_element(IdsAndPids,Id,2) || Id <- NIds],
		Cx_PId ! {self(),Cx_Id,SPIds,NPIds,APIds},
		{SPIds,NPIds,APIds}.
%The cortex is initialized to its proper state just as other elements. Because we have not yet implemented a learning algorithm for our NN system, we need to specify when the NN should shutdown. We do this by specifying the total number of cycles the NN should execute before terminating, which is 1000 in this case.

backup_genotype(FileName,IdsAndPids,Genotype,NPIds)->
	Neuron_IdsNWeights = get_backup(NPIds,[]),
	update_Genotype(IdsAndPids,Genotype,Neuron_IdsNWeights),
	genotype:save_to_file(Genotype,FileName),
	io:format("Finished updating genotype to file:~p~n",[FileName]).
	get_backup([NPId|NPIds],Acc)->
		NPId ! {self(),get_backup},
			receive
				{NPId,NId,WeightTuples}->
					get_backup(NPIds,[{NId,WeightTuples}|Acc])
			end;
	get_backup([],Acc)->
		Acc.
%The backup_genotype/4 uses get_backup/2 to contact all the neurons in its NN and request for the neuron's Ids and their Input_IdPs. Once the updated Input_IdPs from all the neurons have been accumulated, they are passed through the update_genotype/3 function to produce the genotype with updated weights, which is then saved to file.

	update_Genotype(IdsAndPids,Genotype,[{N_Id,PIdPs}|WeightPs]) ->
		N = genotype:read(Genotype,N_Id),
		io:format("PIdPs:~p~n",[PIdPs]),
		Updated_InputIdPs = convert_PIdPs2IdPs(IdsAndPids,PIdPs,[]),
		U_N = N#neuron{input_ids_plus_weights=Updated_InputIdPs},
		U_Genotype = genotype:write(Genotype,U_N),
		io:format("N:~p~n U_N:~p~n Genotype:~p~n U_Genotype:~p~n",[N,U_N,Genotype,U_Genotype]),
		update_Genotype(IdsAndPids,Genotype,WeightPs);
	update_Genotype(_IdsAndPids,Genotype,[]) ->
		Genotype.

	convert_PIdPs2IdPs(IdsAndPids,[{PId,Weights}|Input_PIdPs],Acc) ->
		convert_PIdPs2IdPs(IdsAndPids,Input_PIdPs,[{ets:lookup_element(IdsAndPids,PId,2),Weights}|Acc]);
	convert_PIdPs2IdPs(_IdsAndPids,[Bias],Acc) ->
		lists:reverse([{bias,Bias}|Acc]).
%For every {N_Id,PIdPs} tuple the update_genotype/3 function extracts the neuron with the id: N_Id and updates its weights. The convert_PIdPs2IdPs/3 performs the conversion from PIdPs to Ids of every {PId,Weights} tuple in the Input_PIdPs list. The updated Genotype is then returned back to the caller.

terminate_phenotype(Cx_PId,SPIds,NPIds,APIds,ScapePIds) ->
	[PId ! {self(),terminate} || PId <- SPIds],
	[PId ! {self(),terminate} || PId <- NPIds],
	[PId ! {self(),terminate} || PId <- APIds],
	[PId ! {self(),terminate} || PId <- ScapePIds],
	Cx_PId ! {self(), terminate}.

