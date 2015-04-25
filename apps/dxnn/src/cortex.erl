-module(cortex).
-compile(export_all).
-include("records.hrl").

gen(ExoSelf_PId, Node) ->
	spawn(Node, ?MODULE, prep, [ExoSelf_PId]).

prep(ExoSelf_PId) ->
	{V1, V2, V3} = now(),
	random:seed(V1, V2, V3),
	receive
		{ExoSelf_PId, Id, SPIds, NPIds, APIds} ->
			put(start_time, now()),
			[SPId ! {self(),sync} || SPId <- SPIds ],
			loop(Id, ExoSelf_PId, SPIds, {APIds, APIds}, NPIds, 1, 0, 0, active)
	end.
%The gen/2 function spawns the cortex element, which immediately starts to wait for the state message from the same process that spawned itself, exoself. The initial state message contains the sensor, actuator and neuron PId lists. Before dropping into the main loop, CycleAcc, FitnessAcc and HFAcc (HaltFlac Acc) are all set to 0, and the status of the cortex is set to active, prompting it to begin the synchronization process and call the sensors to action.

loop(Id, ExoSelf_PId, SPIds, {[APId|APIds], MAPIds}, NPIds, CycleAcc, FitnessAcc, HFAcc, active) ->
  receive
	  {APId, sync, Fitness, HaltFlag} ->
		  loop(Id, ExoSelf_PId, SPIds, {APIds, MAPIds}, NPIds, CycleAcc, FitnessAcc + Fitness, HFAcc + HaltFlag, active);
    {ExoSelf_PId, terminate} ->
			io:format("Cortex:~p is terminating.~n",[Id]),
			[PId ! {self(),terminate} || PId <- SPIds],
			[PId ! {self(),terminate} || PId <- MAPIds],
			[PId ! {self(),terminate} || PId <- NPIds]
	end;
loop(Id, ExoSelf_PId, SPIds, {[], MAPIds}, NPIds, CycleAcc, FitnessAcc, HFAcc, active) ->
  case HFAcc > 0 of
    true -> %organism finished evolution
      TimeDif = timer:now_diff(now(), get(start_time)),
      ExoSelf_PId ! {self(), evaluation_completed, FitnessAcc, CycleAcc, TimeDif},
      loop(Id, ExoSelf_PId, SPIds, {MAPIds, MAPIds}, NPIds, CycleAcc, FitnessAcc, HFAcc, inactive);
    false ->
      [PId ! {self(), sync} || PId <- SPIds],
        loop(Id, ExoSelf_PId, SPIds, {MAPIds, MAPIds}, NPIds, CycleAcc+1, FitnessAcc, HFAcc, active)
  end;
loop(Id, ExoSelf_PId, SPIds, {MAPIds, MAPIds}, NPIds, _CycleAcc, _FitnessAcc, _HF_Acc, inactive) ->
  receive
    {ExoSelf_PId, reactivate} ->
      put(start_time, now()),
			[SPId ! {self(), sync} || SPId <- SPIds],
			loop(Id, ExoSelf_PId, SPIds, {MAPIds, MAPIds}, NPIds, 1, 0, 0, active);
    {ExoSelf_PId, terminate} ->
      ok
  end.
%The cortex's goal is to synchronize the NN system's sensors and actuators. When the actuators have received all their control signals, they forward the sync messages, the Fitness and the HaltFlag messages to the cortex. The cortex accumulates these Fitness and HaltFlag signals, and if any of the HaltFlag signals have been set to 1, HFAcc will be greater than 0, signifying that the cortex should halt. When HFAcc > 0, the cortex calculates the total amount of time it has ran (TimeDof) and forwards to teh exoself the values FitnessAcc, CycleAcc and TimeDif. Afterwards, the cortex enters the inactive mode and awaits furtner instructions from the exoself. If none of the HaltFlags were set to 0, then the value HFAcc == 0 and the cortex triggers off another Sense-Think-Act cycle. The reason the cortex process stores 2 copies of the actuator PIds: the APIds and the Memory APIDs (MAPIds) is so that once all the actuators have sent it the sync messages, it can restore the APIds list from the MAPIds.

	get_backup([NPId|NPIds],Acc) ->
		NPId ! {self(),get_backup},
		receive
			{NPId,NId,WeightTuples} ->
				get_backup(NPIds,[{NId,WeightTuples}|Acc])
		end;
	get_backup([],Acc) ->
		Acc.
%During backup, the cortex contacts all the neurons in its NN and requests for the neuron's Ids and their Input_IdPs. Once the updated Input_IdPs from all the neurons have been accumulated, te list is sent to exoself for the actual backup and storage.
