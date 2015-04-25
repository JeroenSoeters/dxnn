-module(morphology).
-compile(export_all).
-include("records.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Get Init Actuators/Sensors %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
get_InitSensor(Morphology)->
	Sensors = morphology:Morphology(sensors),
	lists:nth(1, Sensors).

get_InitActuator(Morphology)->
	Actuators = morphology:Morphology(actuators),
	lists:nth(1, Actuators).

get_Sensors(Morphology)->
	morphology:Morphology(sensors).

get_Actuators(Morphology)->
	morphology:Morphology(actuators).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% MORPHOLOGIES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
xor_mimic(sensors) ->
	[
		#sensor{id={sensor, generate_id()}, name=xor_GetInput, scape={private, xor_sim}, vl=2}
	];
xor_mimic(actuators) ->
	[
		#actuator{id={actuator, generate_id()}, name=xor_SendOutput, scape={private, xor_sim}, vl=1}
	].

generate_id() ->
	{MegaSeconds, Seconds, MicroSeconds} = now(),
	MegaSeconds * 1000000 + Seconds + MicroSeconds / 1000000.
