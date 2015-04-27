-module(morphology).
-compile(export_all).
-include("records.hrl").

%% ==================================================================
%% Get actuators/sensors
%% ==================================================================

get_init_sensors(Morphology)->
	Sensors = morphology:Morphology(sensors),
	lists:nth(1, Sensors).

get_init_actuators(Morphology)->
	Actuators = morphology:Morphology(actuators),
	lists:nth(1, Actuators).

get_sensors(Morphology)->
	morphology:Morphology(sensors).

get_actuators(Morphology)->
	morphology:Morphology(actuators).

%% ==================================================================
%% Morphologies
%% ==================================================================

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
