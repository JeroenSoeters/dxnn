-record(sensor, {id, cx_id, name, scape, vl, fanout_ids}).
-record(actuator, {id, cx_id, name, scape, vl, fanin_ids}).
-record(neuron, {id, cx_id, af, input_idps, output_ids}).
-record(cortex, {id, sensor_ids, actuator_ids, nids}).

