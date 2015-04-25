-module(scape).
-compile(export_all).
-include("records.hrl").

gen(ExoSelf_PId, Node) ->
  spawn(Node, ?MODULE, prep, [ExoSelf_PId]). 

prep(ExoSelf_PId) ->
  receive
    {ExoSelf_PId, Name} ->
      scape:Name(ExoSelf_PId)
  end.
%gen/2 is executed by the exoself. The function spawns prep/1 process and awaits the name of the scape form the exoself. Each scape is a seperate and independent process, a self contained system that was developed to interface with the sensors and actuators from which its name was etracted. The name of the scape is the name of it's main process loop.

xor_sim(ExoSelf_PId) ->
  XOR = [{[-1,-1], [-1]}, {[1,-1], [1]}, {[-1,1], [1]}, {[1,1], [-1]}],
  xor_sim(ExoSelf_PId, {XOR, XOR}, 0).

xor_sim(ExoSelf_PId, {[{Input, CorrectOutput}|XOR], MXOR}, ErrAcc) ->
  receive
    {From, sense} ->
      From ! {self(), percept, Input},
      xor_sim(ExoSelf_PId, {[{Input, CorrectOutput}|XOR], MXOR}, ErrAcc);
    {From, action, Output} ->
      Error = list_compare(Output, CorrectOutput, 0),
      case XOR of
        [] ->
          MSE = math:sqrt(ErrAcc + Error),
          Fitness = 1 / (MSE + 0.00001),
					%io:format("*** MSE: ~p *** Fitness: ~p~n", [MSE, Fitness]),
          From ! {self(), Fitness, 1},
          xor_sim(ExoSelf_PId, {MXOR, MXOR}, 0);
        _ ->
          From ! {self(), 0, 0},
          xor_sim(ExoSelf_PId, {XOR, MXOR}, ErrAcc + Error)
      end;
    {ExoSelf_PId, terminate} ->
      ok
  end.

  list_compare([X|Xs], [Y|Ys], ErrorAcc) ->
    list_compare(Xs, Ys, ErrorAcc + math:pow(X-Y, 2));
  list_compare([],[],ErrorAcc) ->
    math:sqrt(ErrorAcc).
%xor_sim/3 is a scape that simulates the XOR operation, interacts with the NN and gages the NN's performance. xor_sim expects two types of messages from the NN, one message from the sensor and one message from the actuator. The message {from, sense} prompts the scape to send the NN the percept, which is a vector of length 2 and contains the XOR input. The second expected message from the NN is the message from the actuator, which is expected to be an output of the NN and packaged into the form {From, action, Output}. At this point xor_sim/3 compares the output with the expected output that is associated with the sensory message that should have been gathered by the sensors and then sends back to the actuator process a message composed of the scape's PId, Fitness and HaltFlag which specifies whether the simulation has ended for the NN. The scape keeps track of the mean squared error between the NN's output and the correct output. Once the NN has processed all 4 signals for the XOR, the scape computes the total MSE, converts it to fitness and finally forwards this fitness and the HaltFlag=1 to the NN. This particular scape uses the lifetime based fitness, rather than step-based fitness. During all other steps the scape sends the actuator the signal: {Scape_PId, 0, 0}, while it accumulates the errors and only at the very end does it calculate the total fitness, which is the inverse of the error with a small extra added to avoid divide by 0  errors. Afterwards, xor_sim resets back to its initial state and awaits for signals from the NN.
