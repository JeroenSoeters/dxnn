-module(polis).
%% API
-export([start/1, start/0, stop/0, init/2, create/0, reset/0, sync/0]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-behaviour(gen_server).
-include("records.hrl").

%% ===================================================================
%% Polis configuration options
%% ===================================================================

-record(state, {active_mods=[], active_scapes=[]}).
-record(scape_summary, {address, type, parameters=[]}).
-define(MODS, []).
-define(PUBLIC_SCAPES, []).
% The MODS list contains the names of the processes, functions or other databases that also need to be executed and started when we start our neuroevolutionary platform. In the same manner, when we have created a new public scape, we can add a scape_summary tuple with the scape's information to the PUBLIC_SCAPES list, so that it is initialized and started with the system. The state record for the polis has all the elements needed to track the currently active mods and public scapes, which are either started during the startup of the neuroevolutionary platform, or spawned later, while the polis is already active.

%% ===================================================================
%% API
%% ===================================================================

sync() ->
	make:all([load]).
% A sync/1 function can compile and reload all the modules pertaining to the project within the folder.

start() ->
	case whereis(polis) of
		undefined ->
			gen_server:start(?MODULE, {?MODS, ?PUBLIC_SCAPES}, []);
		PolisPid ->
			io:format("Polis:~p is already running on this node~n", [PolisPid])
	end.

start(StartParameters) ->
	gen_server:start(?MODULE, StartParameters, []).

init(Pid, InitState) ->
	gen_server:cast(Pid, {init, InitState}).
% The start/0 function first checks whether a polis process has already been spawned, by checking if one is registered. If it's not, then the start/1 function starts up the neuroevolutionary platform.

stop() ->
	case whereis(polis) of
		undefined ->
			io:format("Polis cannot be stopped, is it not online~n");
		PolisPid ->
			gen_server:cast(PolisPid, {stop, normal})
	end.
% The stop/0 function first checks whether a polis is online. If there is an online polis process runing on the node, then the stop function sends the signal to it requesting it to stop.

%% ===================================================================
%% gen_server callbacks
%% ===================================================================

init({Mods, PublicScapes}) ->
	{X, Y, Z} = now(),
	random:seed(X, Y, Z),
	process_flag(trap_exit, true),
	register(polis, self()),
	io:format("Parameters:~p~n", [{Mods, PublicScapes}]),
	mnesia:start(),
	start_supmods(Mods),
	ActivePublicScapes = start_scapes(PublicScapes, []),
	io:format("******** Polis: ##MATHEMA## is now online.~n"),
	InitState = #state{active_mods=Mods, active_scapes=ActivePublicScapes},
	{ok, InitState}.
% The init/1 function first seeds random with a new seed, in the case a random number generator will be needed. The polis process is then registered, the mnesia database is started and the supporting modules, if any, are then started through the start_supmods/1 function. Then all the specified public scapes, if any, are activated. Finallt we create the initial state, which contains the Pids of the currently active public scapes and the names of the activated mods. Finally the function then drops into the main gen_server loop.

handle_call({get_scape, Type}, {_CortexPid, _Ref}, State) ->
	ActivePublicScapes = State#state.active_scapes,
	ScapePid = case lists:keyfind(Type, 3, ActivePublicScapes) of
		false ->
			undefined;
		PublicScape ->
			PublicScape#scape_summary.address
	end,
	{reply, ScapePid, State};
handle_call({stop, normal}, _From, State) ->
	{stop, normal, State};
handle_call({stop, shutdown}, _From, State) ->
	{stop, shutdown, State}.
% At this point the polis only accepts a get_scape call, to which it replies with the Pid or undefined message and the two standard {stop, normal} and {stop, shutdown} calls.

handle_cast({init, InitState}, _State) ->
	{noreply, InitState};
handle_cast({stop, normal}, State) ->
	{stop, normal, State};
handle_cast({stop, shutdown, State}, State) ->
	{stop, shutdown, State}.
% At this point polis allows only for 3 standard casts: {init, InitState}, {stop, normal} and {stop, shutdown}.

handle_info(_Info, State) ->
	{noreply, State}.
% The handle_info/2 function is unused by the polis process at this time.

terminate(Reason, State) ->
	stop_supmods(State#state.active_mods),
	stop_scapes(State#state.active_scapes),
	io:format("******** Polis: ##MATHEMA## is now offline, terminated with reason:~p~n",[Reason]),
	ok.
% When polis is terminated, it first shuts down all the supporting mods by calling the stop_supmods/1 functino, and then it shuts down all the public scapes by calling the stop_scapes/1 function.

code_change(_OldVersion, State, _Extra) ->
	{ok, State}.

%% ===================================================================
%% Internal functions
%% ===================================================================

create() ->
	mnesia:create_schema([node()]),
	mnesia:start(),
	mnesia:create_table(agent, [{disc_copies, [node()]}, {type, set}, {attributes, record_info(fields, agent)}]),
	mnesia:create_table(cortex, [{disc_copies, [node()]}, {type, set}, {attributes, record_info(fields, cortex)}]),
	mnesia:create_table(neuron, [{disc_copies, [node()]}, {type, set}, {attributes, record_info(fields, neuron)}]),
	mnesia:create_table(sensor, [{disc_copies, [node()]}, {type, set}, {attributes, record_info(fields, sensor)}]),
	mnesia:create_table(actuator, [{disc_copies, [node()]}, {type, set}, {attributes, record_info(fields, actuator)}]),
	mnesia:create_table(population, [{disc_copies, [node()]}, {type, set}, {attributes, record_info(fields, population)}]),
	mnesia:create_table(species, [{disc_copies, [node()]}, {type, set}, {attributes, record_info(fields, species)}]),
	ok.
% The create/0 fucntion sets up a new mnsesia database composed of the agent, cortex, neuron, actuator, polis, population and speicie tables.

reset() ->
	mnesia:stop(),
	ok = mnesia:delete_schema([node()]),
	polis:create().
% The reset/0 function deletes the scema and recreates a fresh database from scratch.

start_supmods([ModName|ActiveMods]) ->
	ModName:start(),
	start_supmods(ActiveMods);
start_supmods([]) ->
	done.
% The start_supmods/1 function expects a list of module names of the mods that are to be started with the startup of the neuroevolutionary platform. Each module must have a start/0 function that starts up the supporting mod process.

stop_supmods([ModName|ActiveMods]) ->
	ModName:start(),
	stop_supmods(ActiveMods);
stop_supmods([]) ->
	done.
% The stop_supmods/1 expects a list of supporting mod names. The mod's name must be the name of its module and the module must have a stop/0 function that stops the module.

start_scapes([Scape|Scapes], Acc) ->
	Type = Scape#scape_summary.type,
	Parameters = Scape#scape_summary.parameters,
	{ok, Pid} = scape:start_link({self(), Type, Parameters}),
	start_scapes(Scapes, [Scape#scape_summary{address=Pid}|Acc]);
start_scapes([], Acc) ->
	lists:reverse(Acc).
% The start_scapes/2 function accepts a list of scape_summary records, which specify the names of the public scapes and any parameters with which those scapes should be started. What specifies the scape which is going to be created by the scape module is the Type that is dropped into the function. Of course the scape module should already be able to create the Type fo the scape that is dropped into the start_link function. Once the scape is started, the function returns a list of updated scape_summary records.

stop_scapes([Scape|Scapes]) ->
	Pid = Scape#scape_summary.address,
	gen_server:cast(Pid, {self(), stop, normal}),
	stop_scapes(Scapes);
stop_scapes([]) ->
	ok.
% The stop_scapes/1 function accepts a list of scape_summary records and stops all the scapes in the list.

