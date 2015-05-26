-module(polis_tests).
-include_lib("eunit/include/eunit.hrl").

create_polis_test() ->
	mnesia:delete_schema([node()]),
	?assertEqual(ok, polis:create()).

reset_polis_test() ->
	polis:create(),
	?assertEqual(ok, polis:reset()).

start_stop_polis_test() ->
	{Flag, _Pid} = polis:start(),
	?assertEqual(ok, Flag),
	?assertEqual(ok, polis:stop()).

