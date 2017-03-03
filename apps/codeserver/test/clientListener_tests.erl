-module(clientListener_tests).
-export([]).
-include_lib("eunit/include/eunit.hrl").
-include("../src/record_definition.hrl").

clientListener_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [
      {"check you can start the Client server",fun start_server/0},
      {"check parsing is done correct",fun check_parse/0},
      {"check port is already in use",fun port_already_in_use/0},
      {"check parsing args works",fun parse_should_give_correctly_ordered_mod_funs_args/0}
      
     ]}.

setup() ->
    ok = file_handler:start("loaded/"),
    ok = clientListener:start().

cleanup(_) ->
    exit(whereis(clientListener),kill),
    ensure_exited_clientListener(),
    exit(whereis(file_handler),kill),
    file:delete("loaded/ets_table1.beam"),
    ensure_exited_file_handler().

ensure_exited_file_handler() ->
    case whereis(file_handler) of
	undefined ->
	    ok;
	_ ->
	    timer:sleep(10),
	    ensure_exited_file_handler()
    end.

ensure_exited_clientListener()->
    case whereis(clientListener) of
	undefined ->
	    ok;
	_ ->
	    timer:sleep(10),
	    ensure_exited_clientListener()
    end.

port_already_in_use()->
    Expect = {error, eaddrinuse},
    Result = clientListener:start(),
    ?assertMatch(Expect,Result).

start_server()->
    Pid= whereis(clientListener),
    io:format("~p~n",[Pid]),
    ?assertMatch(true,is_pid(Pid)).

parse_should_give_correctly_ordered_mod_funs_args()->
    Expect = {"robin",{seq,2},[3,10]}, 
    Result = clientListener:parse("run robin seq 3,10"),
    ?assertMatch(Expect, Result).

check_parse()->
    Expect = {"are",{you,2},[a, b]},
    Result = clientListener:parse("how are you a,b"),
    ?assertMatch(Expect,Result).

