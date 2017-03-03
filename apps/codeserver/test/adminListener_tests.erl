-module(adminListener_tests).
-export([]).

-include_lib("eunit/include/eunit.hrl").
-include("../src/record_definition.hrl").

adminListener_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [
      {"check you can start the server",fun start_server/0},
      {"check the input format is correct",fun check_format/0},   
      {"check parsing is done correct",fun check_parse/0},
      {"check execute restrict works",fun execute_restrict_should_return_ok_upon_pass/0},
      {"check execute un-restrict works",fun execute_unrestrict_should_return_ok_upon_pass/0},
      {"check execute delete module works",fun execute_delete_module_keyval_should_return_ok_upon_pass/0},
      {"check port is already in use",fun port_already_in_use /0}

     ]}.


setup() ->
    ok = file_handler:start("loaded/"),
    ok = adminListener:start().

cleanup(_) ->
    exit(whereis(adminListener),kill),
    ensure_exited_adminListener(),
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

ensure_exited_adminListener()->
    case whereis(adminListener) of
	undefined ->
	    ok;
	_ ->
	    timer:sleep(10),
	    ensure_exited_adminListener()
    end.       

port_already_in_use()->
    Expect = {error, eaddrinuse},
    Result = adminListener:start(),
    ?assertMatch(Expect,Result).
    
start_server()->
    Pid= whereis(adminListener),
    io:format("~p~n",[Pid]),    
    ?assertMatch(true,is_pid(Pid)).

check_format()->
    Expect = ["restrict","module", "function","2"], %% mODULE IS IN STRING AND fUNCTION IS IN ATOM,
    Result = adminListener:format("restrict module function 2"),
    ?assertMatch(Expect, Result).

check_parse()->
    Expect = {"My_mod",{my_fun,2}},
    Result = adminListener:parse(["restrict","My_mod","my_fun","2"]),
    ?assertEqual(Expect,Result).

execute_restrict_should_return_ok_upon_pass()->
    Result = adminListener:execute_restrict(["restrict","ets_table1","start","0"]), 
    ?assertEqual("the function in module was restricted",Result).

execute_unrestrict_should_return_ok_upon_pass()->
    "the function in module was restricted" = adminListener:execute_restrict(["restrict","ets_table1","start","0"]), 
    Result = adminListener:execute_unrestrict(["unrestrict","ets_table1","start","0"]), 
    ?assertEqual("the function in module was unrestricted",Result).

execute_delete_module_keyval_should_return_ok_upon_pass()->
    Result = adminListener:execute_delete(["delete","ets_table1"]), 
    ?assertEqual(ok,Result).
