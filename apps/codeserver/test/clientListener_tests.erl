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
      {"check the input format is correct",fun check_format/0},   
      {"check parsing is done correct",fun check_parse/0},
     
      {"check port is already in use",fun port_already_in_use/0},

      {"check parsing args works",fun check_parse_args/0},

      {"check execute list works ",fun check_execute_list/0},

      {"check execute run works ",fun check_execute_run/0},
      
      {"check execute info works  ",fun check_execute_info_works/0}
     ]}.


setup() ->
    ok = file_handler:start(),
    ok = clientListener:start().

cleanup(_) ->
    exit(whereis(clientListener),kill),
    ensure_exited_clientListener(),
    exit(whereis(file_handler),kill),
    file:delete("/home/ekousha/codeserver/apps/codeserver/loaded/ets_table1.beam"),
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

check_format()->
    Expect = ["run","ets_table1","return_ok"], %% mODULE IS IN STRING AND fUNCTION IS IN ATOM,
    Result = clientListener:check_format("run ets_table1 return_ok"),
    ?assertMatch(Expect, Result).

check_parse()->
    Expect = {"ets_table1",{my_fun,2}},
    Result = clientListener:parse(["run","ets_table1","my_fun","a","b"]),
    ?assertEqual(Expect,Result).

check_parse_args()->
    Expect = ["Sharma"],
    Result = clientListener:parse_args(["run","ets_table1","greet_me","Sharma"]),
    ?assertEqual(Expect,Result).

check_execute_list()->
    Expect = ["ets_table1",
                  [{start,0},
                   {return_ok,0},
                   {greet_me,1},
                   {module_info,0},
                   {module_info,1}]],

    Result = clientListener:execute_list(),
    ?assertEqual(Expect,Result).

check_execute_run()->
    Expect = "Hello Koustubh",
    Result = clientListener:execute_run(["run","ets_table1","greet_me","Koustubh"]),
    ?assertEqual(Expect,Result).

check_execute_info_works()->
   %% Expect = [{2017,2,9,15,26,36},"8CE9FCB2B010A44FD97CD3AFBC9FD3CD","ets_table1"],
    Result = clientListener:execute_info(),
    ?assertMatch([_,"8CE9FCB2B010A44FD97CD3AFBC9FD3CD","ets_table1"],Result).


















%% perform_clientListener_test_() ->
%%     {foreach,
%%      fun setup/0,
%%      fun cleanup/1,
%%      [fun function_is_restricted/0,
%%       fun function_is_not_restricted/0
%%      ]}.

%% setup()->
%%  %keep_Specs:start().

%% cleanup(_)->
  
%%     %keep_Specs:terminate_process().


%% function_is_not_restricted()->   
%%     Expected = false,
%%     Result = clientListener:is_restricted(lists,{seq,2}),
%%     ?assertEqual(Expected,Result).

%% function_is_restricted()->
%%     %keep_Specs:add(lists,{seq,2}),
%%     Expected = true,
%%     Result = clientListener:is_restricted(lists,{seq,2}),
%%     ?assertEqual(Expected,Result).

%% runs_unrestricted_function()->
%%     Expected = true,
%%     Result = clientListener:is_restricted(lists,{seq,2}),
%%     ?assertEqual(Expected,Result).
