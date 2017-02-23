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
      {"test 1 of reading data and parsing the args",fun read_data1/0},
      {"test 2 of reading data and parsing the args ",fun read_data2/0},
      {"test 3 of reading data and parsing the args ",fun read_data3/0},
      {"test 4 of reading data and parsing the args ",fun read_data4/0},
  {"test 5 of reading data and parsing the args ",fun read_data5/0}
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

%% check_format_with_quoted_variables()->
%%     Expect = ["run","ets_table1","greet_me","\"ThisIsQuoted\""],
%%     Result = clientListener:check_format("run ets_table1 greet_me \"ThisIsQuoted\""),
%%     ?assertMatch(Expect, Result).

check_parse()->
    Expect = {"ets_table1",{my_fun,2}},
    Result = clientListener:parse(["run","ets_table1","my_fun","a","b"]),
    ?assertMatch(Expect,Result).

check_parse_args()->
    Expect = ["Sharma"],
    Result = clientListener:parse_args(["run","ets_table1","greet_me","\"Sharma\""]),
    ?assertMatch(Expect,Result).

read_data1()->
    Input = "a bb ccc {Koustubh Sharma}",
    Expect = "a,bb,ccc,{Koustubh Sharma}",
    Result = clientListener:read_data(Input),
    ?assertMatch(Expect,Result).

read_data2()->
    Input = "a bb ccc \"Koustubh Sharma\"",
    Expect = "a,bb,ccc,\"Koustubh Sharma\"",
    Result = clientListener:read_data(Input),
    ?assertMatch(Expect,Result).

read_data3()->
    Input = "a bb ccc \"Koustubh Sharma\" james bond",
    Expect = "a,bb,ccc,\"Koustubh Sharma\",james,bond",
    Result = clientListener:read_data(Input),
    ?assertMatch(Expect,Result).
read_data4()->
    Input = "a bb ccc \"Koustubh Sharma\" \"Hello World\"",
    Expect = "a,bb,ccc,\"Koustubh Sharma\",\"Hello World\"",
    Result = clientListener:read_data(Input),
    ?assertMatch(Expect,Result).
read_data5()->
    Input = "\"Koustubh Sharma\" a b {\"Hello World\" c}",
    Expect = "\"Koustubh Sharma\",a,b,{\"Hello World\" c}",
    Result = clientListener:read_data(Input),
    ?assertMatch(Expect,Result).








































%% check_execute_list()->

%%     [Result] = clientListener:execute_list(),
%%     io:format(user,"the result of execute info~p~n",[Result]),
%%     Res = re:run(Result,"For Module \"ets_table1\" the Exported Functions are  [start,return_ok,greet_me,\n                                              module_info,module_info]"),
%%     ?assertMatch({match,_},Res).

%% check_execute_run()->
%%     Expect = "Hello Koustubh",
%%     Result = clientListener:execute_run(["run","ets_table1","greet_me","Koustubh"]),
%%     ?assertMatch(Expect,Result).

%% when_execute_run_should_crash()->
%%  %%   Result = clientListener:execute_run(["run","ets_table1","greet_me",]),

%%     Result = clientListener:execute_run(["run","ets_table1","return_ok",abcd]),
%%     ?assertMatch({error,_},Result).

%% check_execute_info_works()->
%%     Result = clientListener:execute_info(),
%%     io:format(user,"the result of execute info~p~n",[Result]),
%%     Res = re:run(Result,"For Module \".*\"  the md5 is \".*\" Compiled on date .* at time .*"),
%%     ?assertMatch({match,_},Res).

%% parse_test_() ->
%%     [{"Converting to a simple erlang string", fun to_erlang_string_1/0}].

%% to_erlang_string_1()->
%%     Input = "a b c",
%%     Expected = "[a,b,c]",
%%     Result = clientListener:to_erlang_string(Input),
%%     ?assertMatch(Expected, Result).

%% to_erlang_string_2()->
%%     Input = "a b c \"Quote afterspace\"",
%%     Expected = "[a,b,c,\"Quote afterspace\"]",
%%     Result = clientListener:to_erlang_string(Input),
%%     ?assertMatch(Expected, Result).

%% check_string_is_proper_1()->
%%     Input = "run ets_table1 greet_me \"Koustubh Sharma\"",
%%     Result = clientListener:check_string_is_proper(Input),
%%     ?assertMatch(true, Result).
    
    
%% 
    
















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
%%      {"check execute list works ",fun check_execute_list/0},
%%      {"check execute run works ",fun check_execute_run/0},
%%      {"check execute info works  ",fun check_execute_info_works/0},
%%      {"execute run fails when args are not proper",fun when_execute_run_should_crash/0},
 %%     {"check format with quoted variable",fun check_format_with_quoted_variables/0},
%%{"check format with quoted string within variable ",fun to_erlang_string_2/0}
