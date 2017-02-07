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
    ok = file_handler:start(),
    ok = adminListener:start().

cleanup(_) ->
    exit(whereis(adminListener),kill),
    ensure_exited_adminListener(),
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
    Result = adminListener:check_format("restrict module function 2"),
    ?assertMatch(Expect, Result).


check_parse()->
    Expect = {"My_mod",{my_fun,2}},
    Result = adminListener:parse(["restrict","My_mod","my_fun","2"]),
    ?assertEqual(Expect,Result).

execute_restrict_should_return_ok_upon_pass()->
    Result = adminListener:execute_restrict(["restrict","ets_table1","start","0"]), 
    ?assertEqual("the module was restricted",Result).

execute_unrestrict_should_return_ok_upon_pass()->
    "the module was restricted" = adminListener:execute_restrict(["restrict","ets_table1","start","0"]), 
    Result = adminListener:execute_unrestrict(["unrestrict","ets_table1","start","0"]), 
    ?assertEqual("the module was unrestricted",Result).

execute_delete_module_keyval_should_return_ok_upon_pass()->
    Result = adminListener:execute_delete(["delete","ets_table1"]), 
    ?assertEqual(ok,Result).

%% execute_list_clients_should_provide_details_of_socks()->
%%     Sock = <0.43>,
%%     Result = adminListener:execute_list_clients(Sock), 
%%     ?assertEqual(ok,Result).


 































%% admin_msg_restrict()->
%%     Module = {"my_moduleRes", #module{restricted = []}},
%%     Result = file_handler:restrict("my_moduleRes", {fRes, 0}, [Module]),
%%     Expected = [{"my_moduleRes",#module{restricted = [{fRes, 0}]}}],
%%     ?assertEqual(Expected, Result).

%% admin_msg_unrestrict()->
%%     Module = {"my_module", #module{restricted = [{f, 0}]}},
%%     Result = file_handler:unrestrict("my_module", {f, 0}, [Module]),
%%     Expected = [{"my_module",#module{restricted = []}}],
%%     ?assertEqual(Expected, Result).

%% admin_msg_delete_module()->
%%     Expected = {ok,deleted_module},
%%     Ref = make_ref(),

%%     file_handler ! {self(), Ref, delete_module,"ets_table1"},
%%     Got    = receive  
%% 		 {Ref,Result}->
%% 		     Result
%% 	     after 5000->
%% 		     {error,timeout}
%% 	     end,
%%     ?assertMatch(Expected,Got).

%% check_files_in_directory() ->
%%     Result = file_handler:get_files(),
%%     Expected = ["/home/ekousha/codeserver/apps/codeserver/loaded/ets_table1.erl"],
%%     ?assertEqual(Expected, Result).

%% load_file_from_dir()-> 
%%     Expected = {module,ets_table1},
%%     Result =file_handler:compile_and_load_file_from_dir(ets_table1,"/home/ekousha/codeserver/apps/codeserver/loaded/ets_table1.erl"),
%%     ?assertEqual(Expected, Result).

%% check_module_name()->
%%     Input1 = "/home/ekousha/codeserver/apps/codeserver/loaded/ets_table1.erl",
%%     Result = file_handler:get_module_name(Input1),
%%     Expected = "ets_table1",
%%     ?assertEqual(Expected, Result).

%% add_dir_to_path()->
%%     Expected = true, 
%%     Result = file_handler:add_dir_to_path("/home/ekousha/codeserver/apps/codeserver/loaded"),
%%     ?assertEqual(Expected,Result).


%% get_md5()->
%%     Expected = "2FEFF17C44894ADFB17326621C8ACA8A",
%%     Got =  file_handler:get_md5("/home/ekousha/codeserver/apps/codeserver/loaded/ets_table1.erl"),
%%     ?assertMatch(Expected,Got).
