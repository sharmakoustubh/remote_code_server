-module(file_handler_tests).
-include_lib("eunit/include/eunit.hrl").
-define(Module_info,[[{exports,[{start,0},{module_info,0},{module_info,1}]},
		      {imports,[_]},
		      {attributes,[_]},
		      {compile,[_]}],
		     [{exports,[{ets_table1_test_,0},
				{test,0},
				{module_info,0},
				{module_info,1}]},
		      {imports,_},
		      {attributes,_},
		      {compile,_}]]).


file_handler_test_() ->
    {foreach,
     fun setup/0,
     fun cleanup/0,
     [fun check_files_in_directory/0,
      fun spawn_file_handler_process/0,
      fun check_record/0,
      fun admin_msg_restrict/0,
      fun admin_msg_unrestrict/0]
     %%      fun check_module_names_test/0,
     %%  fun check_module_info_test/0,
     %% fun check_record_when_a_file_is_added_test/0,
     %%   fun check_record_when_a_file_is_deleted_test/0]
    }.


setup() ->
    ok.

cleanup() ->
    ok.

spawn_file_handler_process()->
    file_handler:start(),
    Pid= whereis(file_handler),
    io:format("~p~n",[Pid]),    
    ?assertMatch(true,is_pid(Pid)).

admin_msg_restrict()->
    Expect = {ok,restricted},
    Ref = make_ref(),
    file_handler ! {self(), Ref, restrict,lists,{seq,2}},
    Got = receive 
		 {Ref,Result}->
		     Result
	     after 5000->
		     {error,timeout}
	     end,
    ?assertMatch(Expect,Got).
	
admin_msg_unrestrict()->
    Expect = {ok,unrestricted},
    Ref = make_ref(),
    file_handler ! {self(), Ref, restrict,lists,{seq,2}},
    Got    = receive  
		 {Ref,Result}->
		     Result
	     after 5000->
		     {error,tdimeout}
	     end,
    ?assertMatch(Expect,Got).



movein_a_file_to_loaded()->
   % os:cmd("cd ~/codeserver/apps/codeserver/loadedtmp"),
    os:cmd("mv ~/codeserver/apps/codeserver/loadedtmp/flyingfile.erl ~/codeserver/apps/codeserver/loaded").

remove_a_file_from_loaded()->
   % os:cmd("cd ~/codeserver/apps/codeserver/loadedtmp"),
    os:cmd("mv ~/codeserver/apps/codeserver/loaded/flyingfile.erl ~/codeserver/apps/codeserver/loadedtmp").


check_files_in_directory() ->
    Input1 = "/home/ekousha/codeserver/apps/codeserver/loaded",
    Input2 = "erl",
    Result = file_handler:get_files(Input1,Input2),
    Expected = ["/home/ekousha/codeserver/apps/codeserver/loaded/ets_table1.erl",
		"/home/ekousha/codeserver/apps/codeserver/loaded/ets_table1_tests.erl"],
    ?assertEqual(Expected, Result).


%% check_module_names_test()->
%%     Input = ["ets_table1.erl"],
%%     Result = file_handler:get_module_names(Input),
%%     Expected = ["ets_table1"],
%%     ?assertEqual(Expected, Result).

%% check_module_info_test()->
%%     Input = ["ets_table1"],
%%     Result = file_handler:get_module_info(Input),
%%     ?assertMatch(?Module_info,Result).

check_record()->
    Input_filepath = "/home/ekousha/codeserver/apps/codeserver/loaded",
    Input_filetype = "erl",
    Result = file_handler:create_record_now(Input_filepath,Input_filetype),
    ?assertMatch(Record_expected,Result).


%% check_record_when_a_file_is_added_test() ->
%%     Expected = ,
%%     Result = ,
%%     ?assertMatch(),
%%     movein_a_file_to_loaded(),

%%     Expected = ,
%%     Result = ,
%%     ?assertMatch(),
%%     remove_a_file_from_loaded().


%% check_record_when_a_file_is_deleted_test() ->
%%     movein_a_file_to_loaded(),
%%     Expected = ,
%%     Result = ,
%%     ?assertMatch(),
    
%%     remove_a_file_from_loaded(),
%%     Expected = ,
%%     Result = ,
%%     ?assertMatch().

