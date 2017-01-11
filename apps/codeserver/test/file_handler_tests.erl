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

-define(Record_expected,[[[{file_poller_record,"ets_table1","erl",_,_},
			   {file_poller_record,"ets_table1_tests","erl",_,_}],
			  {file_poller_record,"ets_table1","beam",_,_},
			  {file_poller_record,"ets_table1_tests","beam",_,_}]]).

file_handler_tests_() ->
    {foreach,
     fun setup/0,
     fun cleanup/0,
     [fun check_files_in_directory_test/0,
     
      fun check_record/0]
    %%      fun check_module_names_test/0,
%%  fun check_module_info_test/0,
%% fun check_record_when_a_file_is_added_test/0,
   %%   fun check_record_when_a_file_is_deleted_test/0]
    }.


setup() ->
    ok.

cleanup() ->
    ok.

movein_a_file_to_loaded()->
   % os:cmd("cd ~/codeserver/apps/codeserver/loadedtmp"),
    os:cmd("mv ~/codeserver/apps/codeserver/loadedtmp/flyingfile.erl ~/codeserver/apps/codeserver/loaded").

remove_a_file_from_loaded()->
   % os:cmd("cd ~/codeserver/apps/codeserver/loadedtmp"),
    os:cmd("mv ~/codeserver/apps/codeserver/loaded/flyingfile.erl ~/codeserver/apps/codeserver/loadedtmp").


check_files_in_directory_test() ->
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

