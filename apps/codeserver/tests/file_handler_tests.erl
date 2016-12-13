-module(file_handler_tests).
-include_lib("eunit/include/eunit.hrl").
%% -export([check_files_in_directory_test/0,check_module_names_test/0]).


%% file_handler_tests()->
%%     check_files_in_directory(),
%%     check_module_names(),
%%     goodtest.

check_files_in_directory_test() ->
    Input1 = "/home/ekousha/Documents/Erlang_Projs/loaded",
    Input2 = "erl",
    Result = file_handler:get_files(Input1,Input2),
    Expected = ["/home/ekousha/Documents/Erlang_Projs/loaded/ets_table1.erl",
 "/home/ekousha/Documents/Erlang_Projs/loaded/ets_table1_tests.erl"],
    ?assertEqual(Expected, Result).


check_module_names_test()->
    Input = ["ets_table1.erl","ets_table1_tests.erl"],
    Result = file_handler:get_module_names(Input),
    Expected = ["ets_table1","ets_table1_tests"],
    ?assertEqual(Expected, Result).

%% check_module_info()->
%%     Input = [ets_table1],
%%     Result = file_handler:get_module_info(Input),
%%     ?assertMatch([{exports,[{start,0},{module_info,0},{module_info,1}]},
%% 		 {imports,[]},
%% 		 {attributes,_},
%% 		 {compile,_}],Result)
%%  fun check_module_info/0	.



    
