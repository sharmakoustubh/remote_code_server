-module(keep_Specs_tests).
-export([keep_Specs_tests_/0,spawn_keep_Specs_process_test/0,add_mod_fun_to_list_test/0,delete_mod_fun_from_list_test/0,fetch_the_list_test/0]).
-include_lib("eunit/include/eunit.hrl").



keep_Specs_tests_() ->
    {foreach,
     fun setup/0,
     fun cleanup/1,
     [fun spawn_keep_Specs_process_test/0,
      fun add_mod_fun_to_list_test/0,
      fun delete_mod_fun_from_list_test/0,
      fun fetch_the_list_test/0]}.

setup()->
    keep_Specs:start().    

cleanup(_) ->
    keep_Specs ! terminate.


spawn_keep_Specs_process_test()->
    keep_Specs:start(),
    Pid= whereis(keep_Specs),
    ?assertMatch(true,is_pid(Pid)).
  

add_mod_fun_to_list_test() ->
    R1 = keep_Specs:add(ets_table1, {transform_entry, 2}),
    R2 = keep_Specs:add(ets_table1, {get_value, 1}),
    ?assertEqual({ok,added}, R1),
    ?assertEqual({ok,added}, R2),
    keep_Specs:delete(ets_table1,{transform_entry, 2}),
    keep_Specs:delete(ets_table1,{get_value, 1}).
    
delete_mod_fun_from_list_test() ->
    keep_Specs:add(ets_table1, {transform_entry, 2}),
    keep_Specs:add(ets_table1, {get_value, 1}),
    R = keep_Specs:delete(ets_table1,{transform_entry, 2}),
    keep_Specs:start(),
    ?assertEqual({ok,deleted},R).



fetch_the_list_test() ->
    %%  Expected = {_,{ok,added}},
    keep_Specs:start(),
    Ref = make_ref(), 
    keep_Specs ! {self(),Ref,fetch},
    Result =  receive
		  {Ref,Gotback}->
		      Gotback
	      after 5000 ->
		      error
	      end,
    ?assertEqual({ok,[{ets_table1,{get_value,1}}]},Result).




%% add_to_restrict_database_test()->
%%     Pid!{restrict,ets_table1,get_value,1}



