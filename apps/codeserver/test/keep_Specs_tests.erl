-module(keep_Specs_tests).
-include_lib("eunit/include/eunit.hrl").
-export([fetch_the_list/0]).
keep_Specs_test_() ->
    {foreach,
     fun setup/0,
     fun cleanup/1,
     [fun spawn_keep_Specs_process/0,
      fun add_mod_fun_to_list/0,
      fun delete_mod_fun_from_list/0,
      fun fetch_the_list/0]}.

setup()->
    io:format("Setting up test~n"),
    keep_Specs:start().

cleanup(_) ->
    keep_Specs:terminate_process().

spawn_keep_Specs_process()->
    Pid= whereis(keep_Specs),
    ?assertMatch(true,is_pid(Pid)).

add_mod_fun_to_list() ->
    R1 = keep_Specs:add(ets_table1, {transform_entry, 2}),
    R2 = keep_Specs:add(ets_table1, {get_value, 1}),
    ?assertEqual({ok,added}, R1),
    ?assertEqual({ok,added}, R2),
    keep_Specs:delete(ets_table1,{transform_entry, 2}),
    keep_Specs:delete(ets_table1,{get_value, 1}).

delete_mod_fun_from_list() ->
    keep_Specs:add(ets_table1, {transform_entry, 2}),
    keep_Specs:add(ets_table1, {get_value, 1}),
    R = keep_Specs:delete(ets_table1,{transform_entry, 2}),
    keep_Specs:start(),
    ?assertEqual({ok,deleted},R).

fetch_the_list() ->
    keep_Specs:add(ets_table1, {get_value, 1}),
    Result = keep_Specs:fetch(),
    ?assertEqual({ok,[{ets_table1,{get_value,1}}]},Result).
