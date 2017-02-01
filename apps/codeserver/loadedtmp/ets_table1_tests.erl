-module(ets_table1_tests).
-export([test_1/0,setup/0,cleanup/0]).
-include_lib("eunit/include/eunit.hrl").




ets_table1_test_() ->
    {foreach,
     fun setup/0,
     fun cleanup/0,
     [fun test_1/0]
    }.

test_1() ->
    database!{set,1,a,self()},
    R = receive
	    Resp ->
		Resp
	after 100 ->
		failed
	end,
    ?assertEqual(inserted, R).
    

setup()->
    
    database!{set,1,a,self()},
    database!{set,2,b,self()},
    database!{set,3,c,self()}.

cleanup()->
    exit(whereis(database),kill).



