-module(clientListener_test).
-export([]).
-include_lib("eunit/include/eunit.hrl").

perform_clientListener_test_() ->
    {foreach,
     fun setup/0,
     fun cleanup/1,
     [fun function_is_restricted/0,
      fun function_is_not_restricted/0
     ]}.

setup()->
 keep_Specs:start().

cleanup(_)->
    keep_Specs:terminate_process().


function_is_not_restricted()->   
    Expected = false,
    Result = clientListener:is_restricted(lists,{seq,2}),
    ?assertEqual(Expected,Result).

function_is_restricted()->
    keep_Specs:add(lists,{seq,2}),
    Expected = true,
    Result = clientListener:is_restricted(lists,{seq,2}),
    ?assertEqual(Expected,Result).

runs_unrestricted_function()->
    Expected = true,
    Result = clientListener:is_restricted(lists,{seq,2}),
    ?assertEqual(Expected,Result).


