-module(keep_Specs).
-export([start/0, add/2,delete/2,fetch/0,terminate_process/0]).

start()->
    Ref = make_ref(),
    Parent = self(),
    io:format("Starting"),
    spawn(fun() ->		  
		  register(?MODULE, self()),
		  Parent ! {Ref, started},
		  loop([])
	  end),
    receive
	{Ref, started} ->
	    ok
    after 1000 ->
	    {error, "Couldn't start keep_Specs"}
    end.

loop(Old_List)->
    receive 
	{From, Ref, add,Module,Function} ->
	    From ! {Ref, {ok,added}},
	    loop([{Module,Function}|Old_List]);	
	{From,Ref,delete,Module, Function} ->
	    New_List = delete_from_list(From,Ref,Module, Function,Old_List),
	    loop(New_List);
	{From,Ref,fetch} ->
	    do_fetch(From,Ref,Old_List),
	    loop( Old_List);
	{From,Ref,terminate} ->	    
	   From !{Ref,terminated},
	    ok
    end.

add(Module, Function) ->
    Ref = make_ref(),
    ?MODULE ! {self(), Ref, add, Module, Function},
    receive 
	{Ref, Res} ->
	    Res
    after 500 ->
	    {error, timeout}
    end.

delete(Module, Function) ->
    Ref = make_ref(),
    ?MODULE ! {self(), Ref, delete, Module, Function},
    receive 
	{Ref, Res} ->
	    Res
    after 500 ->
	    {error, timeout}
    end.

delete_from_list(From,Ref,Module,Function,Old_List)->
    case lists:member({Module,Function},Old_List) of
	true ->
	    From ! {Ref,{ok,deleted}},
	    lists:delete({Module,Function},Old_List);
	false ->
	    From ! {Ref,{error,not_found}},
	    Old_List
    end.

do_fetch(From,Ref,Old_List)->
    From !{Ref,{ok,Old_List}}.


fetch() ->
    Ref = make_ref(),
    ?MODULE ! {self(),Ref,fetch},
    receive 
	{Ref, Res} ->
	    Res
    after 500 ->
	    {error, timeout}
    end.

terminate_process()->
    Ref=make_ref(),
    ?MODULE ! {self(),Ref,terminate},
    receive 
	{Ref,terminated}->
	    timer:sleep(100),
	    ok
    after 500 ->
	    {error,timeout}
    end.
