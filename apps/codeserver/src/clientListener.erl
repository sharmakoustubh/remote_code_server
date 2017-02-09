-module(clientListener).
-export([is_restricted/3,
	 start/0,
	 check_format/1,
	 parse/1,
	 parse_args/1,
	 execute_run/1,
	 execute_info/0,
	 execute_list/0
	]).

-include("record_definition.hrl").
-define(INPUT_ERROR,"This command is not executable; Executable commands are list,run with module_name function_name function_arguments,info,exit").

start()->
    Ref = make_ref(),
    Parent = self(),
    io:format("Starting clientListener server"),
    Result = gen_tcp:listen(50555, [list, {packet, 0},{active, false},{reuseaddr, true}]),
    case Result of
	{ok,LSock}->	
	    spawn(fun()-> 
			  Parent ! {ok, Ref},
			  register(?MODULE,self()),
			  loop(LSock)
		  end),
	    receive
		{ok,Ref} ->
		    ok
	    after 1000 ->
		    {error, "Couldn't start client listener"}
	    end;
	Error->
	    Error
    end.	



loop(LSock) ->
    {ok, Sock} = gen_tcp:accept(LSock),
    spawn(fun()-> do_recv(Sock) end),
    loop(LSock).


do_recv(Sock) ->
    case gen_tcp:recv(Sock, 0) of
	{ok, Data} ->
	    io:format("Got data: ~p~n", [Data]),
	    check_process_send(Data,Sock),
	    do_recv(Sock);
	{error, closed} ->
	    io:format("Connection closed~n", []);
	Error ->
	    io:format("Unhandled error: ~p~n", [Error])
    end.


check_process_send(Data,Sock)->
    Formatted_data = check_format(Data),
    Result =  process(Formatted_data,Sock),
  %%  io:format(user,"tcp send data Result~p~n",[Result]),
    send(Sock,Result).

check_format(Data)->
    Data_without_nextline = Data -- "\n",
    string:tokens(Data_without_nextline," "). 

process(Data,Sock)->
    Count_elements = count(Data,0),
    case hd(Data) of
    	"list"->
	    case Count_elements of
		1->
		    execute_list();
		_->
		    ?INPUT_ERROR
			
	    end;
	
	"run"->
	    case Count_elements >= 3 of
		true ->
		    execute_run(Data);	
		
		false ->
		    ?INPUT_ERROR
	    end;
	
	"info"->
	    case Count_elements of
		1->
		    execute_info();
		_->
		    ?INPUT_ERROR
	    end;

	"exit"->
	    case Count_elements of
		1->
		    
		    terminate(Sock);
		_->
		    ?INPUT_ERROR
	    end;
    	_->
    	    "This command is not executable; Executable commands are list,run,info,exit"
    end.


execute_list()->
    case file_handler:fetch() of 
	{error, no_response}->
	    "error; no_response";
	{error,Error}->
	    Error;
	{ok,List}->
	    Keys =  proplists:get_keys(List),	    
	    Rec_List =  [proplists:get_value(Key,List)||Key <-Keys],
	    Exported_funs = [Rec#module.exported||Rec <- Rec_List],
	    lists:merge(Keys, Exported_funs)
    end.

execute_run(Data)->
    {ok,Modules} = file_handler:fetch(),
    {Module,{Function,Arity}} = parse(Data),
   %% io:format(user,"Module ~p Function  ~p Arity ~p ~n", [Module,Function,Arity]),
    case is_restricted(Module,{Function,Arity}, Modules) of
	true ->
	    "Function is restricted to run";
	false ->
	    apply(list_to_atom(Module), Function, parse_args(Data))
	end.

execute_info()->
    case file_handler:fetch() of 
	{error, no_response}->
	    "error; no_response";
	{error,Error}->
	    Error;
	{ok,List}->
	    Keys =  proplists:get_keys(List),	    
	    Rec_List =  [proplists:get_value(Key,List)||Key <-Keys],
	    Md5s = [Rec#module.module_md5||Rec <- Rec_List],
	    Compile_times = [Rec#module.compile_time||Rec <- Rec_List],
	    lists:merge3(Keys,Md5s,Compile_times)
    end.


is_restricted(Module,Function,Modules)->
   %% io:format(user,"ModuleName should be ~p Key val should be ~p~n .....=====", [Module, Modules]),
    Restricted = file_handler:get_restricted(Module, Modules),
   %% io:format(user,"restricted inside is_restricted client listener.....===== ~p ~n", [Restricted]),
    lists:member(Function,Restricted).
  
parse(Data)->
   %% io:format(user,"Data ~p ~n", [Data]),
    [ Command, Module, Function | Args] = Data,
    Args_number = count(Args,0),
    Function_atom =  list_to_atom(Function),
    {Module,{Function_atom,Args_number}}.

parse_args(Data)->
    [ _, _Module, _Function | Args] = Data,
    [Arg||Arg<- Args ].

terminate(Sock)->
    gen_tcp:close(Sock).

send(Sock,Result)->
    Formatted_data = io_lib:format("~p~n",[Result]),
    gen_tcp:send(Sock, Formatted_data).

count([],Counter)->
    Counter;
count([_H|T],Counter)->
    count(T,Counter+1).

