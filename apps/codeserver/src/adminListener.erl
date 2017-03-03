-module(adminListener).
-export([start/0,
	 format/1,
	 parse/1,
	 execute_restrict/1,
	 execute_unrestrict/1,
	 execute_delete/1,
	 execute_list_clients/1
	]).

-define(INPUT_ERROR,"This command is not executable; Executable commands are restrict with module_name function_name function_arity,unrestrict with module_name function_name function_arity,delete,list_clients,exit").


start()->
    Ref = make_ref(),
    Parent = self(),
    io:format("Starting adminListener server~n"),
    Result = gen_tcp:listen(50556, [list, {packet, 0}, {active, false}]),
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
		    {error, "Couldn't start admin listener"}
	    end;
	Error->
	    Error
    end.	

loop(LSock) ->
    {ok, Sock} = gen_tcp:accept(LSock),
    spawn(fun()-> handle_connection(Sock) end),
    loop(LSock).


handle_connection(Sock) ->
    case gen_tcp:recv(Sock, 0) of
	{ok, Data} ->
	    io:format("Got data: ~p~n", [Data]),
	    format_process_send(Data,Sock),
	    handle_connection(Sock);
	{error, closed} ->
	    io:format("Connection closed~n", []);
	Error ->
	    io:format("Unhandled error: ~p~n", [Error])
    end.


format_process_send(Data,Sock)->
    Formatted_data = format(Data),
    Result =  process(Formatted_data,Sock),
    send(Sock,Result).

format(Data)->
    Data_without_nextline = Data -- "\n",
    string:tokens(Data_without_nextline," "). 

process(Data,Sock)->
    Count_elements = length(Data),
    case hd(Data) of
    	"restrict"->
	    case Count_elements of
		4->
		    execute_restrict(Data);
		_->
		    ?INPUT_ERROR
		    
	    end;
	
	"unrestrict"->
	    case Count_elements of
		4->
		    execute_unrestrict(Data);	
		
		_->
		    ?INPUT_ERROR
	    end;
	
	"list_clients"->
	    case Count_elements of
		1->
		    execute_list_clients(Sock);
		_->
		    ?INPUT_ERROR
	    end;

	"delete"->
	    
	    case Count_elements of
		2->
		    execute_delete(Data);
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
    	    "This command is not executable; Executable commands are restrict,unrestrict,delete,list_clients,exit"
    end.

execute_restrict(Data)->
    {Module,{Function,Arity}} = parse(Data),
    case file_handler:restrict(Module,{Function,Arity}) of 
	{error, no_response}->
	    "error; no_response";
	{error,Error}->
	    Error;
	{ok,Result}->
	    Result	    
    end.


execute_unrestrict(Data)->
    {Module,{Function,Arity}} = parse(Data),
    case file_handler:unrestrict(Module,{Function,Arity}) of 
	{error, no_response}->
	    "error; no_response";
	{error,Error}->
	    Error;
	{ok,Result}->
	    Result	    
    end.

execute_delete(Data)->
    Module = parse_delete_module(Data),
    case file_handler:delete_module_keyval(Module) of 
	ok ->
	    ok;
	_->
	    "Could not delete module; Module does not exist"
		
    end.

execute_list_clients(Sock)->
   {ok,{{IP1,IP2,IP3,IP4},Port}} = inet:sockname(Sock),
    Result = lists:flatten(io_lib:format("~p", [Port])),
    "IP is "++ integer_to_list(IP1)++"."++integer_to_list(IP2)++"."++ integer_to_list(IP3)++"."++ integer_to_list(IP4)++"."++"and port of the clients are "++Result.

parse(Data)->
    [_,Module,Function,Arity] = Data,
    Function_atom =  list_to_atom(Function),
    Arity_atom = list_to_integer(Arity),
    {Module,{Function_atom,Arity_atom}}.

parse_delete_module(Data)->
    [_,Module] = Data,
    Module.

terminate(Sock)->
    gen_tcp:close(Sock).

send(Sock,Result)->
    Formatted_data = io_lib:format("~p~n",[Result]),
    gen_tcp:send(Sock, Formatted_data).

