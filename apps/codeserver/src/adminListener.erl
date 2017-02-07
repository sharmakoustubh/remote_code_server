-module(adminListener).
-export([start/0,
	 check_format/1,
	 parse/1,
	 execute_restrict/1,
	 execute_unrestrict/1,
	 execute_delete/1
	]).

start()->
    Ref = make_ref(),
    Parent = self(),
    io:format("Starting adminListener server"),
    
    spawn(fun()-> {ok, LSock} = gen_tcp:listen(50556, [list, {packet, 0},{active, false}]),
		  Parent ! {ok, Ref},
		  true = register(?MODULE,self()),
		  loop(LSock) end),
    receive
	{ok,Ref} ->
	    ok
    after 1000 ->
	    {error, "Couldn't start main"}
    end.


loop(LSock) ->
    
    {ok, Sock} = gen_tcp:accept(LSock),
    spawn(fun()-> do_recv(Sock) end),
    loop(LSock).


%% restrict module function 2


do_recv(Sock) ->
    case gen_tcp:recv(Sock, 0) of
	{ok, Data} ->
	    io:format("Got data: ~p~n", [Data]),
	    check_process_send(Data),
	    do_recv(Sock);
	Error ->
	    io:format("Error! ~p~n", [Error])
    end.


check_process_send(Data)->
    Formatted_data = check_format(Data),
    Result =  process(Formatted_data),
    send(Result).

check_format(Data)->
    Data_without_nextline = Data -- "\n",
    string:tokens(Data_without_nextline," ").

process(Data)->
    case hd(Data) of
    	"restrict"->
    	    execute_restrict(Data);
    	 "unrestrict"->
            execute_unrestrict(Data);
    	%% "list_clients"->
    	%%     execute_list_clients(Data);
    	 "delete"->
    	     execute_delete(Data);
    	%% "exit"->
    	%%     terminate(Data);
    	_->
    	    {error,command_not_executable}
    end.

send(Result)->
    ok.

execute_restrict(Data)->
    [Module,{Function,Arity}] = parse(Data),
       file_handler:restrict(Module,{Function,Arity}).

execute_unrestrict(Data)->
    [Module,{Function,Arity}] = parse(Data),
       file_handler:unrestrict(Module,{Function,Arity}).

execute_delete(Data)->
    [Module] = parse_delete_module(Data),
    io:format(user,"execute delete module mod~p~n",[Module]),
    file_handler:delete_module_keyval(Module).
   

parse(Data)->
    [_,Module,Function,Arity] = Data,
    Function_atom =  list_to_atom(Function),
    Arity_atom = list_to_integer(Arity),
    [Module,{Function_atom,Arity_atom}].

parse_delete_module(Data)->
    [_,Module] = Data,
    [Module].

