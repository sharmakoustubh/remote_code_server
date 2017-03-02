-module(clientListener).
-define(Complements,[{$",$"},{$',$'},{$[,$]},{${,$}},{$\",$\"}]).
-export([is_restricted/3,
	 start/0,
	 parse/1,
	 execute_run/1,
	 execute_info/0,
	 execute_list/0,
	 to_expression/1,
	 tokenise_data_for_n_terms/3,
	 format/1,
	 process/2
	]).

-include("record_definition.hrl").
-define(INPUT_ERROR,"This command is not executable; Executable commands are list,run with module_name function_name function_arguments,info,exit").

start()->
    Ref = make_ref(),
    Parent = self(),
    io:format("Starting clientListener server"),
    Result = gen_tcp:listen(50557, [list, {packet, 0},{active, false},{reuseaddr, true}]),
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
    Output = case process(Formatted_data,Sock) of
		 {error,Error}->
		     Error;
		 Result->
		     Result 
	     end,    
    send(Sock,Output).

format(Data)->
    case "\n" == Data of
	true ->
	    Data;
	false ->
	    Data -- "\n"
    end.

process(Data,Sock)->
    [Cmd|Rest] = string:tokens(Data," "),
    Count_elements = length(Rest),
    case Cmd of
    	"list"->
	    case Count_elements of
		0->
		    execute_list();
		_->
		    ?INPUT_ERROR
			
	    end;
	
	"run"->	    
	    case Count_elements >= 2 of
		true ->
		    execute_run(Data);	
		
		false ->
		    ?INPUT_ERROR
	    end;
	
	"info"->
	    case Count_elements of
		0->
		    execute_info();
		_->
		    ?INPUT_ERROR
	    end;

	"exit"->
	    case Count_elements of
		0->
		    
		    terminate(Sock);
		_->
		    ?INPUT_ERROR
	    end;
    	_->
    	    lists:flatten(io_lib:format("~s~n",["This command is not executable; Executable commands are list,run,info,exit"]))
    end.
    

execute_list()->
    case file_handler:fetch() of 
	{error, no_response}->
	    "error; no_response";
	{error,Error}->
	    Error;
	{ok,List}->
	    Res = [pretty_print({Key,Rec#module.exported--Rec#module.restricted}) ||{Key,Rec}<-List],
	    lists:flatten(io_lib:format("~s~n",[Res]))
    end.


pretty_print(H) ->
    {Mod_name,Funs}=H, 
    Module_format = lists:flatten (io_lib:format("~s~n",[Mod_name])),
    Function_format_list = [lists:flatten (io_lib:format("  ~p/~p~n",[Fun, Arity]))|| {Fun, Arity} <- Funs],
    Function_format_flat = lists:flatten(io_lib:format("~s~n",[Function_format_list])),
    Module_format ++ Function_format_flat.


execute_info()->
    case file_handler:fetch() of 
	{error, no_response}->
	    "error; no_response";
	{error,Error}->
	    Error;
	{ok,List}->
	    Res = [pretty_print_for_execute_info({Key,Rec#module.module_md5,Rec#module.compile_time})||{Key,Rec}<-List],
	    lists:flatten(io_lib:format("~s~n",[Res]))
    end.


pretty_print_for_execute_info(H) ->
    {Mod_name,Md5,{Year,Month,Date,Hrs,Min,Seconds}}=H, 
    Module_format = lists:flatten (io_lib:format("~s~n",[Mod_name])),
    Function_format_list = [lists:flatten (io_lib:format(" the md5 is ~p Compiled on date ~p-~p-~p at time ~p:~p:~p~n",[Md5,Year,Month,Date,Hrs,Min,Seconds]))],
    Function_format_flat = lists:flatten(io_lib:format("~s~n",[Function_format_list])),
    Module_format ++ Function_format_flat.


execute_run(Data)->
    {ok, Modules} = file_handler:fetch(),
    case catch parse(Data) of
	{'EXIT', _}->
	    io_lib:format("~p~n",["You have entered an expression that cannot be parsed please enter in the following format: Command Module Function Arg1, Arg2, Arg3"]);
	{Module, {Function, Arity}, Args_list}->
	    Result = run_seq([fun module_exists/3,
			      fun is_exported/3,
			      fun is_restricted/3
			     ],{ok,Module,{Function,Arity},Modules}),
	    case Result of
		{ok,"The Function is un-restricted !!!"} ->
		    Run_fun_res =  run_fun(Module,Function,Args_list),
		    io_lib:format("~p~n",[Run_fun_res]);	    
		{error,Msg}->
		    io_lib:format("~p~n",[Msg])
	    end;
	_->
	    io_lib:format("~p~n",["You have entered an expression that cannot be parsed please enter in the following format: Command Module Function Arg1, Arg2, Arg3"])	  
    end.

parse(Data)->
    [ _, Module, Function | Args] = tokenise_data_for_n_terms(Data,[],3),
    Args_list = to_expression("["++Args++"]"),
    Args_number = length(Args_list),
    Function_atom =  list_to_atom(Function),
    {Module,{Function_atom,Args_number},Args_list}.

tokenise_data_for_n_terms([],Acc,_)->
    string:tokens(lists:reverse(Acc),",");

tokenise_data_for_n_terms(T,Acc,0)->
    lists:append(string:tokens(lists:reverse(Acc),","),T);

tokenise_data_for_n_terms([H|T],Acc,Max)->
    case H of
	$\s ->
	    tokenise_data_for_n_terms(T,[$,|Acc],Max-1);
	_ ->
	    tokenise_data_for_n_terms(T,[H|Acc],Max)
    end.

to_expression(String) ->
    {ok, Tokens,_ } = erl_scan:string(String ++ "."),
    {ok, Abstract} = erl_parse:parse_exprs(Tokens),
    {value, Expression, _} = erl_eval:exprs(Abstract, erl_eval:new_bindings()),
    Expression.

run_fun(Module,Function,Args_list)->
    case catch apply(list_to_atom(Module), Function, Args_list) of	
    	{'EXIT',Error} ->
    	    {error,Error};
    	Res->
    	    Res		
    end.

run_seq([],Res)->
    Res;
run_seq([F|T],{ok,Module,Function,Modules}) ->
    run_seq(T,F(Module,Function,Modules));
run_seq(_,{error,Msg}) ->
    {error,Msg}.

module_exists(Module,Function,Modules)->
    case lists:member(Module,proplists:get_keys(Modules)) of
	true ->
	    {ok,Module,Function,Modules};
	false ->
	    {error,"The Module does not exist !!!"}
    end.

is_exported(Module,Function,Modules)->
    Record = proplists:get_value(Module,Modules),
    Exported_funs =Record#module.exported,
    case lists:member(Function,Exported_funs) of
	true ->
	    {ok,Module,Function,Modules};
	false ->
	    {error,"The Function does not exist !!!"}
    end.

is_restricted(Module,Function,Modules)->
    Restricted = file_handler:get_restricted(Module, Modules),
    case lists:member(Function,Restricted) of
	true ->
	    {error,"The Function is restricted !!!"};
	false ->
	    {ok,"The Function is un-restricted !!!"}		
    end.


terminate(Sock)->
    gen_tcp:close(Sock).

send(Sock,Result)->
    gen_tcp:send(Sock, Result).

