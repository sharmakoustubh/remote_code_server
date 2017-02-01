-module(adminListener).
-export([]).

start_server()->
    spawn(fun()-> {ok, LSock} = gen_tcp:listen(50556, [binary, {packet, 0}, 
						       {active, false}]),
		  server(LSock) end).

server(LSock) ->
    
    {ok, Sock} = gen_tcp:accept(LSock),
    spawn(fun()-> do_recv(Sock) end),
    server(LSock).



%% restrict module function 2


do_recv(Sock) ->
    case gen_tcp:recv(Sock, 0) of
        {From_Sock, restrict,ModuleName_bin, FunctionName_bin} ->
	    ModuleName = binary_to_list(ModuleName_bin)--"\n",
	    FunctionName = binary_to_list(FunctionName_bin)--"\n",
	    keep_Specs:update_restrict_database(ModuleName,FunctionName);
	
        {From_Sock, un_restrict,ModuleName_bin, FunctionName_bin} ->
	    ModuleName = binary_to_list(ModuleName_bin)--"\n",
	    FunctionName = binary_to_list(FunctionName_bin)--"\n",
	    
	    keep_Specs:update_un_restrict_database(ModuleName,FunctionName)
	
	
	
	%% {From_Sock, list_clients} ->
	    
	%%     ;								  
	%% {From_Sock,delete,ModuleName_bin} ->
	    
	%%     ;
	%% {From_Sock, exit,Sock} ->
	    
	%%     gen_tcp:close(Sock);
	
	 end.

specs_list(ModuleName,FunctionName)->
    keep_Specs:update_database(ModuleName,FunctionName,restrict) .


%% function_specs() ->
    
