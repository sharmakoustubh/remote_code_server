-module(clientListener).
-export([]).

start_server()->
    spawn(fun()-> {ok, LSock} = gen_tcp:listen(50555, [binary, {packet, 0}, 
                                        {active, false}]),
    server(LSock) end).

server(LSock) ->
    
    {ok, Sock} = gen_tcp:accept(LSock),
    spawn(fun()-> do_recv(Sock) end),
    server(LSock).

do_recv(Sock) ->
    case gen_tcp:recv(Sock, 0) of
        {From_Sock, list} ->

	 Result = file_handler:file_handler_main_function([],[],[]),
	%%	Record to binary
	 gen_tcp:send(From_Sock,list_to_binary(Result));
	
        {From_Sock, run,ModuleName_bin, FunctionName_bin} ->
	
	    ModuleName = binary_to_list(ModuleName_bin)--"\n",
	    FunctionName = binary_to_list(FunctionName_bin)--"\n",
	    code:load_file(ModuleName),
	    Result = ModuleName:FunctionName(),
	    gen_tcp:send(From_Sock,list_to_binary(Result));
	
	{From_Sock, info,ModuleName} ->
	
	    code:load_file(ModuleName),
	    Result = ModuleName:module_info(),
	    gen_tcp:send(From_Sock,list_to_binary(Result));
	
	{From, exit,Sock} ->
	    
	    gen_tcp:close(Sock)
	    
	
	end.


