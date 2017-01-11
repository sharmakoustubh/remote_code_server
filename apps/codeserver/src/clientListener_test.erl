-module(clientListener_test).
-export([]).

start_server()->
    spawn(fun()-> {ok, LSock} = gen_tcp:listen(50556, [binary, {packet, 0}, 
                                        {active, false}]),
    serverTest(LSock) end).

serverTest(LSock) ->
    %% {ok, Sock} = gen_tcp:send(LSock),
    %% spawn(fun()-> do_recv(Sock) end),
    %% serverTest(LSock),
    ok.

%% do_recv(Sock) ->
%%     case gen_tcp:recv(Sock, 0) of
%%         {ok, B} ->
%% 	    Input= binary_to_list(B)--"\n";
	    
%% 	    %% gen_tcp:send(Sock,list_to_binary(Res)),
%% 	    %% gen_tcp:close(Sock);
%% 	{error, closed} ->
%% 	    ok
	    
%%     end.

