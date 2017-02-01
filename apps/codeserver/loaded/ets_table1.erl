
-module(ets_table1).
-export([start/0]).
 
 start() ->
     spawn(fun() ->
 		  ets:new(table,[set,named_table]),
 		  register(database,self()),
 		  loop()
 	  end).
 
 
 
 loop() ->
     receive	
 	{set,K,V,Pid} ->
 	    set_entry({K,V}),
 	    Pid!inserted;    
 	{get,K,Pid}->	
 	   Pid!get_value(K);
 	{delete,K,Pid}->
 	    delete_entry(K),
 	    Pid!deleted;
 	{transform,K,F,Pid} ->
 	   Pid!transform_entry(K,F);
 	printtable ->
 	    io:format("~p~n",[ets:tab2list(table)]);
 	deletetable ->
 	    ets:delete(table)
 	end,
     loop().
 
 set_entry({K,V})->
     ets:insert(table,[{K,V}]).
 
 get_value(K)->
     case ets:lookup(table,K) of
 	[{_,V}] ->
 	    V;
 	[] ->
 	    {error, "key does not exist"}
     end.
 
 delete_entry(K)->
     ets:delete(table,K).
 
 transform_entry(K,F)->
     V=get_value(K),
     case V of
 	{error, "key does not exist"} = Err ->
 	    Err;
 	_ ->
 	    case catch F(V) of
 		{'EXIT',_}->
 		    {error,"function not implementable"};
 		New_V->
 		    set_entry({K,New_V}),
 		    transformed    
 	    end	    
     end.
 
