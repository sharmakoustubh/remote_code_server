-module(star_print_server).
-export([star_print/3,star_print/1,check_format/3,start_server/0]).

start_server()->
    spawn(fun()-> {ok, LSock} = gen_tcp:listen(5678, [binary, {packet, 0}, 
                                        {active, false}]),
    server(LSock) end).

server(LSock) ->
    
    {ok, Sock} = gen_tcp:accept(LSock),
    spawn(fun()-> do_recv(Sock) end),
    server(LSock).

do_recv(Sock) ->
    case gen_tcp:recv(Sock, 0) of
        {ok, B} ->
	    Input= binary_to_list(B)--"\n",
	    
	    Res= case string:tokens(Input," ") of
		     [Rows,Columns,Char]->
			 
			 case check_format(Rows,Columns,Char)of
			     ok->
				 [Ch]=Char,
				 R=list_to_integer(Rows),
				 C=list_to_integer(Columns),			  
				 star_print(R,C,Ch);
			     Error ->
				 io_lib:format("~p~n",[Error])
			 end;
		     _->
			 Error={error, "incorrect format"},
			 io_lib:format("~p~n",[Error])
			 
		 end,
	    gen_tcp:send(Sock,list_to_binary(Res)),
	    gen_tcp:close(Sock);
	{error, closed} ->
	    ok
    end.




star_print(Rows,Columns,S) ->
    case is_printable(S) of
	true->				  
	    case(Rows<0 orelse Columns<0)of
		true ->
		    {error,"negative row or column entry, please enter valid row or column"};
		                                                                   
		false->
		    StringRow=lists:duplicate(Columns,S),
		    FinalPattern = lists:duplicate(Rows,lists:flatten(StringRow)++"\n"),
		    lists:flatten(FinalPattern)		
			
	    end;

	false->
	    {error,"please enter printable character"}
	    
	end.
		

is_printable(S)->
    S>=32 andalso S=<255.



star_print(File_path)->
    case filelib:is_file(File_path)of
    	false ->
    	    {error,"No such file exists"};
	
	true -> 
	    {_,File_content} =  file:read_file(File_path),
	    File_content_list= binary_to_list(File_content),
	    case string:tokens(File_content_list," ") of
		[Rows,Columns,Char]->
		 
		    case check_format(Rows,Columns,Char)of
			ok->
			    [Ch]=Char,
			    R=list_to_integer(Rows),
			    C=list_to_integer(Columns),			  
			    star_print(R,C,Ch);
			Error ->
			    Error
		    end;
		_->
		    {error, "incorrect file format"}
	    end
    end.

check_format(Rows,Columns,Char)->
   case check_rows_are_integers(Rows) of
       ok->
	   case check_columns_are_integers(Columns)of
	       ok->
		   validate_char_length(Char);
	       Error->
		      Error
	   end;
       Error->
	   Error
   end.
		      
check_rows_are_integers(Rows)->
    case catch list_to_integer(Rows) of
	X when is_integer(X) ->
	    ok;
	_ ->
	  {error, "Rows is not an integer"}
		
    end.

check_columns_are_integers(Columns)->
    case catch list_to_integer(Columns) of
	Y when is_integer(Y) ->
	    ok;
	_ ->
	   {error, "Columns is not an integer"}
		
    end.

validate_char_length(Char)->
    case length(Char) of
	1->
	    ok;
	_ ->
	    {error, "Char should be of length 1"}   
    end.
