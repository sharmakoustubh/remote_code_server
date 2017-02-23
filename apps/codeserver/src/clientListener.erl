-module(clientListener).
-define(Complements,[{$",$"},{$',$'},{$[,$]},{${,$}},{$\",$\"}]).
-export([is_restricted/3,
	 start/0,
	 check_format/1,
	 parse/1,
	 parse_args/1,
	 execute_run/1,
	 execute_info/0,
	 execute_list/0,
	 to_expression/1,
	 read_data/1,
	 get_block_and_remainder/3
%%	 to_erlang_string/1
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
    Output = case catch process(Formatted_data,Sock) of
		 {'EXIT',Error}->
		     {error,Error};
		 Result->
		    Result 
	     end,

%%     io:format(user,"tcp send data Result~p~n",[Output]),
    send(Sock,Output).

check_format(Data)->
    Data_without_nextline = Data -- "\n",
    Formatted_data = read_data(Data_without_nextline),
    string:tokens(Formatted_data,","). 

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
	    %% Res = [{Key,Rec#module.exported}||{Key,Rec}<-List],
	    %% Formatted_output = format_result(Res,[]),
       
	    Res = [pretty_print({Key,Rec#module.exported}) ||{Key,Rec}<-List],
	    Res_format = lists:flatten(io_lib:  format("~s~n",[Res])),
	   %% io:format(user,"~s~n", [Res_format])
	    Res_format
	%%    [lists:flatten (io_lib:format(" ~p~n  ~p",[Key,Function_names]))||{Key,Function_names} <- Formatted_output] 	    
    end.


pretty_print(H) ->
    {Mod_name,Funs}=H, 
    Module_format = lists:flatten (io_lib:format("~s~n",[Mod_name])),
    Function_format_list = [lists:flatten (io_lib:format("  ~p/~p~n",[Fun, Arity]))|| {Fun, Arity} <- Funs],
    io:format(user,"~p~n",[Function_format_list]),
    Function_format_flat = lists:flatten(io_lib:format("~s~n",[Function_format_list])),
%%    io_lib:format(user,"~s~n",Module_format ++ Function_format_flat).
      Module_format ++ Function_format_flat.


execute_info()->
    case file_handler:fetch() of 
	{error, no_response}->
	    "error; no_response";
	{error,Error}->
	    Error;
	{ok,List}->
	   Res = [pretty_print_info({Key,Rec#module.module_md5,Rec#module.compile_time})||{Key,Rec}<-List],
	%%    [lists:flatten (io_lib:format("For Module ~p the md5 is ~p Compiled on date ~p-~p-~p at time ~p:~p:~p~n",[Key,Md5,Date,Month,Year,Hrs,Min,Seconds]))||{Key,Md5,{Year,Month,Date,Hrs,Min,Seconds}} <- Res]
	    Res_format = lists:flatten(io_lib:  format("~s~n",[Res])),
	    Res_format  
    end.


pretty_print_info(H) ->
    {Mod_name,Md5,{Year,Month,Date,Hrs,Min,Seconds}}=H, 
    Module_format = lists:flatten (io_lib:format("~s~n",[Mod_name])),
    Function_format_list = [lists:flatten (io_lib:format(" the md5 is ~p Compiled on date ~p-~p-~p at time ~p:~p:~p~n",[Md5,Year,Month,Date,Hrs,Min,Seconds]))],


    io:format(user,"~p~n",[Function_format_list]),
    Function_format_flat = lists:flatten(io_lib:format("~s~n",[Function_format_list])),
%%    io_lib:format(user,"~s~n",Module_format ++ Function_format_flat).
      Module_format ++ Function_format_flat.



    
execute_run(Data)->
    {ok,Modules} = file_handler:fetch(),
    {Module,{Function,Arity}} = parse(Data),
    Result =   run_seq([fun module_exists/3,
			fun is_exported/3,
			fun is_restricted/3
		       ],{ok,Module,{Function,Arity},Modules}),
    case Result of
	{ok,"The Function is un-restricted !!!"} ->
	    Run_fun_res =  run_fun(Module,Function,Data),
	    Formatted_data = io_lib:format("~p~n",[Run_fun_res]),
	    Formatted_data;
        {error,Msg}->
	    Msg
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
	  %%io:format(user,"the module_exists is TRUE~p~n",[true]),
	    {ok,Module,Function,Modules};
	false ->
	    {error,"The Module does not exist !!!"}
    end.

is_exported(Module,Function,Modules)->
    Record = proplists:get_value(Module,Modules),
    Exported_funs =Record#module.exported,
%%  io:format(user,"the passed function to the module is ~p~n",[Function]),
      %%io:format(user,"the Exported function  to the module is ~p~n",[Exported_funs]),
    case lists:member(Function,Exported_funs) of
	true ->
	   %%o:format(user, "the Is_Exported is TRUE~p~n",[true]),
	    {ok,Module,Function,Modules};
	false ->
	%%   io:format(user,"the Funcs --- does not exist return tuple error it does not exist ~p~n",[func_doesnt_exist]),
	    {error,"The Function does not exist !!!"}
    end.


is_restricted(Module,Function,Modules)->
    Restricted = file_handler:get_restricted(Module, Modules),
    case lists:member(Function,Restricted) of
	true ->
	  %%   io:format(user, "the Is_Restricted is TRUE~p~n",[true]),
	    {error,"The Function is restricted !!!"};
	false ->
	%%    io:format(user, "the Is_Restricted is FALSE~p~n",[false]),

	    {ok,"The Function is un-restricted !!!"}		
    end.

run_fun(Module,Function,Data)->
    case catch apply(list_to_atom(Module), Function, parse_args(Data)) of	
    	{'EXIT',Error} ->
    	    {error,Error};
    	Res->
    	    Res		
    end.



parse(Data)->
   %% io:format(user,"Data ~p ~n", [Data]),
    [ _, Module, Function | Args] = Data,
    io:format(user,"Data is .......> ~p ~n", [Data]),
    Args_number = count(Args,0),
    Function_atom =  list_to_atom(Function),
    io:format(user,"Parse ~p ~p ~p ~n", [Module,Function_atom,Args_number]),
    {Module,{Function_atom,Args_number}}.

parse_args(Data)->
    [ _, _Module, _Function | Args] = Data,
    [to_expression(Arg)||Arg<- Args ].

terminate(Sock)->
    gen_tcp:close(Sock).

send(Sock,Result)->
    %%  Formatted_data = io_lib:format("~p~n",[Result]),
    gen_tcp:send(Sock, Result).

count([],Counter)->
    Counter;
count([_H|T],Counter)->
    count(T,Counter+1).

to_expression(String) ->
    {ok, Tokens,_ } = erl_scan:string(String ++ "."),
    {ok, Abstract} = erl_parse:parse_exprs(Tokens),
    {value, Expression, _} = erl_eval:exprs(Abstract, erl_eval:new_bindings()),
    Expression.

%% to_erlang_string(String) ->
%%     "[" ++
%%     [begin
%% 	 case C of
%% 	     $\s -> $,;
%% 	     $"->
%% 		 check_for_double_quotes_ending()
%% 	     _ -> C
%% 	 end
%%      end || C <- String] ++ "]".

%% double([]) ->
%%     [];
%% double([H|T]) ->
%%     [H*2 | double(T)].



%% separate_quoted_non_quoted([H|T])->
%%     case H of
%% 	$" 

%% check_string_is_proper([H|T],) ->
%%     case is_special(H) of
%% 	true ->
%% 	    Ending = get_ending_character(H),
%% 	    case does_special_char_ends(Ending,T) of
%% 		true->
%% 		    ;
%% 		false ->
%% 	    end;
%% 	false ->
%% 	    check_string_is_proper([H|Heading],Quoted,Trailing,T)
%%     end.

is_special(Char)->
    lists:member(Char, "\"{['").

get_ending_character(H) ->
    proplists:get_value(H,?Complements).

does_special_char_ends(Char,[])-> 
    false;

does_special_char_ends(Char,[H|T])-> 
    case H of
	Char ->
	    true;
	_ ->
	    does_special_char_ends(Char,T)
    end.

get_block_and_remainder([], _, _) ->
    {error, "Not a valid block"};
get_block_and_remainder([C|T], C, Acc) ->
    {ok, {Acc, T}};
get_block_and_remainder([H|T], C, Acc) ->
    get_block_and_remainder(T, C, [H|Acc]).

read_data(Text) ->
    read_data([], [], [], Text).

read_data(Heading,Word_Block,Trailing,[])->
    {lists:reverse(Heading),lists:reverse(Word_Block),lists:reverse(Trailing)},
    lists:reverse(Heading);

read_data(Heading,Word_Block,Trailing,[H|T])->
    case is_special(H) of
	true ->
	    Ending = get_ending_character(H),
	    io:format(user,"H is ~p ~n",[H]),
	    case get_block_and_remainder(T,Ending,[]) of
		{ok,{Within_special_chars,After_special_char}}->
		    Wrapped = wrap(Within_special_chars,H,Ending),
		    Joint_string = Wrapped++Heading,
		    read_data(Joint_string,Wrapped,After_special_char,After_special_char);
		   %% [Joint_string,Wrapped,After_special_char,H,Ending];
		Error ->
		    Error
	    end;
	false ->
	    case H of
		$\s -> 
		    read_data([$,|Heading],Word_Block,Trailing,T);
		_ ->
		    io:format(user,"Heading ~p Word_Block ~p trailing ~p ~n",[(Heading),(Word_Block),(Trailing)]),
		    read_data([H|Heading],Word_Block,Trailing,T)
	    end
    end.


wrap(S,Start,End) ->
    [End|S] ++ [Start].


%% 	    check_string_is_proper([H|Heading],Word_Block,Trailing,T)
%% end.



%% case H of
%% 	$" ->
%% 	    case check_for_double_quotes_ending([],T) of
%% 		{ok,{Quoted_word,Trailing}}->
%% 		    io:format(user,"Heading ~p Quoted ~p trailing ~p ~n",[lists:reverse(Heading),lists:reverse(Quoted),lists:reverse(Trailing)]),
    %% 		    read_data(Quoted_word++Heading,Quoted_word,Trailing,Trailing);
%% 		{error,Error} ->
%% 		    Error
%% 	    end;
%% 	$\s -> 
%% 	    read_data([$,|Heading],Quoted,Trailing,T);

%% 	_ ->
%% 	    io:format(user,"Heading ~p Quoted ~p trailing ~p ~n",[lists:reverse(Heading),lists:reverse(Quoted),lists:reverse(Trailing)]),
%% 	    read_data([H|Heading],Quoted,Trailing,T)
%% end.


check_for_double_quotes_ending(_,[])->
    {error, "There is no ending quote"};

check_for_double_quotes_ending(Acc,[H|T])->
    case H of
	$" ->
	    {ok,{Acc,T}};
	_->
	    check_for_double_quotes_ending([H|Acc],T)
    end.



%% separate_string([H|T],outside,Counter)->
%%     case special(H) of
%% 	true ->
%% 	  separate_string(T,inside,Counter);
%% 	false ->
%% 	   [H|separate_string(T,outside,Counter)]
%%     end.



%% separate_string([H|T], inside, Counter)->
%%     case special(H) of
%% 	true->
%% 	   separate_string(T,inside,Counter);
%% 	false ->
%% 	    [H|separate_string(T,inside,Counter)]
%%     end.




%% separate_string([H|T],inside,Counter)->
%% 			case special(H) of
%% 			    true ->
%% 				separate_string(T,inside,Counter2),
%% 				;
%% 			    false ->
%% 				[H|separate_string(T,inside,Counter)]
%% 			end
	
			    






































    %% 	$[ ->
    %% 	    {Rem_string, Acc} = check_for_square_braces_ending(T),
    %% 	    read_data(Rem_string,Acc);
    %% 	${ ->
    %% 	    {Rem_string, Acc} = check_for_curly_braces_ending(T),
    %% 	    read_data(Rem_string,Acc);
		    
    %% 	$' ->
    %% 	    {Rem_string, Acc} = check_for_single_quotes_ending(T),
    %% 	    read_data(Rem_string,Acc);
	    
    %% 	$\" ->	  
    %% 	    {Rem_string, Acc} = check_for_slash_ending(T),
    %% 	    read_data(Rem_string,Acc);
	   
    %% 	$\' ->
    %% 	    {Rem_string, Acc} = check_for_single_quotes_ending(T),
    %% 	    read_data(Rem_string,Acc);
    %% 	 _ ->
    %% 	    ignore
    %% end.

%% check_for_double_quotes_ending(T,Acc)->
%%     [Ele+Acc||Ele <- T],

%% check_for_square_braces_ending(Word,Acc)->
%%     [case Letter of
%% 	 ] ->
%% 						;
%% 	_->
%% 						New_Acc = Letter++Acc

%%  || Letter<- Word]
%%     .

%% %% Keys =  proplists:get_keys(List),	    
%% 	    %% Rec_List =  [proplists:get_value(Key,List)||Key <-Keys],
%% 	    %% Md5s = [Rec#module.module_md5||Rec <- Rec_List],
%% 	    %% Compile_times = [Rec#module.compile_time||Rec <- Rec_List],
%% 	    %% lists:merge3(Keys,Md5s,Compile_times)
