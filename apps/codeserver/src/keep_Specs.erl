-module(keep_Specs).
-export([start/0, add/2,delete/2,fetch/3]).
%% -define(ModuleName_r_List= []).
%% -define(FunctionName_r_List= []).
%% -define(ModuleName_ur_List= []).
%% -define(FunctionName_ur_List= []).


start()->
    Ref = make_ref(),
    Parent = self(),
    spawn(fun() ->		  
		  register(?MODULE, self()),
		  Parent ! {Ref, started},
		  loop([])
	  end),
    receive
	{Ref, started} ->
	    ok
    after 1000 ->
	    {error, "Couldn't start keep_Specs"}
    end.
	   


loop(Old_List)->
    
    receive 
	{From, Ref, add,Module,Function} ->
	    From ! {Ref, {ok,added}},
	    loop([{Module,Function}|Old_List]);	
	
	{From,Ref,delete,Module, Function} ->
	    New_List = delete_from_list(From,Ref,Module, Function,Old_List),
	    loop(New_List);
	
	{From,Ref,fetch} ->
	    fetch(From,Ref,Old_List),
	    loop( Old_List);
	terminate ->
	    ok
    end.


add(Module, Function) ->
    Ref = make_ref(),
    ?MODULE ! {self(), Ref, add, Module, Function},
    receive 
	{Ref, Res} ->
	    Res
    after 500 ->
	    {error, timeout}
    end.

delete(Module, Function) ->
    Ref = make_ref(),
    ?MODULE ! {self(), Ref, delete, Module, Function},
    receive 
	{Ref, Res} ->
	    Res
    after 500 ->
	    {error, timeout}
    end.

delete_from_list(From,Ref,Module,Function,Old_List)->
    case lists:member({Module,Function},Old_List) of
		true ->
	    From ! {Ref,{ok,deleted}},
	    lists:delete({Module,Function},Old_List);
	false ->
	    From ! {Ref,{error,not_found}},
	    Old_List
    end.

fetch(From,Ref,Old_List)->
    From !{Ref,{ok,Old_List}}.



%% loop()->
    
%%     update_database(),    
    
%%     loop().


%% update_database(Mod_name, Func_name) ->
%%     receive
%%         {From,add } ->
%%             From ! {self(), ok};
           
%%         {From,delete } ->
%%             case lists:member(List) of
%%                 true ->
%%                     From ! {self(), {ok }},
%%                     f(lists:delete(List));
%%                 false ->
%%                     From ! {self(), not_found},
%%                     f(List)
%%             end;
%%         terminate ->
%%             ok
%%     end.




%% update_restrict_database(ModuleName,FunctionName)->
%%     ModuleName_r_List = [ModuleName|ModuleName_r_List],
%%     FunctionName_r_List1 = [FunctionName|FunctionName_r_List],
%%     {restrict,ModuleName_r_List,FunctionName_r_List}.    


%% update_un_restrict_database(ModuleName,FunctionName)->
%%     [ ModuleName | ModuleName_ur_List],
%%     [ FunctionName |FunctionName_ur_List],
    
%%     {un_restrict,ModuleName_ur_List,FunctionName_ur_List}.

%% update_restrict_database([H|New_Files],Old_Files)->
%%     case  lists:member(H,Old_Files) of
%% 	false ->
%% 	    Old_Files_appended = lists:append(H,Old_Files),
%% 	    createMemoryFunction(New_Files,Old_Files_appended);
	
%% 	_-> createMemoryFunction(New_Files,Old_Files)
%%     end;
%% update_restrict_database([],Old_Files) ->
%%     Old_Files.


