-module(file_handler).
-export([start/0,
	 loop/3,
	 create_record/3,
	 get_files/0,
	 get_module_name/1,
	 get_md5/1,
	 createRecordForNewFiles/3,
	 compile_and_load_file_from_dir/2,
	 get_restricted/2,
	 restrict/2,
	 unrestrict/2,
	 delete_module_keyval/1,
	 delete_module_keyval/2,
	 add_dir_to_path/1,
	 fetch/0,
	 update_changed_files/3,
	 is_changed/3,
	 beam_check/3,
	 separate_beam_erl_files/3]).

-include("record_definition.hrl").


start()->
    Ref = make_ref(),
    Parent = self(),
   % io:format("Starting file handler main function"),
    spawn(fun() ->		  
		  true = register(?MODULE,self()),
		  Parent ! {ok, Ref},
		  add_dir_to_path("/home/ekousha/codeserver/apps/codeserver/loaded/"),
		  loop([],[],1)
		  
	  end),
    receive
	{ok,Ref} ->
	    ok
    after 1000 ->
	    {error, "Couldn't start main"}
    end.

loop(Old_erl_files, Records_list,Counter)->
    Contents = io_lib:format("Old erl files: ~p~n"
			     "Record list: ~p~n",
			     [Old_erl_files, Records_list]),
    file:write_file("/tmp/records", Contents),
    
    New_erl_files_path = get_files(),
    Deleted_files = get_deleted_files(Old_erl_files,New_erl_files_path,[]),
    Records_list_deleted_modules_removed =  remove_modules_of_deleted_files(Deleted_files,Records_list),

    {Erl_files,Beam_files} = separate_beam_erl_files(New_erl_files_path,[],[]),

    Updated_erl_files = update(Old_erl_files, Erl_files, Records_list_deleted_modules_removed),

    Eligible_beam_files = beam_check(Beam_files,Updated_erl_files,[]),

    Updated_beam_files = update(Old_erl_files, Eligible_beam_files,Updated_erl_files ),

    Updated_Records_list_with_restricts = admin_msg(Updated_beam_files),
   loop(New_erl_files_path, Updated_Records_list_with_restricts,Counter+1).
%%    Updated_Records_list_with_restricts.

get_deleted_files([],_,Acc)->
    Acc;

get_deleted_files([H|T],New_erl_files_path,Acc)->
    case lists:member(H,New_erl_files_path) of
	true ->
	    get_deleted_files(T,New_erl_files_path,Acc);
	false ->
	    get_deleted_files(T,New_erl_files_path,[H|Acc])
    end.

remove_modules_of_deleted_files([],Records_list)->
    Records_list;
remove_modules_of_deleted_files([H|T],Records_list)->
    Mod_name = get_module_name(H),
    case filename:extension(H)of
	".erl"->
	    Updated_Records_list = delete_module_keyval(Mod_name,Records_list),
	    Files_related_to_deleted_file = Mod_name++"*",
	    [file:delete(File)||File<-Files_related_to_deleted_file];   

	".beam"->
	    Updated_Records_list = delete_module_keyval(Mod_name,Records_list)
   end,

	    remove_modules_of_deleted_files(T,Updated_Records_list).


separate_beam_erl_files([],Acc_erl,Acc_beam)->
    {Acc_erl,Acc_beam};
separate_beam_erl_files([H|T],Acc_erl,Acc_beam)->
    Filetype = filename:extension(H),
    case Filetype of
	".erl"->
	    separate_beam_erl_files(T,[H|Acc_erl],Acc_beam);
	".beam"->
	    separate_beam_erl_files(T,Acc_erl,[H|Acc_beam])
		
    end.


beam_check([],_,Acc)->
    Acc;
beam_check([H|T],Records_list,Acc)->
    Mod_name = get_module_name(H),
    case proplists:get_value(Mod_name,Records_list) of
	undefined ->
	    beam_check(T,Records_list,[H|Acc]);
	_ ->
	    beam_check(T,Records_list,Acc)
    end.

add_dir_to_path(Dir)->
    code:add_patha(Dir).

get_files() ->
    filelib:wildcard("/home/ekousha/codeserver/apps/codeserver/loaded/*.{erl,beam}").

update(Old_erl_files, New_erl_files_path,Records_list) ->
    Key_val_new_files = createRecordForNewFiles(Old_erl_files, New_erl_files_path,Records_list),
    update_changed_files(Old_erl_files,New_erl_files_path,Key_val_new_files).
    
createRecordForNewFiles( _,[],Old_keyval)->
    Old_keyval;

createRecordForNewFiles(Old_erlbeam_files,[H|T], Old_keyval)->
   case  lists:member(H,Old_erlbeam_files) of
       true ->
	  %% io:format("File was a member~n"),
	   createRecordForNewFiles(Old_erlbeam_files,T,Old_keyval);
       false ->
	  %% io:format("creating new record for ~p~n",[H]),
	   Mod_name = get_module_name(H),
	   Mod_md5 = get_md5(H),
	   Mod_name_atom = list_to_atom(Mod_name),
	   compile_and_load_file_from_dir(Mod_name_atom,H),
	   Filetype = filename:extension(H),
	   New_Rec = create_record(Mod_name, Mod_md5,Filetype),
	   New_keyval = {Mod_name, New_Rec},
	   Old_keyval_updated = [New_keyval | Old_keyval],
	 %%  io:format("creating record for the above mentioned new file ~p~n",[Old_keyval_updated]),
	   createRecordForNewFiles(Old_erlbeam_files, T,Old_keyval_updated)
       end.

get_module_name(File)->
    filename:rootname(filename:basename(File)).

get_md5(Filepath)->
    {ok,Bin} =file:read_file(Filepath),
    Md5_bin = crypto:hash(md5,Bin),
    Md5_hex =hexer:bin_to_hex(Md5_bin),
    erlang:binary_to_list(Md5_hex).

compile_and_load_file_from_dir(Mod_name_atom,Path)->
    code:purge(Mod_name_atom),
    code:delete(Mod_name_atom),    
    code:purge(Mod_name_atom),
    case filename:extension(Path) of
	".erl" ->	
	    compile:file(Path, [{outdir,"/home/ekousha/codeserver/apps/codeserver/loaded/"}]);
	".beam"->
	    do_nothing
    end,
  %%  io:format(user,"loading file ~p",[Path]),
   case code:load_file(Mod_name_atom) of
       {error, Error}->
	   io:format(user,"error file llloaded ~p~n",[Error]),
	   {error, Error};
       Res ->
	   Res
   end.
    

create_record(Mod_name,Mod_md5, Filetype) -> 
    Mod_name_atom = list_to_atom(Mod_name),
    Mod_info = Mod_name_atom:module_info(),
    Exported = proplists:get_value(exports, Mod_info),
    Compile_time = proplists:get_value(time, Mod_name_atom:module_info(compile)),
    Result = #module{filetype = Filetype,
		     exported =Exported,
		     module_md5 =Mod_md5,
		     compile_time = Compile_time},
%%    io:format(user,"Created Record is ----->>> ~p ~n",[Result]),
    Result.

update_changed_files(_,[],Records_list)->
    Records_list;

update_changed_files(Old_erl_files,[H|T],Old_keyval)->
    Mod_name = get_module_name(H),
    Mod_md5 = get_md5(H),
    Mod_name_atom = list_to_atom(Mod_name),
    Filetype = filename:extension(H),
    case is_changed(Mod_name,Mod_md5,Old_keyval)of
	false ->
	  %%%  io:format("file is not changed ~n"),
	    update_changed_files(Old_erl_files,T,Old_keyval);	
	true ->
	 %%   io:format("file is Changed ~n"),
	    compile_and_load_file_from_dir(Mod_name_atom,H),
	    New_Rec = create_record(Mod_name, Mod_md5,Filetype),
	    New_keyval = {Mod_name, New_Rec},
	    Old_keyval_updated = lists:keyreplace(Mod_name,1,Old_keyval,New_keyval),
	    update_changed_files(Old_erl_files,T,Old_keyval_updated);
	error ->
	   %% io:format("~p~n",["new_file"]),
	    update_changed_files(Old_erl_files,T,Old_keyval)
    end.

is_changed(Mod_name,New_md5,Records_list)->
    Value = proplists:get_value(Mod_name,Records_list), 
    case Value of 
	undefined ->
	    error;
	_ ->
	    Curr_md5 = Value#module.module_md5,
	    case Curr_md5 of
		New_md5->
		    false;
		_ ->
		  %%  io:format(user,"Changed file is ~p Curr_md5 ~p New_md5 ~p ~n",[Mod_name , Curr_md5, New_md5]),
		    true
	    end 
    end.

admin_msg(Modules)->
    receive 
	{From, Ref, restrict, Module, Function} ->
	    case module_is_loaded(Module, Modules) of
		true ->
		    restrict(From, Ref, Function, Module, Modules);
		false ->
		    From ! {Ref, {error, "the module does not exist"}},
		    Modules
	    end;

	{From,Ref,unrestrict,Module, Function} ->
	    case module_is_loaded(Module, Modules) of
		true ->
		    unrestrict(From, Ref, Function, Module, Modules);
		false ->
		    From ! {Ref, {error, "the module exists but the function does not exist"}},
		    Modules
	    end;

	{From,Ref,delete_module,Module} ->
	    Res = delete_module_keyval(Module, Modules),
	    From ! {Ref, ok},
	    Res;
	{From,Ref,fetch} ->
	    From ! {Ref, Modules},
	    Modules	    
    after 0 -> 
	    Modules
    end.

fetch()->
    Ref = make_ref(),
    ?MODULE ! {self(), Ref, fetch},
    receive
	{Ref, List} ->
	    {ok,List}
    after 500 ->
	    {error, no_response}
    end.

restrict(Module, Function) ->
    Ref = make_ref(),
    ?MODULE ! {self(), Ref, restrict, Module, Function},
    receive
	{Ref, Result} ->
	    Result
    after 500 ->
	    {error, no_response}
    end.

unrestrict(Module, Function) ->
    ?MODULE ! {self(), make_ref(), unrestrict, Module, Function},
    receive
	{_Ref, Result} ->
	    Result
    after 500 ->
	    {error, no_response}
    end.

delete_module_keyval(Module)->
    ?MODULE ! {self(),make_ref(),delete_module,Module},
    receive
	{_Ref, Result} ->
	    Result
    after 500 ->
	    {error, no_response}
    end.

module_is_loaded(ModuleName, Modules)->
    lists:keymember(ModuleName, 1, Modules).

is_exported(Function, Module) ->
    Exported = Module#module.exported,
    lists:member(Function, Exported).

restrict(From, Ref, Function, ModuleName, Modules) ->
    
    case is_exported(Function, proplists:get_value(ModuleName, Modules)) of
	true ->
	    Updated = add_restricted_function(Function, ModuleName, Modules),
	    From ! {Ref, {ok, "the function in module was restricted"}},
	    Updated;
	false ->
	    From ! {Ref, {error, "the module exists but the function does not exist"}},
	    Modules
    end.

unrestrict(From, Ref, Function, ModuleName, Modules) ->
    case is_exported(Function, proplists:get_value(ModuleName, Modules)) of
	true ->
	    Updated = remove_restricted_function(Function, ModuleName, Modules),
	    From ! {Ref, {ok, "the function in module was unrestricted"}},
	    Updated;
	false ->
	    From ! {Ref, {error, "the module exists but the function does not exist"}},
	    Modules
    end.

get_restricted(ModuleName, Modules) ->
    Module = proplists:get_value(ModuleName, Modules),
    %%io:format(user,"Inside get_restricted Module ~p ~n", [Module]),
    Restricted =  Module#module.restricted,
   %% io:format(user,"restricted inside is_restricted file handler.................>>>>>>>>>> ~p ~n", [Restricted]),
    Restricted.

add_restricted_function(Function, ModuleName, Modules) ->
    Restricted = get_restricted(ModuleName, Modules),
    NewRestricted = lists:usort([Function | Restricted]),
    change_restricted_if_exported(Function, ModuleName, Modules, NewRestricted).

remove_restricted_function(Function, ModuleName, Modules) ->
    Restricted = get_restricted(ModuleName, Modules),
    NewRestricted = Restricted -- [Function],
    change_restricted_if_exported(Function, ModuleName, Modules, NewRestricted).

change_restricted_if_exported(Function, ModuleName, Modules, NewRestricted) ->
    Module = proplists:get_value(ModuleName, Modules),
    NewModule = case is_exported(Function, Module) of
		    true ->
			Module#module{restricted = NewRestricted};
		    false ->
			Module
		end,
    lists:keyreplace(ModuleName, 1, Modules, {ModuleName, NewModule}).

delete_module_keyval(Module,Updated)->
    proplists:delete(Module,Updated).

