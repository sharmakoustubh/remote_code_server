-module(file_handler).
-export([start/1,
	 loop/3,
	 create_record/3,
	 get_files/1,
	 get_module_name/1,
	 get_md5/1,
	 create_records_for_new_files/4,
	 compile_and_load_file_from_dir/3,
	 get_restricted/2,
	 restrict/2,
	 unrestrict/2,
	 delete_module_keyval/1,
	 delete_module_keyval/2,
	 add_dir_to_path/1,
	 fetch/0,
	 update_changed_files/4,
	 is_changed/3,
	 beam_check/3,
	 separate_beam_erl_files/3]).

-include("record_definition.hrl").

start(User_defined_path)->
    register(?MODULE,self()),
    add_dir_to_path(User_defined_path),
    loop([], [], User_defined_path).


add_dir_to_path(Dir)->
    code:add_patha(Dir).

loop(Old_erl_files, Modules,User_defined_path)->
    Contents = io_lib:format("Old erl files: ~p~n"
			     "Record list: ~p~n",
			     [Old_erl_files, Modules]),
    file:write_file("/tmp/records", Contents),
    New_erl_files_path = get_files(User_defined_path),
    Deleted_files = get_deleted_files(Old_erl_files,New_erl_files_path,[]),
    Modules_deleted_modules_removed =  remove_modules_of_deleted_files(Deleted_files,Modules),
    {Erl_files,Beam_files} = separate_beam_erl_files(New_erl_files_path,[],[]),
    Updated_erl_files = update(Old_erl_files, Erl_files, Modules_deleted_modules_removed,User_defined_path),
    Eligible_beam_files = beam_check(Beam_files,Updated_erl_files,[]),
    Updated_beam_files = update(Old_erl_files, Eligible_beam_files,Updated_erl_files,User_defined_path),
    Updated_Modules_with_restricts = admin_msg(Updated_beam_files),
    loop(New_erl_files_path, Updated_Modules_with_restricts,User_defined_path).

%% get_files() ->
%%     filelib:wildcard("/home/ekousha/codeserver/apps/codeserver/loaded/*.{erl,beam}").

get_files(User_defined_path) ->
    filelib:wildcard(User_defined_path ++ "*.{erl,beam}").



get_deleted_files([],_,Acc)->
    Acc;

get_deleted_files([H|T],New_erl_files_path,Acc)->
    case lists:member(H,New_erl_files_path) of
	true ->
	    get_deleted_files(T,New_erl_files_path,Acc);
	false ->
	    get_deleted_files(T,New_erl_files_path,[H|Acc])
    end.

remove_modules_of_deleted_files([],Modules)->
    Modules;
remove_modules_of_deleted_files([H|T],Modules)->
    Mod_name = get_module_name(H),
    case filename:extension(H)of
	".erl"->
	    Updated_Modules = delete_module_keyval(Mod_name,Modules),
	    Files_related_to_deleted_file = Mod_name++"*",
	    [file:delete(File)||File<-Files_related_to_deleted_file];   
	".beam"->
	    Updated_Modules = delete_module_keyval(Mod_name,Modules)
    end,
    remove_modules_of_deleted_files(T,Updated_Modules).


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
beam_check([H|T],Modules,Acc)->
    Mod_name = get_module_name(H),
    case proplists:get_value(Mod_name,Modules) of
	undefined ->
	    beam_check(T,Modules,[H|Acc]);
	_ ->
	    beam_check(T,Modules,Acc)
    end.

update(Old_erl_files, New_erl_files_path,Modules,User_defined_path) ->
    Key_val_new_files = create_records_for_new_files(Old_erl_files, New_erl_files_path,Modules,User_defined_path),
    update_changed_files(Old_erl_files,New_erl_files_path,Key_val_new_files,User_defined_path).
    
create_records_for_new_files( _,[],New_Modules,_)->
    New_Modules;
create_records_for_new_files(Old_erlbeam_files,[H|T], Old_Modules,User_defined_path)->
   case is_old(H, Old_erlbeam_files) of
       true ->
	   create_records_for_new_files(Old_erlbeam_files,T,Old_Modules,User_defined_path);
       false ->
	   Mod_name = get_module_name(H),
	   Mod_name_atom = list_to_atom(Mod_name),
	   case compile_and_load_file_from_dir(Mod_name_atom,H,User_defined_path) of
	       {error,_} ->
		   create_records_for_new_files(Old_erlbeam_files,T,Old_Modules,User_defined_path);
	       _->
		   Mod_md5 = get_md5(H),
		   Filetype = filename:extension(H),
		   New_Rec = create_record(Mod_name, Mod_md5,Filetype),
		   New_Modules = {Mod_name, New_Rec},
		   New_key_val = [New_Modules | Old_Modules],
		   create_records_for_new_files(Old_erlbeam_files, T,New_key_val,User_defined_path)
	   end
   end.

is_old(File, Old_files) ->
    lists:member(File, Old_files).

update_changed_files(_,[],Modules,_)->
    Modules;

update_changed_files(Old_erl_files,[H|T],Old_Modules,User_defined_path)->
    Mod_name = get_module_name(H),
    case get_md5(H) of
	{error,_}->
	    update_changed_files(Old_erl_files,T,Old_Modules,User_defined_path);
	Mod_md5 ->
	    Mod_name_atom = list_to_atom(Mod_name),
	    Filetype = filename:extension(H),
	    case is_changed(Mod_name,Mod_md5,Old_Modules) of
		false ->
		   
		    update_changed_files(Old_erl_files,T,Old_Modules,User_defined_path);	
		true ->
		    case compile_and_load_file_from_dir(Mod_name_atom,H,User_defined_path) of
			{error,_} ->
			    update_changed_files(Old_erl_files,T,Old_Modules,User_defined_path);
			_->			  
			    New_Rec = create_record(Mod_name, Mod_md5,Filetype),
			    New_Modules = {Mod_name, New_Rec},
			    New_key_val = lists:keyreplace(Mod_name,1,Old_Modules,New_Modules),
			    update_changed_files(Old_erl_files,T,New_key_val,User_defined_path)
		    end
		
	    end
    end.

is_changed(Mod_name,New_md5,Modules)->
    Value = proplists:get_value(Mod_name,Modules), 
    case Value of 
	undefined ->
	    true;
	_ ->
	    Curr_md5 = Value#module.module_md5,
	    case Curr_md5 of
		New_md5->
		    false;
		_ ->
		    true
	    end 
    end.

get_module_name(File)->
    filename:rootname(filename:basename(File)).

get_md5(Filepath)->
    case file:read_file(Filepath) of
	{ok,Bin}->
	    Md5_bin = crypto:hash(md5,Bin),
	    Md5_hex =hexer:bin_to_hex(Md5_bin),
	    erlang:binary_to_list(Md5_hex);
	{error,Error}->
	    {error,Error}
    end.

compile_and_load_file_from_dir(Mod_name_atom,Path,User_defined_path)->
    code:purge(Mod_name_atom),
    code:delete(Mod_name_atom),    
    code:purge(Mod_name_atom),
    case filename:extension(Path) of
	".erl" ->	
	    case catch compile:file(Path, [{outdir,User_defined_path}]) of %%"/home/ekousha/codeserver/apps/codeserver/loaded/"
		{'EXIT',_} ->
		    do_nothing;
		_->
		    code:load_file(Mod_name_atom)		   
	    end;
	".beam"->
	    code:load_file(Mod_name_atom)
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
    Result.


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


add_restricted_function(Function, ModuleName, Modules) ->
    Restricted = get_restricted(ModuleName, Modules),
    NewRestricted = lists:usort([Function | Restricted]),
    change_restricted_if_exported(Function, ModuleName, Modules, NewRestricted).

get_restricted(ModuleName, Modules) ->
    Module = proplists:get_value(ModuleName, Modules),
    Restricted =  Module#module.restricted,
    Restricted.

change_restricted_if_exported(Function, ModuleName, Modules, NewRestricted) ->
    Module = proplists:get_value(ModuleName, Modules),
    NewModule = case is_exported(Function, Module) of
		    true ->
			Module#module{restricted = NewRestricted};
		    false ->
			Module
		end,
    lists:keyreplace(ModuleName, 1, Modules, {ModuleName, NewModule}).

remove_restricted_function(Function, ModuleName, Modules) ->
    Restricted = get_restricted(ModuleName, Modules),
    NewRestricted = Restricted -- [Function],
    change_restricted_if_exported(Function, ModuleName, Modules, NewRestricted).


delete_module_keyval(Module,Updated)->
    proplists:delete(Module,Updated).

