-module(file_handler).
-export([start/0,
	 loop/3,
	 create_record/3,
	 get_files/2,
	 get_module_name/2,
	 get_module_info/1,
	 get_md5/1,
	 createRecordForNewFiles/4,
	 load_file_in_dir/1,
	 add_dir_to_path/1]).
-record(module, {filetype,
		 restricted = [],
		 exported,
		 module_md5 }).


start()->
    Ref = make_ref(),
    Parent = self(),
    io:format("Starting file handler main function"),
    spawn(fun() ->		  
		  register(?MODULE,self()),
		  Parent ! {ok, Ref},
		  loop([],[],[])
		  
	  end),
    receive
	{ok,Ref} ->
	    ok
    after 1000 ->
	    {error, "Couldn't start main"}
    end.


loop(Records_list,Old_erl_files,Old_beam_files)->
  
    New_erl_files_path = get_files("/home/ekousha/codeserver/apps/codeserver/loaded","erl"),
    New_beam_files_path = get_files("/home/ekousha/codeserver/apps/codeserver/loaded","beam"),
    Updated = update(Old_erl_files,Old_beam_files, New_erl_files_path, New_beam_files_path,Records_list),
    io:format("~p~n",["enter admin_msg"]),
    Updated_rec_with_restricts = admin_msg(Updated),
    loop(Updated_rec_with_restricts, New_erl_files_path, New_beam_files_path).
 
update(Old_erl_files, Old_beam_files, New_erl_files_path, New_beam_files_path,Records_list) ->
    With_erl = createRecordForNewFiles(Old_erl_files, "erl", New_erl_files_path,Records_list),
    createRecordForNewFiles(Old_beam_files, "beam", New_beam_files_path, With_erl).


createRecordForNewFiles(Old_erlbeam_files, Filetype, [H|T], Old_keyval)->
    case lists:member(H,Old_erlbeam_files) of
	false ->
	    Mod_name = get_module_name(H, Filetype),
	    Mod_md5 = get_md5(H),
	    add_dir_to_path("/home/ekousha/codeserver/apps/codeserver/loaded/"),
	    load_file_in_dir(Mod_name),
	    New_Rec = create_record(Mod_name, Filetype, Mod_md5),
	    New_keyval = [{Mod_name, New_Rec}],
	    Old_keyval_updated = lists:append(New_keyval,Old_keyval),
	    createRecordForNewFiles(Old_erlbeam_files, Filetype, T, Old_keyval_updated);
	true ->
	    createRecordForNewFiles(Old_erlbeam_files, Filetype, T,Old_keyval)
    end;
createRecordForNewFiles( _,_,[],Old_keyval)->
    Old_keyval.

add_dir_to_path(Dir)->
    code:add_path(Dir).

load_file_in_dir(Mod_name)->
    Mod_name_atom = list_to_atom(Mod_name),
    %%io:format("~p~n",[Mod_name_atom]),
    %% ok = case code:purge(Mod_name_atom) of
    %% 	     true ->
    %% 		 ok;
    %% 	     _ ->
    %% 		 {error,not_purrged}
    %% 	 end,
    %% code:purge(Mod_name_atom),
    %% ok = case code:delete(Mod_name_atom) of
    %% 	     true ->
    %% 		 ok;
    %% 	     _ ->
    %% 		 {error,not_delleted}
    %% 	 end,
    code:purge(Mod_name_atom),
    code:delete(Mod_name_atom),
    case compile:file(Mod_name_atom) of
	{ok,_}->
	    ok;
	Error ->
	    Error
    end,
    code:load_file(Mod_name_atom).


create_record(Mod_name, Filetype, Mod_md5) ->   
    Mod_info = get_module_info(Mod_name),
    Exported = proplists:get_value(exports, Mod_info),
    #module{filetype = Filetype,
	    exported =Exported,
	    module_md5 =Mod_md5}. 

admin_msg(Updated)->
    receive 
	{From, Ref, restrict, Module, Function} ->
	    io:format("~p~n",["You have come to restricting file-------)))"]),
	    restrict(From, Ref,Module, Function, Updated);
	{From,Ref,unrestrict,Module, Function} ->
	    unrestrict(From, Ref,Module, Function, Updated);
	{From,Ref,delete_module,Module} ->
	    delete_module(From, Ref, Module, Updated)	
    after 500 -> 
	    Updated
    end.

restrict(From, Ref, Module, Function,Updated)->
    Rec_of_Module = proplists:get_value(Module,Updated),
    Specs = Rec_of_Module#module.restricted,
    Updated_Specs = [Function | Specs],
    From ! {Ref, {ok, restricted}},
    Rec_of_Module_restricts = Rec_of_Module#module{restricted = Updated_Specs},
    Keyval = [{Module, Rec_of_Module_restricts}],
    lists:append(Keyval,Updated).


unrestrict(From, Ref, Module, Function,Updated)->
    Rec_of_Module = proplists:get_value(Module,Updated),
    Specs = Rec_of_Module#module.restricted,
    Updated_Specs = lists:subtract(Specs,[Function]),
    From ! {Ref, {ok, unrestricted}},
    Rec_of_Module_restricts = Rec_of_Module#module{restricted = Updated_Specs},
    Keyval = {Module, Rec_of_Module_restricts},
    lists:keyreplace(Module,1, Updated, Keyval).

delete_module(From, Ref, Module,Updated)->
    From ! {Ref, {ok, deleted_module}},    
    proplists:delete(Module,Updated).


get_files(Directory,Filetype) ->
    Path_modified_for_files = filename:join(Directory,"*."++Filetype),
    filelib:wildcard(Path_modified_for_files).

get_module_name(File,Filetype)->
    filename:basename(File,"."++Filetype).


get_module_info(One_file)->
    Module = list_to_atom(One_file), 
    Module:module_info().
    % Md5_bin = Mod_name_atom:module_info(md5),

get_md5(Filepath)->
    {ok,Bin} =file:read_file(Filepath),
    Md5_bin = crypto:hash(md5,Bin),
    Md5_hex =hexer:bin_to_hex(Md5_bin),
    erlang:binary_to_list(Md5_hex).
