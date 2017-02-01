-module(file_handler).
-export([start/0,
	 loop/3,
	 create_record/3,
	 get_files/2,
	 get_module_name/2,
	 get_md5/1,
	 createRecordForNewFiles/4,
	 load_file_from_dir/2,
	 restrict/3,
	 unrestrict/3,
	 add_dir_to_path/1,
	 fetch/0,
	 list_md5/1]).

-include("record_definition.hrl").

start()->
    Ref = make_ref(),
    Parent = self(),
    io:format("Starting file handler main function"),
    spawn(fun() ->		  
		  register(?MODULE,self()),
		  Parent ! {ok, Ref},
		  add_dir_to_path("/home/ekousha/codeserver/apps/codeserver/loaded/"),
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
    Updated_rec_with_restricts = admin_msg(Updated),
    loop(Updated_rec_with_restricts, New_erl_files_path, New_beam_files_path).
 
update(Old_erl_files, Old_beam_files, New_erl_files_path, New_beam_files_path,Records_list) ->
    With_erl = createRecordForNewFiles(Old_erl_files, "erl", New_erl_files_path,Records_list),
    
    createRecordForNewFiles(Old_beam_files, "beam", New_beam_files_path, With_erl).

update_if_new(Records_list,Filetype,Mod_name,Mod_md5)->
    Record = proplists:get_val(Mod_name,Records_list),
    
    File_kind = case Record of 
		    undefined ->
			new_file;
		    _->
			old_file
		end,
	case File_kind of
	  new_file->
		new;
	    old_file ->
		Md5_record_list = Record#module.module_md5;
	    _->
		error
	end,
    Old_md5 =  Record#module.module_md5,
	case Mod_md5 of 
	    Old_md5 ->
		same_as_old;
	    
	    _ ->
		update
	end.




createRecordForNewFiles( _,_,[],Old_keyval)->
    Old_keyval;

createRecordForNewFiles(Old_erlbeam_files, Filetype, [H|T], Old_keyval)->
    Mod_name = get_module_name(H, Filetype),
    %% io:format(user,"file before md5 -------->>>~p~n",[H]),
    Mod_md5 = get_md5(H),
    Mod_name_atom = list_to_atom(Mod_name),
    Response_update_if_new = update_if_new(Old_keyval,Filetype,Mod_name,Mod_md5),
    case Response_update_if_new of
    	ok -> 
    	    createRecordForNewFiles(Old_erlbeam_files, Filetype, T,Old_keyval);
    	update->
	    
	    case Filetype of
		"erl"->
		    load_file_from_dir(Mod_name_atom,H),
		    New_Rec = create_record(Mod_name, Filetype, Mod_md5),
		    New_keyval = [{Mod_name, New_Rec}],
		    Old_keyval_updated = lists:append(New_keyval,Old_keyval),
		    createRecordForNewFiles(Old_erlbeam_files, Filetype, T,Old_keyval_updated);
		"beam"->
		    Old_keys = proplists:get_keys(Old_keyval),
		    case lists:member(Mod_name,Old_keys) of
			true->
			    createRecordForNewFiles(Old_erlbeam_files, Filetype, T,Old_keyval);
			false->
			    code:load_file(Mod_name_atom),
			    New_Rec = create_record(Mod_name, Filetype, Mod_md5),
			    New_keyval = [{Mod_name, New_Rec}],
			    Old_keyval_updated = lists:append(New_keyval,Old_keyval),
			    createRecordForNewFiles(Old_erlbeam_files, Filetype, T,Old_keyval_updated)
		    end
			
			
	    end
		
    end.

add_dir_to_path(Dir)->
    code:add_path(Dir).

load_file_from_dir(Mod_name_atom,Path)->
    code:delete(Mod_name_atom),    
    code:purge(Mod_name_atom),
    compile:file(Path, [{outdir,"/home/ekousha/codeserver/apps/codeserver/loaded/"}]),
    code:load_file(Mod_name_atom).


create_record(Mod_name, Filetype, Mod_md5) -> 
    Mod_name_atom = list_to_atom(Mod_name),
    %io:format(user, "Getting module info~n", []),
    Mod_info = Mod_name_atom:module_info(),
    %io:format(user, "Getting exported from module info~n", []),
    Exported = proplists:get_value(exports, Mod_info),
    Result = #module{filetype = Filetype,
		     exported =Exported,
		     module_md5 =Mod_md5},
    %io:format(user, "Returning this: ~p~n", [Result]),
    Result.

admin_msg(Updated)->
    receive 
	{From, Ref, restrict, Module, Function} ->
	    io:format("~p~n",["You have come to restricting file-------)))"]),
	    Result = restrict(Module, Function, Updated),
	    From!{Ref, ok},
	    Result;
	{From,Ref,unrestrict,Module, Function} ->
	    Res = unrestrict(Module, Function, Updated),
	    From ! {Ref, ok},
	    Res;
	{From,Ref,delete_module,Module} ->
	    delete_module(From, Ref, Module, Updated);
	{From,Ref,fetch} ->
	   From ! {Ref, Updated},
	    Updated	    
    after 0 -> 
	    Updated
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

restrict(Module, Function,Modules)->
    Rec = proplists:get_value(Module, Modules),
    Old_restricted = Rec#module.restricted,
    Restricted = [Function | Old_restricted],
    Updated = {Module, Rec#module{restricted = Restricted}},
    lists:keyreplace(Module, 1, Modules, Updated).


unrestrict(Module, Function, Modules) ->
    Rec = proplists:get_value(Module, Modules),
    Old_restricted = Rec#module.restricted,
    Restricted = Old_restricted -- [Function],
    Updated = {Module, Rec#module{restricted = Restricted}},
    lists:keyreplace(Module, 1, Modules, Updated).

delete_module(From, Ref, Module,Updated)->
    From ! {Ref, {ok, deleted_module}},    
    proplists:delete(Module,Updated).


get_files(Directory,Filetype) ->
    Path_modified_for_files = filename:join(Directory,"*."++Filetype),
    filelib:wildcard(Path_modified_for_files).

get_module_name(File,Filetype)->
    filename:basename(File,"."++Filetype).

list_md5(Files)->
    List_md5 = [get_md5(File)||File<-Files],
    io:format(user,"md5 in a list format of files in dir    ~p~n",[List_md5]),
    List_md5.
		      
get_md5(Filepath)->
    {ok,Bin} =file:read_file(Filepath),
    Md5_bin = crypto:hash(md5,Bin),
    Md5_hex =hexer:bin_to_hex(Md5_bin),
    erlang:binary_to_list(Md5_hex).
