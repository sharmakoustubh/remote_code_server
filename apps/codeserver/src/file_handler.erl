-module(file_handler).
-export([start/0,
	 loop/3,
	 create_record/3,
	 get_files/0,
	 get_module_name/1,
	 get_md5/1,
	 createRecordForNewFiles/3,
	 compile_and_load_file_from_dir/2,
	 restrict/3,
	 unrestrict/3,
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
    io:format("Starting file handler main function"),
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
    {Erl_files,Beam_files} = separate_beam_erl_files(New_erl_files_path,[],[]),

    Updated_erl_files = update(Old_erl_files, Erl_files, Records_list),
    case Counter =<3 of
	true-> 
	    io:format("Counter: ~p Updated_erl_files~p ----------->~n",[Counter,Updated_erl_files]);
	false ->
	    ok
    end, 
    Eligible_beam_files = beam_check(Beam_files,Updated_erl_files,[]),
   %% io:format("Eligible beam files~p~n",[Eligible_beam_files]),
    Updated_beam_files = update(Old_erl_files, Eligible_beam_files,Updated_erl_files ),
    case Counter =<3 of
	true-> 
	    io:format("Counter: ~p Updated_beam_files ~p ----------->~n",[Counter,Updated_beam_files]);
	false ->
	    ok
    end,
    Updated_Records_list_with_restricts = admin_msg(Updated_beam_files),
   loop(New_erl_files_path, Updated_Records_list_with_restricts,Counter+1).
%%    Updated_Records_list_with_restricts.

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
	   {ok,_}= compile:file(Path, [{outdir,"/home/ekousha/codeserver/apps/codeserver/loaded/"}]);
	".beam"->
	    do_nothing
    end,
    io:format(user,"loading file ~p",[Path]),
    code:load_file(Mod_name_atom).
    

create_record(Mod_name,Mod_md5, Filetype) -> 
    Mod_name_atom = list_to_atom(Mod_name),
    Mod_info = Mod_name_atom:module_info(),
    Exported = proplists:get_value(exports, Mod_info),
    Result = #module{filetype = Filetype,
		     exported =Exported,
		     module_md5 =Mod_md5},
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
	    io:format("file is Changed ~n"),
	    compile_and_load_file_from_dir(Mod_name_atom,H),
	    New_Rec = create_record(Mod_name, Mod_md5,Filetype),
	    New_keyval = {Mod_name, New_Rec},
	    Old_keyval_updated = lists:keyreplace(Mod_name,1,Old_keyval,New_keyval),
	    update_changed_files(Old_erl_files,T,Old_keyval_updated);
	error ->
	    io:format("~p~n",["new_file"]),
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
		    io:format(user,"Changed file is ~p Curr_md5 ~p New_md5 ~p ~n",[Mod_name , Curr_md5, New_md5]),
		    true
	    end 
    end.

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

