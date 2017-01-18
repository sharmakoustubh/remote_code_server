-module(file_handler).
-export([start/0,loop/3,create_record/2,get_files/2,get_module_name/2,get_module_info/1,get_md5/1]).
-record(module, {module_name,
			     filetype,
			     specs,
 			     module_info,
 			     module_md5 }).


start()->
    Ref = make_ref(),
    Parent = self(),
    io:format("Starting file handler main function"),
    spawn(fun() ->		  
		  register(?MODULE,self()),
		  Parent ! {Ref, started},
		  loop([],[],[])
	  end),
    receive
	{Ref, started} ->
	    ok
    after 1000 ->
	    {error, "Couldn't start main"}
    end.


loop(Records_list,Old_erl_files,Old_beam_files)->
    %%  New_Restrict_List = admin_msg(Restrict_list),
    New_erl_files = get_files("/home/ekousha/codeserver/apps/codeserver/loaded","erl"),
    io:format("~p~n",[New_erl_files]),
    New_beam_files = get_files("/home/ekousha/codeserver/apps/codeserver/loaded","beam"),
    io:format("~p~n",[New_beam_files]),
    Updated = update(Old_erl_files,Old_beam_files, New_erl_files, New_beam_files,Records_list),
    Updated_rec_with_restricts = admin_msg(Updated),
   
    loop(Updated_rec_with_restricts, New_erl_files, New_beam_files).

update(Old_erl_files,Old_beam_files, New_erl_files, New_beam_files,Records_list) ->
    With_erl = createRecordForNewFiles(Old_erl_files, "erl", New_erl_files,Records_list),
    createRecordForNewFiles(Old_beam_files, "beam", New_beam_files, With_erl).

createRecordForNewFiles(Old_erlbeam_files,FileType,[H|New_file],Acc)->
    case  lists:member(H,Old_erlbeam_files) of
	false ->
	    New_Rec =  create_record(H,FileType),
	    New_Acc = [New_Rec|Acc],
	    createRecordForNewFiles(Old_erlbeam_files,FileType,New_file,New_Acc);
	
	true ->  createRecordForNewFiles(Old_erlbeam_files,FileType,New_file,Acc)
		     
    end;

createRecordForNewFiles(File_list,FileType,[],Acc)->
    Acc.

create_record(File_path,Filetype) ->
    Mod_name = get_module_name(File_path,Filetype), 
    Mod_name_atom = list_to_atom(Mod_name),
    load_file(Mod_name_atom),
    Mod_info = get_module_info(Mod_name),
    Exported = proplists:get_value(exported, Mod_info),
    Mod_md5 = get_md5(File_path),
    put_in_record(Mod_name,Filetype,Exported,Mod_md5).

put_in_record(Mod_name,Filetype,Exported,Mod_md5)->
    Rec = #module{module_name = Mod_name,filetype = Filetype,module_info =Exported,module_md5 =Mod_md5},
    {Mod_name,Rec}. 


    
admin_msg(Updated)->
    receive 
	{From, Ref, restrict,Module,Function} ->
	    From ! {Ref, {ok,restricted}},
	    lists:member(Module,Updated);
	 %%   Updated#module.specs = {restricted,{Module,Function}};	
	{From,Ref,unrestrict,Module, Function} ->
%%	    Updated#module
	    delete_from_list(From,Ref,Module, Function,[])	
    after 500 -> 
	    Updated
		
    end.


restrict(Module, Function) ->
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

do_fetch(From,Ref,Old_List)->
    From !{Ref,{ok,Old_List}}.


fetch() ->
    Ref = make_ref(),
    ?MODULE ! {self(),Ref,fetch},
    receive 
	{Ref, Res} ->
	    Res
    after 500 ->
	    {error, timeout}
    end.

terminate_process()->
    Ref=make_ref(),
    ?MODULE ! {self(),Ref,terminate},
    receive 
	{Ref,terminated}->
	    timer:sleep(100),
	    ok
    after 500 ->
	    {error,timeout}
    end.

load_file(Mod_name_atom)->
    code:load_file(Mod_name_atom).

get_files(Directory,Filetype) ->
    Path_modified_for_files = filename:join(Directory,"*."++Filetype),
    Files_in_directory = filelib:wildcard(Path_modified_for_files).

get_module_name(File,Filetype)->
    filename:basename(File,"."++Filetype).
						%    [ filename:basename(File,"."++Filetype) || File <- Files ].

get_module_info(One_file)->
    Module = list_to_atom(One_file),
    Module:module_info().

%% Atom_module_names = [list_to_atom(One_file)||One_file <- All_files_basename],
%% [Module:module_info() ||Module <- Atom_module_names].

get_md5(Filepath)->
    {ok,Bin} =file:read_file(Filepath),
    Md5_bin = crypto:hash(md5,Bin),
						%  io:format("~p~n",[Md5_bin]),
    Md5_hex =hexer:bin_to_hex(Md5_bin),
						%  io:format("~p~n",[Md5_hex]),
    Md5_list =erlang:binary_to_list(Md5_hex).
						%  io:format("~p~n",[Md5_list]).

%% get_specs(Mod_name,Mod_info)->
%%    get_info_from_admin(Mod_name,FunctionName,Specification).
    
    
%% get_info_from_admin()->
%%     admin_listener:function_specs(),
    
    


createMemoryFunction([H|New_Files],Old_Files)->
    case  lists:member(H,Old_Files) of
	false ->
	    Old_Files_appended = lists:append(H,Old_Files),
	    createMemoryFunction(New_Files,Old_Files_appended);
	
	_-> createMemoryFunction(New_Files,Old_Files)
    end;
createMemoryFunction([],Old_Files) ->
    Old_Files.







						%   Erl_files_ create_record(Erl_files,[]),

%% append_record(Record) ->
%%     Record|Records_list.    



%%   io:format("~p",Erl_files),
%%     Beam_files = get_files("/home/ekousha/Documents/Erlang_Projs/loaded","beam"),
%%     io:format("~p",Beam_files),
%%     Erl_module_names = get_module_name(Erl_files,erl),
%%     io:format("~p",Erl_module_names),
%%     Beam_module_names = get_module_name(Beam_files,beam),
%%     io:format("~p",Beam_module_names),
%%     Erl_module_info = get_module_info(Erl_module_names),
%%     io:format("~p",Erl_module_info),
%%     Beam_module_info = get_module_info(Beam_module_names).

%% [create_record(X)|| X <- Erl_files ]

%%  create_record(H|T,Acc)->
%%      Acc_h =   #module{directory_erl = H},
%%      create_record(T,Acc_h|Acc).

%%  create_record([],Acc)->
%%      Acc.



%% get_beam_files(Directory) ->
%%     Path_modified_for_erl_files = string:concat(Directory,"/*.beam"),
%%     filelib:wildcard(Path_modified_for_erl_files).

%% get_beam_files_module_names(Directory)->
%%     Path_modified_for_erl_files = string:concat(Dir ectory,"/*.beam"),
%%     [Files_in_directory] = filelib:wildcard(Path_modified_for_beam_files),
%%     Beam_file = filename:basename(Files_in_Directory,".beam"),
%%     One_file_at_a_time_atom_format =list_to_atom(Beam_file),
%%     modules:module_info(One_file_at_a_time_atom_format),
%%     ets_table1:module_info().

%% Input_to_moduleinfo = list_to_atom(Files_pFiles_in_directorym
						%resent_in_dir),
%% Mod_info = module_info(Input_to_moduleinfo),
%% #module{directory = Directory,
%% 			files=Input_to_moduleinfo,
%% 			details=[Mod_info]}.


%% #module{directory_erl = Erl_files ,directory_beam = Beam_files ,module_names_erl = Erl_module_names, module_names_beam = Beam_module_names,details_erl = Erl_module_info, details_beam = Beam_module_info}.



%% append_record(Record_list_head),
%% Records_list_head|Records_list,
			%   io:format("~p~n",[Record]),

