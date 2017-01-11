-module(file_handler).
-export([file_handler_main_function/3,create_record_now/2,get_files/2,get_module_names/2,get_module_info/1,get_md5/1]).
-record(file_poller_record, {module_name,filetype,specs,
 			     module_info,
 			     module_md5 }).

file_handler_main_function(Records_list,Old_erl_files,Old_beam_files)->
    Erl_files = get_files("/home/ekousha/codeserver/apps/codeserver/loaded","erl"),
    io:format("~p~n",[Erl_files]),
    Beam_files = get_files("/home/ekousha/codeserver/apps/codeserver/loaded","beam"),
    io:format("~p~n",[Beam_files]),
    Joint_erl_beam_list =lists:append(Erl_files,Beam_files),
    io:format("~p~n",[Joint_erl_beam_list]),
    
    %% Old_erl_files = createMemoryFunction(Erl_files,Old_erl_files),
    %% Old_beam_files = createMemoryFunction(Beam_files,Old_beam_files),
    
    New_Rec_list1 =  createRecordForNewFiles( Old_erl_files,"erl",Erl_files,Records_list),
    New_Rec_list2 =   createRecordForNewFiles(Old_beam_files,"beam",Beam_files,New_Rec_list1),
    io:format("~p~n",[New_Rec_list2]).
%%    file_handler_main_function(New_Rec_list2,Erl_files,Beam_files).

createMemoryFunction([H|New_Files],Old_Files)->
    case  lists:member(H,Old_Files) of
	false ->
	    Old_Files_appended = lists:append(H,Old_Files),
	    createMemoryFunction(New_Files,Old_Files_appended);
	
	_-> createMemoryFunction(New_Files,Old_Files)
    end;
createMemoryFunction([],Old_Files) ->
    Old_Files.


createRecordForNewFiles(File_list,FileType,[H|New_file],Acc)->
    case  lists:member(H,File_list) of
	false ->
	    New_Rec =  create_record_now(H,FileType),
	    New_Acc = [New_Rec|Acc],
	    createRecordForNewFiles(File_list,FileType,New_file,New_Acc);
	
	true ->  createRecordForNewFiles(File_list,FileType,New_file,Acc)
		     
    end;

createRecordForNewFiles(File_list,FileType,[],Acc)->
    Acc.




%% Records_erl = [create_record_now(File_erl,"erl")||File_erl<-Erl_files],
%% Records_beam = [create_record_now(File_Beam,"beam")||File_Beam<-Beam_files],
%% R1 = [Records_erl|Records_beam],
%% Updated_record = lists:append(Records_erl,Records_beam),
%% timer:sleep(100),
%% io:format("~p~n",[Updated_record]).
%% 						%    file_handler_main_function(Updated_record,Erl_files,Beam_files).


create_record_now(File_path,Filetype) ->
    Mod_name = get_module_names(File_path,Filetype), 
						%  io:format("~p~n",[Mod_name]),
    Mod_name_atom = list_to_atom(Mod_name),
 %% case  "erl" =Filetype of 
 %%     true ->	 
 %% 	 c:cd("../loaded"),
 %% 	 c(Mod_name_atom),
 %% 	 l(Mod_name_atom),
 %% 	 c:cd("../src");
 %%     false -> ok
 %% end,
    code:load_file(Mod_name_atom),
    io:format("~p~n",[Mod_name_atom]),
    Mod_info = get_module_info(Mod_name),
    io:format("~p~n",[Mod_info]),    
    Mod_md5 = get_md5(File_path),
    %% Specs = get_specs(Mod_name,Mod_info),
						% io:format("~p~n",[Mod_md5]),
    Record = #file_poller_record{module_name = Mod_name,filetype = Filetype,module_info = Mod_info,module_md5 = Mod_md5}, %%specs = Specs,
    Record .


get_files(Directory,Filetype) ->
    Path_modified_for_files = filename:join(Directory,"*."++Filetype),
    Files_in_directory = filelib:wildcard(Path_modified_for_files).

get_module_names(File,Filetype)->
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
    
    







						%   Erl_files_ create_record(Erl_files,[]),

%% append_record(Record) ->
%%     Record|Records_list.    



%%   io:format("~p",Erl_files),
%%     Beam_files = get_files("/home/ekousha/Documents/Erlang_Projs/loaded","beam"),
%%     io:format("~p",Beam_files),
%%     Erl_module_names = get_module_names(Erl_files,erl),
%%     io:format("~p",Erl_module_names),
%%     Beam_module_names = get_module_names(Beam_files,beam),
%%     io:format("~p",Beam_module_names),
%%     Erl_module_info = get_module_info(Erl_module_names),
%%     io:format("~p",Erl_module_info),
%%     Beam_module_info = get_module_info(Beam_module_names).

%% [create_record(X)|| X <- Erl_files ]

%%  create_record(H|T,Acc)->
%%      Acc_h =   #file_poller_record{directory_erl = H},
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
%% #file_poller_record{directory = Directory,
%% 			files=Input_to_moduleinfo,
%% 			details=[Mod_info]}.


%% #file_poller_record{directory_erl = Erl_files ,directory_beam = Beam_files ,module_names_erl = Erl_module_names, module_names_beam = Beam_module_names,details_erl = Erl_module_info, details_beam = Beam_module_info}.



%% append_record(Record_list_head),
%% Records_list_head|Records_list,
			%   io:format("~p~n",[Record]),

