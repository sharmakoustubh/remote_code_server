-module(file_handler).
-export([file_handler_main_function/0,get_files/2,get_module_names/1]).

%% 
%% -record(file_poller_record, {directory,
%% 			     files=[],
%% 			     details=[]}).

file_handler_main_function()->
    Erl_files = get_files("/home/ekousha/Documents/Erlang_Projs/loaded","erl"),
    io:format("~p",Erl_files),
    Beam_files = get_files("/home/ekousha/Documents/Erlang_Projs/loaded","beam"),
    io:format("~p",Beam_files),
    Erl_module_names = get_module_names(Erl_files),
    io:format("~p",Erl_module_names),
    Beam_module_names = get_module_names(Beam_files),
    io:format("~p",Beam_module_names).
  
  %% Erl_module_info = get_module_info(Erl_module_names),
  %%   io:format("~p",Erl_module_info),
  %%   Beam_module_info = get_module_info(Beam_module_names).
   

get_files(Directory,Filetype) ->
    Path_modified_for_files = filename:join(Directory,"*."++Filetype),
    Files_in_directory = filelib:wildcard(Path_modified_for_files).

get_module_names(Files)->
    [ filename:basename(File, ".erl") || File <- Files ].

get_module_info(All_files_basename)->
    Atom_module_names = [list_to_atom(One_file)||One_file <- All_files_basename],
    [Module:module_info() ||Module <- Atom_module_names].


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


