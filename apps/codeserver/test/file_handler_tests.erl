-module(file_handler_tests).
-include_lib("eunit/include/eunit.hrl").
-include("../src/record_definition.hrl").
-export([check_amended_file_is_changed_in_rec/0]).

file_handler_test_() ->
    {foreach,
     fun setup/0,
     fun cleanup/1,
     [
   %%  {"check  files are there in dir",fun check_files_in_directory/0},
      {"check file handler process starts",fun spawn_file_handler_process/0},   
      {"check admin can put Module Function in restricted field of record", fun admin_msg_restrict/0},
      {"check admin can delete Module Function from restricted field of record",fun admin_msg_unrestrict/0},
      {"check if module name is in string format",fun check_module_name/0},
      {"check files are loaded in dir loaded",fun load_file_from_dir/0},
      {"check if module is deleted when admin asks for it",fun admin_msg_delete_module/0},
      {"check directory loaded is added to the path",fun add_dir_to_path/0},
      {"check you get correct md5 ",fun get_md5/0},
      {" Restrict admin_msg_restrict_undefined_function_input ",fun admin_msg_restrict_undefined_function_input/0},
      {" Un-restrict admin_msg_restrict_undefined_function_input ",fun admin_msg_unrestrict_undefined_function_input/0}
     ]}.


setup() ->
    ok = file_handler:start().

cleanup(_) ->
    exit(whereis(file_handler),kill),
    file:delete("/home/ekousha/codeserver/apps/codeserver/loaded/ets_table1.beam"),
    ensure_exited().

ensure_exited() ->

    case whereis(file_handler) of
	undefined ->
	    ok;
	_ ->
	    timer:sleep(10),
	    ensure_exited()
    end.       

spawn_file_handler_process()->
    Pid= whereis(file_handler),
 %%   io:format("~p~n",[Pid]),    
    ?assertMatch(true,is_pid(Pid)).

admin_msg_restrict()->
    Result = file_handler:restrict("ets_table1", {start, 0}),
    Expected ={ok,"the function in module was restricted"},
    ?assertEqual(Expected, Result).

admin_msg_restrict_undefined_function_input()->
    Result = file_handler:restrict("ets_table1", {random_fun, 0}),
    Expected = {error,"the module exists but the function does not exist"},
    ?assertEqual(Expected, Result).


admin_msg_unrestrict()->
    file_handler:restrict("ets_table1", {start, 0}),
    Result = file_handler:unrestrict("ets_table1", {start, 0}),
    %% Module = {"my_module", #module{restricted = [{f, 0}]}},
    %% Result = file_handler:unrestrict(self(),make_ref(),"my_module", {f, 0}),
    Expected = {ok,"the function in module was unrestricted"},
    ?assertEqual(Expected, Result).

admin_msg_unrestrict_undefined_function_input()->
      Result = file_handler:unrestrict("ets_table1", {random_fun, 0}),
    Expected = {error,"the module exists but the function does not exist"},
    ?assertEqual(Expected, Result).


admin_msg_delete_module()->
    Module = {"my_module", #module{restricted = [{f, 0}]}},
    Result = file_handler:delete_module_keyval("my_module",[Module]),
    Expected = [],
    ?assertEqual(Expected, Result).

load_file_from_dir()-> 
    Expected = {module,ets_table1},
    Result =file_handler:compile_and_load_file_from_dir(ets_table1,"/home/ekousha/codeserver/apps/codeserver/loaded/ets_table1.erl"),
    ?assertEqual(Expected, Result).

check_module_name()->
    Input1 = "/home/ekousha/codeserver/apps/codeserver/loaded/ets_table1.erl",
    Result = file_handler:get_module_name(Input1),
    Expected = "ets_table1",
    ?assertEqual(Expected, Result).

add_dir_to_path()->
    Expected = true, 
    Result = file_handler:add_dir_to_path("/home/ekousha/codeserver/apps/codeserver/loaded"),
    ?assertEqual(Expected,Result).


get_md5()->
    Expected = "8CE9FCB2B010A44FD97CD3AFBC9FD3CD",
    Got =  file_handler:get_md5("/home/ekousha/codeserver/apps/codeserver/loaded/ets_table1.erl"),
    ?assertMatch(Expected,Got).

loading_test_()->
    {setup,
     fun copy_files/0,
     fun delete_files/1,
     [
       {"check you can load an erl",
        fun check_a_src_file_is_loaded/0},
       {"check you can load a beam",
        fun check_a_beam_file_is_loaded/0}
     ]}.

copy_files()->
    os:cmd("cp ~/codeserver/apps/codeserver/test/flyingfile.erl ~/codeserver/apps/codeserver/loaded"),
    os:cmd("cp ~/codeserver/apps/codeserver/test/movingbeam.beam ~/codeserver/apps/codeserver/loaded").

delete_files(_)->
    os:cmd("rm ~/codeserver/apps/codeserver/loaded/flyingfile.erl"),
    os:cmd("rm ~/codeserver/apps/codeserver/loaded/flyingfile.beam"),
    os:cmd("rm ~/codeserver/apps/codeserver/loaded/movingbeam.beam").

check_a_src_file_is_loaded()->
    file_handler:compile_and_load_file_from_dir(flyingfile,"/home/ekousha/codeserver/apps/codeserver/loaded/flyingfile.erl"),
    Result = case catch flyingfile:module_info() of
		 {'EXIT',_}->
		     not_loaded;
		 _ ->
		     ok
	     end,
    ?assertEqual(ok,Result).

check_a_beam_file_is_loaded()->
    file_handler:compile_and_load_file_from_dir(movingbeam,"/home/ekousha/codeserver/apps/codeserver/loaded/movingbeam.beam"),
    Result = case catch movingbeam:module_info() of
		 {'EXIT',_}->
		     not_loaded;
		 _ ->
		     ok
	     end,
    ?assertEqual(ok,Result).
 
fetch_and_amendfile_test_() ->
    {foreach,
     fun setup2/0,
     fun cleanup2/1,
     [
      {"amend the file and check if the record is changed",fun check_amended_file_is_changed_in_rec/0},
      {"check fetch gives required output",fun check_fetch/0}
     ]}.



setup2() ->
    setup(),
    Target = "/home/ekousha/codeserver/apps/codeserver/loaded/amend_file.erl",
    os:cmd("cp /home/ekousha/codeserver/apps/codeserver/test/dummies/amend_file.erl " ++ Target),
    ?assertEqual(true, filelib:is_file(Target)).
    

cleanup2(Args) ->
    cleanup(Args),
    file:delete("/home/ekousha/codeserver/apps/codeserver/loaded/amend_file.erl"),
    file:delete("/home/ekousha/codeserver/apps/codeserver/loaded/amend_file.beam").


	     
check_amended_file_is_changed_in_rec()->
    timer:sleep(500),
    {ok,Res1} = file_handler:fetch(),
   %% io:format(user,"before adding new fun in amend_file~p~n",[Res1]),
    Mod_rec1 = proplists:get_value("amend_file",Res1),

    R1_exported = Mod_rec1#module.exported,
    ?assertMatch([{my_fun_in_amend,0},{module_info,0},{module_info,1}],R1_exported),
    Target = "/home/ekousha/codeserver/apps/codeserver/loaded/amend_file.erl",
    os:cmd("cp /home/ekousha/codeserver/apps/codeserver/loadedtmp/amend_file.erl " ++ Target),

    timer:sleep(500),
  %%  io:format(user, "which? ~p~n", [code:which(amend_file)]),
    {ok,Res2} = file_handler:fetch(),
  %%  io:format(user,"after adding new fun in amend_file~p~n",[Res2]),
    Mod_rec2 = proplists:get_value("amend_file",Res2),
    R2_exported = Mod_rec2#module.exported,

    ?assertEqual(lists:sort([{new_fun,0},{my_fun_in_amend,0},{module_info,0},{module_info,1}]),lists:sort(R2_exported)).


%%[{my_fun_in_amend,0},{module_info,0},{module_info,1}] 

check_fetch()->   
    timer:sleep(500),
    {ok,Result} = file_handler:fetch(),
%%    Mod_rec1 = proplists:get_value("amend_file",Result),
%%    R1_exported = Mod_rec1#module.exported,
    Expect =    [{"amend_file",
                   {module,".erl",[],
                           [{my_fun_in_amend,0},
                            {module_info,0},
                            {module_info,1}],
                           "25803448269B32F6C2FF222F4BF65839",
                           {2017,2,9,15,10,47}}},
                  {"ets_table1",
                   {module,".erl",[],
                           [{start,0},
                            {return_ok,0},
                            {greet_me,1},
                            {module_info,0},
                            {module_info,1}],
                           "8CE9FCB2B010A44FD97CD3AFBC9FD3CD",
                           {2017,2,9,15,10,47}}}],

    ?assertMatch([{"amend_file",
                   {module,".erl",[],
                           [{my_fun_in_amend,0},
                            {module_info,0},
                            {module_info,1}],
                           "25803448269B32F6C2FF222F4BF65839",
                           _}},
                  {"ets_table1",
                   {module,".erl",[],
                           [{start,0},
                            {return_ok,0},
                            {greet_me,1},
                            {module_info,0},
                            {module_info,1}],
                           "8CE9FCB2B010A44FD97CD3AFBC9FD3CD",
                           _}}],Result).



























%% list_md5()->
%%     %% Expect = ["2FEFF17C44894ADFB17326621C8ACA8A","25803448269B32F6C2FF222F4BF65839"],
%%     Expect =["2FEFF17C44894ADFB17326621C8ACA8A","25803448269b32f6c2ff222f4bf65839"],

%%     Got = file_handler:list_md5(["/home/ekousha/codeserver/apps/codeserver/loaded/ets_table1.erl","/home/ekousha/codeserver/apps/codeserver/loaded/amend_file.erl"]),
    
%%     ?assertMatch(Expect,Got).

%% update_if_new()->
%%     Expect = [{"ets_table1",
%% 	       {module,"erl",[],
%% 		[{start,0},{module_info,0},{module_info
%% 					   ,1}],
%% 		"2FEFF17C44894ADFB17326621C8ACA8A"}}],
%%     Got = file_handlery:update_changed_files(["/home/ekousha/codeserver/apps/codeserver/loaded/ets_table1.erl"],["/home/ekousha/codeserver/apps/codeserver/loaded/ets_table1.erl"],Expect),
%%     ?assertMatch(Expect,Got).


%% fun() ->
%% 	     setup(),
%% 	     Target = "/home/ekousha/codeserver/apps/codeserver/loaded/amend_file.erl",
%% 	     os:cmd("cp /home/ekousha/codeserver/apps/codeserver/test/amend_file.erl " ++ Target),
%% 	     ?assertEqual(true, filelib:is_file(Target))
%%      end,
%%      fun(Arg) ->
%% 	     cleanup(Arg),
%% 	     file:delete("/home/ekousha/codeserver/apps/codeserver/loaded/amend_file.erl"),
%% 	     file:delete("/home/ekousha/codeserver/apps/codeserver/loaded/amend_file.beam")
%%      end,

  %% ?assertMatch([{"ets_table1",#module{filetype = _,
    %% 					restricted = _,
    %% 					exported = _,
    %% 					module_md5 = _}},
    %% 		  {"amend_file",#module{filetype = _,
    %% 					restricted = _,
    %% 					exported = _,
    %% 					module_md5 = _}}],
    
    
