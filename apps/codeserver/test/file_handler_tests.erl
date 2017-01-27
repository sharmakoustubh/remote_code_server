-module(file_handler_tests).
-include_lib("eunit/include/eunit.hrl").
-define(Module_info,[[{exports,[{start,0},{module_info,0},{module_info,1}]},
		      {imports,[_]},
		      {attributes,[_]},
		      {compile,[_]}],
		     [{exports,[{ets_table1_test_,0},
				{test,0},
				{module_info,0},
				{module_info,1}]},
		      {imports,_},
		      {attributes,_},
		      {compile,_}]]).


file_handler_test_() ->
    {foreach,
     fun setup/0,
     fun cleanup/1,
     [{"check the right files are there in dir",fun check_files_in_directory/0},
      {"check file handler process starts",fun spawn_file_handler_process/0},   
     % {"check the record is being created", fun create_record/0},
      {"check admin can put Module Function in restricted field of record", fun admin_msg_restrict/0},
      {"check admin can delete Module Function from restricted field of record",fun admin_msg_unrestrict/0},
      {"check if module name is in string format",fun check_module_name/0},
      {"check load file are loaded in dir",fun load_file_in_dir/0},
      {"check if module is deleted when admin asks for it",fun admin_msg_delete_module/0},
      {"check directory loaded is added to the path",fun add_dir_to_path/0},
{"check you get correct md5 ",fun get_md5/0}
]}.


setup() ->
    ok = file_handler:start().

cleanup(_) ->
    exit(whereis(file_handler),kill),
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
    io:format("~p~n",[Pid]),    
    ?assertMatch(true,is_pid(Pid)).

admin_msg_restrict()->
    Expected = {ok,restricted},
    Ref = make_ref(),
    file_handler ! {self(), Ref, restrict,"ets_table1",{start,0}},
    Got = receive 
	      {Ref,Result}->
		  Result
	  after 5000->
		  {error,timeout}
	  end,
    ?assertMatch(Expected,Got).

admin_msg_unrestrict()->
    Expected = {ok,unrestricted},
    Ref1 = make_ref(),
    Ref2 = make_ref(),
    file_handler ! {self(), Ref1, restrict,"ets_table1",{start,0}},
    timer:sleep(500),
    file_handler ! {self(), Ref2,unrestrict,"ets_table1",{start,0}},
    Got    = receive  
		 {Ref2,Result}->
		     Result
	     after 5000->
		     {error,timeout}
	     end,
    ?assertMatch(Expected,Got).

admin_msg_delete_module()->
    Expected = {ok,deleted_module},
    Ref = make_ref(),

    file_handler ! {self(), Ref, delete_module,"ets_table1"},
    Got    = receive  
		 {Ref,Result}->
		     Result
	     after 5000->
		     {error,timeout}
	     end,
    ?assertMatch(Expected,Got).

check_files_in_directory() ->
    Input1 = "/home/ekousha/codeserver/apps/codeserver/loaded",
    Input2 = "erl",
    Result = file_handler:get_files(Input1,Input2),
    Expected = ["/home/ekousha/codeserver/apps/codeserver/loaded/ets_table1.erl"],
    ?assertEqual(Expected, Result).

load_file_in_dir()-> 
    Expected = {module,ets_table1},
    Result =file_handler:load_file_in_dir("ets_table1"),
    ?assertEqual(Expected, Result).

check_module_name()->
    Input1 = "/home/ekousha/codeserver/apps/codeserver/loaded/ets_table1.erl",
    Input2 = erl,
    Result = file_handler:get_module_name(Input1,Input2),
    Expected = "ets_table1",
    ?assertEqual(Expected, Result).

add_dir_to_path()->
    Expected = true, 
    Result = file_handler:add_dir_to_path("/home/ekousha/codeserver/apps/codeserver/loaded"),
    ?assertEqual(Expected,Result).


get_md5()->
    Expected = "B0EE12C0E644D8AE78A7E6E5C197900D",
    Got =  file_handler:get_md5("/home/ekousha/codeserver/apps/codeserver/loaded/ets_table1.erl"),
    ?assertMatch(Expected,Got).

loading_test_()->
    {setup,
     fun copy_files/0,
     fun delete_files/1,
     [{"check you can load an erl",
       fun check_a_src_file_is_loaded/0},
      {"check you can load a beam",
       fun check_a_beam_file_is_loaded/0}]}.

copy_files()->
    os:cmd("cp ~/codeserver/apps/codeserver/test/flyingfile.erl ~/codeserver/apps/codeserver/loaded"),
    os:cmd("cp ~/codeserver/apps/codeserver/test/movingbeam.beam ~/codeserver/apps/codeserver/loaded").

delete_files(_)->
    os:cmd("rm ~/codeserver/apps/codeserver/loaded/flyingfile.erl"),
    os:cmd("rm ~/codeserver/apps/codeserver/loaded/movingbeam.beam").

check_a_src_file_is_loaded()->
    file_handler:load_file_in_dir("flyingfile"),
    Result = case catch flyingfile:module_info() of
		 {'EXIT',_}->
		     not_loaded;
		 _ ->
		     ok
	     end,
    ?assertEqual(ok,Result).

check_a_beam_file_is_loaded()->
    file_handler:load_file_in_dir("movingbeam"),
    Result = case catch movingbeam:module_info() of
		 {'EXIT',_}->
		     not_loaded;
		 _ ->
		     ok
	     end,
    ?assertEqual(ok,Result).

change_the_file_content()->
    file_handler:load_file_in_dir("flyingfile"),
    Md5_1 = file_handler:get_md5(),
    Result = case catch flyingfile:module_info() of
		 {'EXIT',_}->
		     not_loaded;
		 _ ->
		     ok
	     end,
    ?assertEqual(ok,Result).

