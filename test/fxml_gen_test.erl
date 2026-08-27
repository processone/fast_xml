%%%----------------------------------------------------------------------
%%% File    : fxml_gen_test.erl
%%% Purpose : XML generator testing
%%%
%%%
%%% Copyright (C) 2002-2026 ProcessOne, SARL. All Rights Reserved.
%%%
%%% Licensed under the Apache License, Version 2.0 (the "License");
%%% you may not use this file except in compliance with the License.
%%% You may obtain a copy of the License at
%%%
%%%     http://www.apache.org/licenses/LICENSE-2.0
%%%
%%% Unless required by applicable law or agreed to in writing, software
%%% distributed under the License is distributed on an "AS IS" BASIS,
%%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%% See the License for the specific language governing permissions and
%%% limitations under the License.
%%%
%%%----------------------------------------------------------------------
-module(fxml_gen_test).

-include_lib("eunit/include/eunit.hrl").

reference_order_test() ->
    TmpBase = case os:getenv("TMPDIR") of
		  false -> "/tmp";
		  Dir -> Dir
	      end,
    TmpDir = filename:join(
	       TmpBase,
	       "fast_xml_reference_order_" ++ os:getpid() ++ "_" ++
	       integer_to_list(erlang:unique_integer([positive]))),
    Modules = [reference_order_codec_external, reference_order_codec],
    ok = file:make_dir(TmpDir),
    try
	ok = fxml_gen:compile(
	       "test/reference_order_codec.spec",
	       [{erl_dir, TmpDir}, {hrl_dir, TmpDir}]),
	lists:foreach(fun(Mod) -> load_generated(Mod, TmpDir) end, Modules),
	{xmlel, <<"ordered">>, _, Children} =
	    reference_order_codec:encode(
	      {ordered,
	       [{mechanism, <<"first">>}, {mechanism, <<"second">>}],
	       {inline}}),
	Names = [Name || {xmlel, Name, _, _} <- Children],
	?assertEqual(
	   [<<"mechanism">>, <<"mechanism">>, <<"inline">>],
	   Names)
    after
	lists:foreach(fun unload/1, lists:reverse(Modules)),
	remove_generated(TmpDir, Modules)
    end.

load_generated(Mod, Dir) ->
    Source = filename:join(Dir, atom_to_list(Mod) ++ ".erl"),
    {ok, Mod, Binary} = compile:file(Source, [binary]),
    {module, Mod} = code:load_binary(Mod, Source, Binary),
    ok.

unload(Mod) ->
    code:delete(Mod),
    code:purge(Mod),
    ok.

remove_generated(Dir, Modules) ->
    Files = [atom_to_list(Mod) ++ ".erl" || Mod <- Modules] ++
	["reference_order_codec.hrl"],
    lists:foreach(fun(File) ->
			  ok = file:delete(filename:join(Dir, File))
		  end, Files),
    ok = file:del_dir(Dir).
