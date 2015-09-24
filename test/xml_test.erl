%%%----------------------------------------------------------------------
%%% File    : xml_test.erl
%%% Author  : Evgeniy Khramtsov <ekhramtsov@process-one.net>
%%% Purpose : xml module testing
%%% Created : 17 Dec 2013 by Evgeny Khramtsov <ekhramtsov@process-one.net>
%%%
%%%
%%% Copyright (C) 2002-2015 ProcessOne, SARL. All Rights Reserved.
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

-module(xml_test).

-compile(export_all).

-include_lib("eunit/include/eunit.hrl").
-include("xml.hrl").

new() ->
    new(self()).
new(Pid) ->
    new(Pid, infinity).
new(Pid, MaxSize) ->
    xml_stream:new(Pid, MaxSize).

close(State) ->
    ?assertEqual(true, xml_stream:close(State)).

start_test() ->
    ?assertEqual(ok, application:start(p1_xml)).

tag_test() ->
    ?assertEqual(#xmlel{name = <<"root">>},
		 xml_stream:parse_element("<root/>")).

empty_tag_test() ->
    ?assertEqual(#xmlel{name = <<"root">>},
		 xml_stream:parse_element("<root></root>")).

tag_with_cdata_test() ->
    ?assertEqual(#xmlel{name = <<"root">>,
			children = [{xmlcdata, <<"cdata">>}]},
		 xml_stream:parse_element("<root>cdata</root>")).

tag_with_attrs_test() ->
    ?assertEqual(#xmlel{name = <<"root">>,
			attrs = [{<<"a">>, <<"1">>}, {<<"b">>, <<"2">>}]},
		 xml_stream:parse_element("<root a='1' b='2'/>")).

tag_with_empty_attr_test() ->
    ?assertEqual(#xmlel{name = <<"root">>, attrs = [{<<"a">>, <<>>}]},
		 xml_stream:parse_element("<root a=''/>")).

tag_with_prefix_test() ->
    ?assertEqual(#xmlel{name = <<"prefix:root">>,
			attrs = [{<<"xmlns:prefix">>, <<"ns">>}]},
		 xml_stream:parse_element("<prefix:root xmlns:prefix='ns'/>")).

tag_with_attr_with_prefix_test() ->
    ?assertEqual(#xmlel{name = <<"root">>,
			attrs = [{<<"xmlns:prefix1">>, <<"ns1">>},
				 {<<"xmlns:prefix2">>, <<"ns2">>},
				 {<<"prefix1:a">>, <<"1">>},
				 {<<"prefix2:b">>, <<"2">>}]},
		 xml_stream:parse_element(
		   "<root prefix1:a='1' xmlns:prefix1='ns1'"
		   "      prefix2:b='2' xmlns:prefix2='ns2'/>")).

tag_with_tags_test() ->
    ?assertEqual(#xmlel{name = <<"root">>,
			children = [#xmlel{name = <<"a">>},
				    {xmlcdata, <<"cdata1">>},
				    #xmlel{name = <<"b">>},
				    {xmlcdata, <<"cdata2">>}]},
		 xml_stream:parse_element("<root><a/>cdata1<b/>cdata2</root>")).

receiver(Acc) ->
    receive
	{'$gen_event', Msg} ->
	    receiver([Msg|Acc]);
	{get, Parent} ->
	    Parent ! lists:reverse(Acc),
	    receiver([])
    end.

collect_events(Pid) ->
    Pid ! {get, self()},
    receive
	Events ->
	    Events
    end.

stream_test() ->
    CallbackPid = spawn_link(fun() -> receiver([]) end),
    Stream0 = new(CallbackPid),
    Data = ["<prefix:root prefix:r='1' xmlns:prefix='ns'>",
	    "junk1", "<a/>", "junk2", "<b>cdata</b>", "junk3",
	    "</prefix:root>"],
    StreamN = lists:foldl(
		fun(Chunk, Stream) ->
			xml_stream:parse(Stream, Chunk)
		end, Stream0, Data),
    close(StreamN),
    ?assertEqual(
       [{xmlstreamstart, <<"prefix:root">>, [{<<"xmlns:prefix">>, <<"ns">>},
					     {<<"prefix:r">>, <<"1">>}]},
	{xmlstreamelement, #xmlel{name = <<"a">>}},
	{xmlstreamelement, #xmlel{name = <<"b">>,
				  children = [{xmlcdata, <<"cdata">>}]}},
	{xmlstreamend, <<"prefix:root">>}],
       collect_events(CallbackPid)).

stream_with_joined_cdata_test() ->
    CallbackPid = spawn_link(fun() -> receiver([]) end),
    Stream0 = new(CallbackPid),
    Data = ["<root>", "<a>", "1", "2", "3", "</a>", "</root>"],
    StreamN = lists:foldl(
		fun(Chunk, Stream) ->
			xml_stream:parse(Stream, Chunk)
		end, Stream0, Data),
    close(StreamN),
    ?assertEqual(
       [{xmlstreamstart, <<"root">>, []},
	{xmlstreamelement, #xmlel{name = <<"a">>,
				  children = [{xmlcdata, <<"123">>}]}},
	{xmlstreamend, <<"root">>}],
       collect_events(CallbackPid)).

splitted_stream_test() ->
    CallbackPid = spawn_link(fun() -> receiver([]) end),
    Stream0 = new(CallbackPid),
    Stream1 = xml_stream:parse(Stream0, "<root"),
    ?assertEqual([], collect_events(CallbackPid)),
    Stream2 = xml_stream:parse(Stream1, "><a>"),
    ?assertEqual([{xmlstreamstart, <<"root">>, []}],
		 collect_events(CallbackPid)),
    Stream3 = xml_stream:parse(Stream2, "</a><b/><c attr="),
    ?assertEqual([{xmlstreamelement, #xmlel{name = <<"a">>}},
		  {xmlstreamelement, #xmlel{name = <<"b">>}}],
		 collect_events(CallbackPid)),
    Stream4 = xml_stream:parse(Stream3, "'1'></c>"),
    ?assertEqual([{xmlstreamelement, #xmlel{name = <<"c">>,
					    attrs = [{<<"attr">>, <<"1">>}]}}],
		 collect_events(CallbackPid)),
    Stream5 = xml_stream:parse(Stream4, ""),
    ?assertEqual([], collect_events(CallbackPid)),
    Stream6 = xml_stream:parse(Stream5, "</root>"),
    ?assertEqual([{xmlstreamend, <<"root">>}], collect_events(CallbackPid)),
    close(Stream6).

too_big_test() ->
    CallbackPid = spawn_link(fun() -> receiver([]) end),
    Stream0 = new(CallbackPid, 5),
    Stream1 = xml_stream:parse(Stream0, "<a>"),
    Stream2 = xml_stream:parse(Stream1, "<b/>"),
    Stream3 = xml_stream:parse(Stream2, "<c/>"),
    Stream4 = xml_stream:parse(Stream3, "<de/>"),
    ?assertEqual([{xmlstreamstart, <<"a">>, []},
		  {xmlstreamelement, #xmlel{name = <<"b">>}},
		  {xmlstreamelement, #xmlel{name = <<"c">>}},
		  {xmlstreamerror, <<"XML stanza is too big">>}],
		 collect_events(CallbackPid)),
    close(Stream4).

close_close_test() ->
    Stream = new(),
    close(Stream),
    ?assertError(badarg, xml_stream:close(Stream)).

close_parse_test() ->
    Stream = new(),
    close(Stream),
    ?assertError(badarg, xml_stream:parse(Stream, "junk")).

close_change_callback_pid_test() ->
    Stream = new(),
    close(Stream),
    ?assertError(badarg, xml_stream:change_callback_pid(Stream, self())).

change_callback_pid_test() ->
    Pid1 = spawn_link(fun() -> receiver([]) end),
    Pid2 = spawn_link(fun() -> receiver([]) end),
    Stream0 = new(Pid1),
    Stream1 = xml_stream:parse(Stream0, "<root>"),
    ?assertEqual([{xmlstreamstart, <<"root">>, []}],
		 collect_events(Pid1)),
    Stream2 = xml_stream:change_callback_pid(Stream1, Pid2),
    Stream3 = xml_stream:parse(Stream2, "</root>"),
    ?assertEqual([{xmlstreamend, <<"root">>}],
		 collect_events(Pid2)),
    close(Stream3).

badarg_new_test() ->
    ?assertError(badarg, xml_stream:new(1)),
    ?assertError(badarg, xml_stream:new(self(), unlimited)),
    ?assertError(badarg, xml_stream:new(foo, fun() -> ok end)).

badarg_parse_test() ->
    Stream = new(),
    ?assertError(badarg, xml_stream:parse(1, <<"<root>">>)),
    ?assertError(badarg, xml_stream:parse(<<>>, "<root>")),
    ?assertError(badarg, xml_stream:parse(Stream, blah)),
    ?assertError(badarg, xml_stream:parse(foo, fun() -> ok end)),
    close(Stream).

badarg_change_callback_pid_test() ->
    Stream = new(),
    ?assertError(badarg, xml_stream:change_callback_pid(1, self())),
    ?assertError(badarg, xml_stream:change_callback_pid(<<>>, self())),
    ?assertError(badarg, xml_stream:change_callback_pid(Stream, foo)),
    ?assertError(badarg, xml_stream:change_callback_pid(foo, fun() -> ok end)),
    close(Stream).

badarg_close_test() ->
    Stream = new(),
    ?assertError(badarg, xml_stream:close(1)),
    ?assertError(badarg, xml_stream:close(<<>>)),
    close(Stream).

badarg_parse_element_test() ->
    ?assertError(badarg, xml_stream:parse_element(1)).

parse_error_test() ->
    L = ["<", "<>", "</>", "</>", "/>",
	 "<x", "<x>", "x/>", "<x/>junk",
	 "<x a/>", "<x a=f/>", "<x a='5'>",
	 "<x:y/>", "<x y:a='1'/>"],
    lists:foreach(
      fun(S) ->
	      ?assertMatch({error, _}, xml_stream:parse_element(S))
      end, L).

dead_pid_test() ->
    CallbackPid = spawn(fun() -> receiver([]) end),
    ?assertEqual(true, exit(CallbackPid, kill)),
    ?assertEqual(false, is_process_alive(CallbackPid)),
    Stream0 = new(CallbackPid),
    Stream1 = xml_stream:parse(Stream0, "<root><a/></root>"),
    close(Stream1).

huge_element_test() ->
    Tags = [list_to_binary(["a", integer_to_list(I)])
	    || I <- lists:seq(1, 100000)],
    Data = ["<root>", [[$<, Tag, "/>"] || Tag <- Tags], "</root>"],
    Els = #xmlel{name = <<"root">>,
		 children = [#xmlel{name = Tag} || Tag <- Tags]},
    ?assertEqual(Els, xml_stream:parse_element(Data)).

many_stream_elements_test() ->
    CallbackPid = spawn(fun() -> receiver([]) end),
    Stream0 = new(CallbackPid),
    Stream1 = xml_stream:parse(Stream0, "<root>"),
    ?assertEqual([{xmlstreamstart, <<"root">>, []}],
		 collect_events(CallbackPid)),
    Stream2 = lists:foldl(
		fun(I, Stream) ->
			Tag = list_to_binary(["a", integer_to_list(I)]),
			NewStream = xml_stream:parse(Stream, [$<, Tag, "/>"]),
			?assertEqual([{xmlstreamelement, #xmlel{name = Tag}}],
				     collect_events(CallbackPid)),
			NewStream
		end, Stream1, lists:seq(1, 100000)),
    close(Stream2).

billionlaughs_test() ->
    Data =
	"<?xml version='1.0'?><!DOCTYPE test [ <!ENTITY lol \"lol\"> "
	"<!ENTITY lol1 \"&lol;&lol;&lol;&lol;&lol;&lol;&lol;&lol;&lol;&lol;\"> "
	"<!ENTITY lol2 \"&lol1;&lol1;&lol1;&lol1;&lol1;&lol1;&lol1;&lol1;&lol1;&lol1;\"> "
	"<!ENTITY lol3 \"&lol2;&lol2;&lol2;&lol2;&lol2;&lol2;&lol2;&lol2;&lol2;&lol2;\"> "
	"<!ENTITY lol4 \"&lol3;&lol3;&lol3;&lol3;&lol3;&lol3;&lol3;&lol3;&lol3;&lol3;\"> "
	"<!ENTITY lol5 \"&lol4;&lol4;&lol4;&lol4;&lol4;&lol4;&lol4;&lol4;&lol4;&lol4;\"> "
	"<!ENTITY lol6 \"&lol5;&lol5;&lol5;&lol5;&lol5;&lol5;&lol5;&lol5;&lol5;&lol5;\"> "
	"<!ENTITY lol7 \"&lol6;&lol6;&lol6;&lol6;&lol6;&lol6;&lol6;&lol6;&lol6;&lol6;\"> "
	"<!ENTITY lol8 \"&lol7;&lol7;&lol7;&lol7;&lol7;&lol7;&lol7;&lol7;&lol7;&lol7;\"> "
	"<!ENTITY lol9 \"&lol8;&lol8;&lol8;&lol8;&lol8;&lol8;&lol8;&lol8;&lol8;&lol8;\"> "
	"] ><stream:stream xmlns='jabber:client' "
	"xmlns:stream='http://etherx.jabber.org/streams' version='1.0' to='&lol9;'>\n",
    CallbackPid = spawn_link(fun() -> receiver([]) end),
    Stream0 = new(CallbackPid),
    Stream1 = xml_stream:parse(Stream0, Data),
    close(Stream1),
    ?assertMatch([{xmlstreamerror, _}], collect_events(CallbackPid)).
