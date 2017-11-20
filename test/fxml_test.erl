%%%----------------------------------------------------------------------
%%% File    : fxml_test.erl
%%% Author  : Evgeniy Khramtsov <ekhramtsov@process-one.net>
%%% Purpose : xml module testing
%%% Created : 17 Dec 2013 by Evgeny Khramtsov <ekhramtsov@process-one.net>
%%%
%%%
%%% Copyright (C) 2002-2017 ProcessOne, SARL. All Rights Reserved.
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

-module(fxml_test).

-compile(export_all).

-include_lib("eunit/include/eunit.hrl").
-include("fxml.hrl").

new() ->
    new(self()).
new(Pid) ->
    new(Pid, infinity).
new(Pid, MaxSize) ->
    fxml_stream:new(Pid, MaxSize).

close(State) ->
    ?assertEqual(true, fxml_stream:close(State)).

start_test() ->
    ?assertEqual(ok, fxml:load_nif(p1_nif_utils:get_so_path(fxml, [], "fxml"))),
    ?assertEqual(ok, fxml_stream:load_nif(p1_nif_utils:get_so_path(fxml_stream, [], "fxml_stream"))).

tag_test() ->
    ?assertEqual(#xmlel{name = <<"root">>},
		 fxml_stream:parse_element(<<"<root/>">>)).

empty_tag_test() ->
    ?assertEqual(#xmlel{name = <<"root">>},
		 fxml_stream:parse_element(<<"<root></root>">>)).

empty_tag_with_ns_test() ->
    ?assertEqual(#xmlel{name = <<"root">>, attrs = [{<<"xmlns">>, <<"ns">>}]},
		 fxml_stream:parse_element(<<"<root xmlns='ns'></root>">>)).

tag_with_cdata_test() ->
    ?assertEqual(#xmlel{name = <<"root">>,
			children = [{xmlcdata, <<"cdata">>}]},
		 fxml_stream:parse_element(<<"<root>cdata</root>">>)).

tag_with_attrs_test() ->
    ?assertEqual(#xmlel{name = <<"root">>,
			attrs = [{<<"a">>, <<"1">>}, {<<"b">>, <<"2">>}]},
		 fxml_stream:parse_element(<<"<root a='1' b='2'/>">>)).

tag_with_empty_attr_test() ->
    ?assertEqual(#xmlel{name = <<"root">>, attrs = [{<<"a">>, <<>>}]},
		 fxml_stream:parse_element(<<"<root a=''/>">>)).

tag_with_prefix_test() ->
    ?assertEqual(#xmlel{name = <<"prefix:root">>,
			attrs = [{<<"xmlns:prefix">>, <<"ns">>}]},
		 fxml_stream:parse_element(<<"<prefix:root xmlns:prefix='ns'/>">>)).

tag_with_prefix_children1_test() ->
    ?assertEqual(#xmlel{name = <<"prefix:root">>,
			attrs = [{<<"xmlns:prefix">>, <<"ns">>}],
			children = [#xmlel{name = <<"prefix:a">>}]},
		 fxml_stream:parse_element(<<"<prefix:root xmlns:prefix='ns'><prefix:a/></prefix:root>">>)).

tag_with_prefix_children2_test() ->
    ?assertEqual(#xmlel{name = <<"prefix:root">>,
			attrs = [{<<"xmlns:prefix">>, <<"ns">>}],
			children = [#xmlel{name = <<"a">>, attrs=[{<<"xmlns">>, <<"ns2">>}]}]},
		 fxml_stream:parse_element(<<"<prefix:root xmlns:prefix='ns'><a xmlns='ns2'/></prefix:root>">>)).

tag_with_prefix_children3_test() ->
    ?assertEqual(#xmlel{name = <<"prefix:root">>,
			attrs = [{<<"xmlns:prefix">>, <<"ns">>}],
			children = [#xmlel{name = <<"zed:a">>, attrs=[{<<"xmlns:zed">>, <<"ns2">>}]}]},
		 fxml_stream:parse_element(<<"<prefix:root xmlns:prefix='ns'><zed:a xmlns:zed='ns2'/></prefix:root>">>)).

tag_with_prefix_children4_test() ->
    ?assertEqual(#xmlel{name = <<"prefix:root">>,
			attrs = [{<<"xmlns:prefix">>, <<"ns">>}],
			children = [#xmlel{name = <<"a">>, attrs=[{<<"xmlns">>, <<"ns">>}]}]},
		 fxml_stream:parse_element(<<"<prefix:root xmlns:prefix='ns'><a xmlns='ns'/></prefix:root>">>)).

tag_with_attr_with_prefix_test() ->
    ?assertEqual(#xmlel{name = <<"root">>,
			attrs = [{<<"xmlns:prefix1">>, <<"ns1">>},
				 {<<"xmlns:prefix2">>, <<"ns2">>},
				 {<<"prefix1:a">>, <<"1">>},
				 {<<"prefix2:b">>, <<"2">>}]},
		 fxml_stream:parse_element(<<
		   "<root prefix1:a='1' xmlns:prefix1='ns1'",
		   "      prefix2:b='2' xmlns:prefix2='ns2'/>">>)).

tag_with_tags_test() ->
    ?assertEqual(#xmlel{name = <<"root">>,
			children = [#xmlel{name = <<"a">>},
				    {xmlcdata, <<"cdata1">>},
				    #xmlel{name = <<"b">>},
				    {xmlcdata, <<"cdata2">>}]},
		 fxml_stream:parse_element(<<"<root><a/>cdata1<b/>cdata2</root>">>)).

receiver(Acc) ->
    receive
	{'$gen_event', Msg} ->
	    receiver([Msg|Acc]);
	{get, Parent} ->
	    Parent ! lists:reverse(Acc),
	    receiver([]);
	{close, Parent} ->
	    Parent ! closed
    end.

collect_events(Pid) ->
    Pid ! {get, self()},
    receive
	Events ->
	    Events
    end.

close_collector(Pid) ->
    Pid ! {close, self()},
    receive
	closed ->
	    closed
    end.

parser_loop(TestData) ->
		parser_loop(infinity, [], TestData).

parser_loop(MaxSize, Options, TestData) ->
    CollectorPid = spawn_link(fun() -> receiver([]) end),
    parser_loop_int(TestData, fxml_stream:new(CollectorPid, MaxSize, Options), CollectorPid).

parser_loop_int([reset | Tail], Handle, CollectorPid) ->
    parser_loop_int(Tail, fxml_stream:reset(Handle), CollectorPid);
parser_loop_int([{Input, Output} | Tail], Handle, CollectorPid) ->
    NHandle = fxml_stream:parse(Handle, Input),
    ?assertEqual(Output, collect_events(CollectorPid)),
    parser_loop_int(Tail, NHandle, CollectorPid);
parser_loop_int([], Handle, CollectorPid) ->
    close(Handle),
    close_collector(CollectorPid).

stream_test() ->
    CallbackPid = spawn_link(fun() -> receiver([]) end),
    Stream0 = new(CallbackPid),
    Data = [<<"<prefix:root prefix:r='1' xmlns:prefix='ns'>">>,
	    <<"junk1">>, <<"<a/>">>, <<"junk2">>, <<"<b>cdata</b>">>,
            <<"junk3">>, <<"</prefix:root>">>],
    StreamN = lists:foldl(
		fun(Chunk, Stream) ->
			fxml_stream:parse(Stream, Chunk)
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

stream_normalized_ns_test() ->
    CallbackPid = spawn_link(fun() -> receiver([]) end),
    Stream0 = new(CallbackPid),
    Data = [<<"<prefix:root prefix:r='1' xmlns='ns0' xmlns:t='ns1' xmlns:prefix='ns'>">>,
	    <<"junk1">>, <<"<a/>">>,
             <<"<t:a><s:b xmlns:s='ns2'><c/></s:b></t:a>">>,
	     <<"<prefix:n1/>">>, <<"<prefix:n1 xmlns:prefix='ns'/>">>,
             <<"<a xmlns='ns5'><b xmlns='ns5'/><t:c xmlns:t='ns1'/></a>">>, <<"<n2 xmlns='2'/>">>,
	     <<"<t:n3/>">>, <<"<v:n4 xmlns:v='3'/>">>,
             <<"<prefix:n5 xmlns:prefix='n4'><e1/><prefix:e2/></prefix:n5>">>,
	     <<"junk2">>, <<"<b>cdata</b>">>,
            <<"junk3">>, <<"</prefix:root>">>],
    StreamN = lists:foldl(
		fun(Chunk, Stream) ->
			fxml_stream:parse(Stream, Chunk)
		end, Stream0, Data),
    close(StreamN),
    ?assertEqual(
       [{xmlstreamstart, <<"prefix:root">>, [{<<"xmlns">>, <<"ns0">>},
                                             {<<"xmlns:t">>, <<"ns1">>},
					     {<<"xmlns:prefix">>, <<"ns">>},
					     {<<"prefix:r">>, <<"1">>}]},
	{xmlstreamelement, #xmlel{name = <<"a">>}},
	{xmlstreamelement, #xmlel{name = <<"t:a">>,
                                  children=[#xmlel{name = <<"b">>,
                                                   attrs=[{<<"xmlns">>, <<"ns2">>}, {<<"xmlns:s">>, <<"ns2">>}],
                                                   children=[#xmlel{name = <<"c">>,
                                                                    attrs=[{<<"xmlns">>, <<"ns0">>}]}]}]}},
	{xmlstreamelement, #xmlel{name = <<"prefix:n1">>}},
	{xmlstreamelement, #xmlel{name = <<"prefix:n1">>}},
	{xmlstreamelement, #xmlel{name = <<"a">>, attrs=[{<<"xmlns">>, <<"ns5">>}],
                                  children=[#xmlel{name = <<"b">>}, #xmlel{name = <<"t:c">>}]}},
	{xmlstreamelement, #xmlel{name = <<"n2">>, attrs=[{<<"xmlns">>, <<"2">>}]}},
	{xmlstreamelement, #xmlel{name = <<"t:n3">>}},
	{xmlstreamelement, #xmlel{name = <<"n4">>, attrs=[{<<"xmlns">>, <<"3">>}, {<<"xmlns:v">>, <<"3">>}]}},
	{xmlstreamelement, #xmlel{name = <<"n5">>, attrs=[{<<"xmlns">>, <<"n4">>}, {<<"xmlns:prefix">>, <<"n4">>}],
                                  children=[#xmlel{name = <<"e1">>, attrs=[{<<"xmlns">>, <<"ns0">>}]},
                                            #xmlel{name = <<"e2">>}]}},
	{xmlstreamelement, #xmlel{name = <<"b">>,
				  children = [{xmlcdata, <<"cdata">>}]}},
	{xmlstreamend, <<"prefix:root">>}],
       collect_events(CallbackPid)).

stream_reset_test() ->
    parser_loop([{<<"<a xmlns='ns1'><b/>">>,
		  [{xmlstreamstart, <<"a">>, [{<<"xmlns">>, <<"ns1">>}]},
		   {xmlstreamelement, #xmlel{name = <<"b">>}}]},
		 reset,
		 {<<"<a><b/></a>">>,
		  [{xmlstreamstart, <<"a">>, []},
		   {xmlstreamelement, #xmlel{name = <<"b">>}},
		   {xmlstreamend, <<"a">>}]}
		]).

stream_error_test() ->
    CallbackPid = spawn_link(fun() -> receiver([]) end),
    S0 = new(CallbackPid),
    S1 = fxml_stream:parse(S0, <<"<a><b>a</c>">>),
    close(S1),
    ?assertEqual(
       [{xmlstreamstart, <<"a">>, []},
	{xmlstreamerror, {7, <<"mismatched tag">>}}],
       collect_events(CallbackPid)).

stream_with_joined_cdata_test() ->
    CallbackPid = spawn_link(fun() -> receiver([]) end),
    Stream0 = new(CallbackPid),
    Data = [<<"<root>">>, <<"<a>">>, <<"1">>, <<"2">>, <<"3">>,
            <<"</a>">>, <<"</root>">>],
    StreamN = lists:foldl(
		fun(Chunk, Stream) ->
			fxml_stream:parse(Stream, Chunk)
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
    Stream1 = fxml_stream:parse(Stream0, <<"<root">>),
    ?assertEqual([], collect_events(CallbackPid)),
    Stream2 = fxml_stream:parse(Stream1, <<"><a>">>),
    ?assertEqual([{xmlstreamstart, <<"root">>, []}],
		 collect_events(CallbackPid)),
    Stream3 = fxml_stream:parse(Stream2, <<"</a><b/><c attr=">>),
    ?assertEqual([{xmlstreamelement, #xmlel{name = <<"a">>}},
		  {xmlstreamelement, #xmlel{name = <<"b">>}}],
		 collect_events(CallbackPid)),
    Stream4 = fxml_stream:parse(Stream3, <<"'1'></c>">>),
    ?assertEqual([{xmlstreamelement, #xmlel{name = <<"c">>,
					    attrs = [{<<"attr">>, <<"1">>}]}}],
		 collect_events(CallbackPid)),
    Stream5 = fxml_stream:parse(Stream4, <<"">>),
    ?assertEqual([], collect_events(CallbackPid)),
    Stream6 = fxml_stream:parse(Stream5, <<"</root>">>),
    ?assertEqual([{xmlstreamend, <<"root">>}], collect_events(CallbackPid)),
    close(Stream6).

map_stream_test() ->
	parser_loop(infinity, [use_maps], [
		{<<"<a xmlns='ns1'><b/>">>,
			[#{'__struct__' => 'Elixir.FastXML.StreamStart',
                        attrs => #{},
                        name => <<"a">>},
                      #{'__struct__' => 'Elixir.FastXML.El',
                        attrs => #{},
                        children => [],
                        name => <<"b">>}]},
			reset,
			{<<"<a><b c='1'/></a>">>,
				[#{'__struct__' => 'Elixir.FastXML.StreamStart',
                        attrs => #{},
                        name => <<"a">>},
                      #{'__struct__' => 'Elixir.FastXML.El',
                        attrs => #{<<"c">> => <<"1">>},
                        children => [],
                        name => <<"b">>},
                      #{'__struct__' => 'Elixir.FastXML.StreamEnd',
                        name => <<"a">>}]}
	]).

streaming_mismatched_tags_error_test() ->
    parser_loop([{<<"<root">>, []},
		 {<<"><a>">>, [{xmlstreamstart, <<"root">>, []}]},
		 {<<"</a><b/><c attr=">>,
		  [{xmlstreamelement, #xmlel{name = <<"a">>}},
		   {xmlstreamelement, #xmlel{name = <<"b">>}}]},
		 {<<"'1'></c>">>,
		  [{xmlstreamelement, #xmlel{name = <<"c">>,
					     attrs = [{<<"attr">>, <<"1">>}]}}]},
		 {<<"<z><a><b><d/></q><a/>">>,
		  [{xmlstreamerror,{7,<<"mismatched tag">>}}]}
		]).

streaming_invalid_attribute_error_test() ->
	parser_loop([{<<"<root">>, []},
		{<<"><a>">>, [{xmlstreamstart, <<"root">>, []}]},
		{<<"</a><b/><c attr=">>,
			[{xmlstreamelement, #xmlel{name = <<"a">>}},
				{xmlstreamelement, #xmlel{name = <<"b">>}}]},
		{<<"'1><d/></c>">>,
			[{xmlstreamerror,{4,<<"not well-formed (invalid token)">>}}]}
	]).

xmpp_stream_test() ->
    parser_loop([{<<"<?xml version='1.0'?>",
		    "<stream:stream to='server.com' ",
		    "version='1.0' xmlns:stream='http://etherx.jabber.org/streams' ",
		    "xml:lang='pl' >">>,
		  [{xmlstreamstart,<<"stream:stream">>,
		    [{<<"xmlns:stream">>,<<"http://etherx.jabber.org/streams">>},
		     {<<"to">>,<<"server.com">>},
		     {<<"version">>,<<"1.0">>},
		     {<<"xml:lang">>, <<"pl">>}]}]},
		 {<<"<stream:features>",
		    "<register xmlns='http://jabber.org/features/iq-register'/>",
		    "</stream:features>">>,
		  [{xmlstreamelement,
		    {xmlel,<<"stream:features">>,
		     [],
		     [{xmlel,<<"register">>,
		       [{<<"xmlns">>,<<"http://jabber.org/features/iq-register">>}],
		       []}]}}]},
		 {<<"<a:a xmlns:a='b'><a:c/></a:a>">>,
		  [{xmlstreamelement,
		    {xmlel,<<"a">>,
		     [{<<"xmlns">>,<<"b">>}, {<<"xmlns:a">>,<<"b">>}],
		     [{xmlel,<<"c">>,[], []}]}}]},
		 reset,
		 {<<"<?xml version='1.0'?>",
		    "<stream:stream xmlns='jabber:client' to='server.com' ",
		    "version='1.0' xmlns:stream='http://etherx.jabber.org/streams' ",
		    "xml:lang='pl' >">>,
		  [{xmlstreamstart,<<"stream:stream">>,
		    [{<<"xmlns">>,<<"jabber:client">>},
		     {<<"xmlns:stream">>,<<"http://etherx.jabber.org/streams">>},
		     {<<"to">>,<<"server.com">>},
		     {<<"version">>,<<"1.0">>},
		     {<<"xml:lang">>, <<"pl">>}]}]},
			reset,
			{<<"<?xml version='1.0'?>",
				"<stream:stream to='server.com' ",
				"version='1.0' xmlns:stream='http://etherx.jabber.org/streams' xmlns='jabber:client' ",
				"xml:lang='pl' >">>,
				[{xmlstreamstart, <<"stream:stream">>,
					[{<<"xmlns:stream">>, <<"http://etherx.jabber.org/streams">>},
						{<<"xmlns">>, <<"jabber:client">>},
						{<<"to">>, <<"server.com">>},
						{<<"version">>, <<"1.0">>},
						{<<"xml:lang">>, <<"pl">>}]}]}
		]).

too_big_test() ->
    CallbackPid = spawn_link(fun() -> receiver([]) end),
    Stream0 = new(CallbackPid, 5),
    Stream1 = fxml_stream:parse(Stream0, <<"<a>">>),
    Stream2 = fxml_stream:parse(Stream1, <<"<b/>">>),
    Stream3 = fxml_stream:parse(Stream2, <<"<c/>">>),
    Stream4 = fxml_stream:parse(Stream3, <<"<de/>">>),
    ?assertEqual([{xmlstreamstart, <<"a">>, []},
		  {xmlstreamelement, #xmlel{name = <<"b">>}},
		  {xmlstreamelement, #xmlel{name = <<"c">>}},
		  {xmlstreamerror, <<"XML stanza is too big">>}],
		 collect_events(CallbackPid)),
    close(Stream4).

too_big_with_data_after_test() ->
    parser_loop(30, [],
		[{
		   <<"<start>">>,
		   [{xmlstreamstart, <<"start">>, []}]
		 },
		 {
		   <<"<foo><a t='1' ">>,
		   []
		 },
		 {
		   <<"<1234567890123456790123456789012345678901234567890/>">>,
		   [{xmlstreamerror, <<"XML stanza is too big">>}]
		 },
		 {
		   <<"z='1'/>">>,
		   [{xmlstreamerror, <<"XML stanza is too big">>}]
		 },
		 {
		   <<"</foo></start>">>,
		   [{xmlstreamerror, <<"XML stanza is too big">>}]
		 }]).

close_close_test() ->
    Stream = new(),
    close(Stream),
    ?assertError(badarg, fxml_stream:close(Stream)).

close_parse_test() ->
    Stream = new(),
    close(Stream),
    ?assertError(badarg, fxml_stream:parse(Stream, <<"junk">>)).

close_change_callback_pid_test() ->
    Stream = new(),
    close(Stream),
    ?assertError(badarg, fxml_stream:change_callback_pid(Stream, self())).

change_callback_pid_test() ->
    Pid1 = spawn_link(fun() -> receiver([]) end),
    Pid2 = spawn_link(fun() -> receiver([]) end),
    Stream0 = new(Pid1),
    Stream1 = fxml_stream:parse(Stream0, <<"<root>">>),
    ?assertEqual([{xmlstreamstart, <<"root">>, []}],
		 collect_events(Pid1)),
    Stream2 = fxml_stream:change_callback_pid(Stream1, Pid2),
    Stream3 = fxml_stream:parse(Stream2, <<"</root>">>),
    ?assertEqual([{xmlstreamend, <<"root">>}],
		 collect_events(Pid2)),
    close(Stream3).

badarg_new_test() ->
    ?assertError(badarg, fxml_stream:new(1)),
    ?assertError(badarg, fxml_stream:new(self(), unlimited)),
    ?assertError(badarg, fxml_stream:new(foo, fun() -> ok end)).

badarg_parse_test() ->
    Stream = new(),
    ?assertError(badarg, fxml_stream:parse(1, <<"<root>">>)),
    ?assertError(badarg, fxml_stream:parse(<<>>, "<root>")),
    ?assertError(badarg, fxml_stream:parse(Stream, blah)),
    ?assertError(badarg, fxml_stream:parse(foo, fun() -> ok end)),
    close(Stream).

badarg_change_callback_pid_test() ->
    Stream = new(),
    ?assertError(badarg, fxml_stream:change_callback_pid(1, self())),
    ?assertError(badarg, fxml_stream:change_callback_pid(<<>>, self())),
    ?assertError(badarg, fxml_stream:change_callback_pid(Stream, foo)),
    ?assertError(badarg, fxml_stream:change_callback_pid(foo, fun() -> ok end)),
    close(Stream).

badarg_close_test() ->
    Stream = new(),
    ?assertError(badarg, fxml_stream:close(1)),
    ?assertError(badarg, fxml_stream:close(<<>>)),
    close(Stream).

badarg_parse_element_test() ->
    ?assertError(badarg, fxml_stream:parse_element(1)).

parse_error_test() ->
    L = ["<", "<>", "</>", "</>", "/>",
	 "<x", "<x>", "x/>", "<x/>junk",
	 "<x a/>", "<x a=f/>", "<x a='5'>",
	 "<x:y/>", "<x y:a='1'/>"],
    lists:foreach(
      fun(S) ->
	      ?assertMatch({error, _}, fxml_stream:parse_element(list_to_binary(S)))
      end, L).

dead_pid_test() ->
    CallbackPid = spawn(fun() -> receiver([]) end),
    ?assertEqual(true, exit(CallbackPid, kill)),
    ?assertEqual(false, is_process_alive(CallbackPid)),
    Stream0 = new(CallbackPid),
    Stream1 = fxml_stream:parse(Stream0, <<"<root><a/></root>">>),
    close(Stream1).

huge_element_test_() ->
    {timeout, 60,
     fun() ->
	     Tags = [list_to_binary(["a", integer_to_list(I)])
		     || I <- lists:seq(1, 100000)],
	     Data = ["<root>", [[$<, Tag, "/>"] || Tag <- Tags], "</root>"],
	     Els = #xmlel{name = <<"root">>,
			  children = [#xmlel{name = Tag} || Tag <- Tags]},
	     ?assertEqual(Els, fxml_stream:parse_element(iolist_to_binary(Data)))
     end}.

many_stream_elements_test_() ->
    {timeout, 60,
     fun() ->
	     CallbackPid = spawn(fun() -> receiver([]) end),
	     Stream0 = new(CallbackPid),
	     Stream1 = fxml_stream:parse(Stream0, <<"<root>">>),
	     ?assertEqual([{xmlstreamstart, <<"root">>, []}],
			  collect_events(CallbackPid)),
	     Stream2 = lists:foldl(
			 fun(I, Stream) ->
				 Tag = list_to_binary(["a", integer_to_list(I)]),
				 NewStream = fxml_stream:parse(Stream, iolist_to_binary([$<, Tag, "/>"])),
				 ?assertEqual([{xmlstreamelement, #xmlel{name = Tag}}],
					      collect_events(CallbackPid)),
				 NewStream
			 end, Stream1, lists:seq(1, 100000)),
	     close(Stream2)
     end}.

billionlaughs_test() ->
    Data = <<
	"<?xml version='1.0'?><!DOCTYPE test [ <!ENTITY lol \"lol\"> ",
	"<!ENTITY lol1 \"&lol;&lol;&lol;&lol;&lol;&lol;&lol;&lol;&lol;&lol;\"> ",
	"<!ENTITY lol2 \"&lol1;&lol1;&lol1;&lol1;&lol1;&lol1;&lol1;&lol1;&lol1;&lol1;\"> ",
	"<!ENTITY lol3 \"&lol2;&lol2;&lol2;&lol2;&lol2;&lol2;&lol2;&lol2;&lol2;&lol2;\"> ",
	"<!ENTITY lol4 \"&lol3;&lol3;&lol3;&lol3;&lol3;&lol3;&lol3;&lol3;&lol3;&lol3;\"> ",
	"<!ENTITY lol5 \"&lol4;&lol4;&lol4;&lol4;&lol4;&lol4;&lol4;&lol4;&lol4;&lol4;\"> ",
	"<!ENTITY lol6 \"&lol5;&lol5;&lol5;&lol5;&lol5;&lol5;&lol5;&lol5;&lol5;&lol5;\"> ",
	"<!ENTITY lol7 \"&lol6;&lol6;&lol6;&lol6;&lol6;&lol6;&lol6;&lol6;&lol6;&lol6;\"> ",
	"<!ENTITY lol8 \"&lol7;&lol7;&lol7;&lol7;&lol7;&lol7;&lol7;&lol7;&lol7;&lol7;\"> ",
	"<!ENTITY lol9 \"&lol8;&lol8;&lol8;&lol8;&lol8;&lol8;&lol8;&lol8;&lol8;&lol8;\"> ",
	"] ><stream:stream xmlns='jabber:client' ",
	"xmlns:stream='http://etherx.jabber.org/streams' version='1.0' to='&lol9;'>\n">>,
    CallbackPid = spawn_link(fun() -> receiver([]) end),
    Stream0 = new(CallbackPid),
    Stream1 = fxml_stream:parse(Stream0, Data),
    close(Stream1),
    ?assertMatch([{xmlstreamerror, _}], collect_events(CallbackPid)).

element_to_binary_entities_test() ->
    S = <<"<a b='a&#x9;&#xA;&#xD;&lt;>&amp;&quot;&apos;b'>a&lt;&gt;&amp;&quot;&apos;b</a>">>,
    E = #xmlel{name = <<"a">>,
	       attrs = [{<<"b">>, <<"a\t\n\r<>&\"'b">>}],
	       children = [{xmlcdata, <<"a<>&\"'b">>}]},
    R = fxml:element_to_binary(E),
    ?assertEqual(S, R),
    ?assertEqual(E, fxml_stream:parse_element(R)).

element_to_binary_resize_test() ->
    A = #xmlel{
	   name = <<"a">>,
	   children = [{xmlcdata, <<"1234567890123456790123456789012345678901234567890">>}]
	  },

    ?assertEqual(
       <<"<t>",
	 "<a>1234567890123456790123456789012345678901234567890</a>"
	 "<a>1234567890123456790123456789012345678901234567890</a>"
	 "<a>1234567890123456790123456789012345678901234567890</a>"
	 "<a>1234567890123456790123456789012345678901234567890</a>"
	 "<a>1234567890123456790123456789012345678901234567890</a>"
	 "<a>1234567890123456790123456789012345678901234567890</a>"
	 "<a>1234567890123456790123456789012345678901234567890</a>"
	 "<a>1234567890123456790123456789012345678901234567890</a>"
	 "<a>1234567890123456790123456789012345678901234567890</a>"
	 "<a>1234567890123456790123456789012345678901234567890</a>"
	 "<a>1234567890123456790123456789012345678901234567890</a>"
	 "<a>1234567890123456790123456789012345678901234567890</a>"
	 "<a>1234567890123456790123456789012345678901234567890</a>"
	 "<a>1234567890123456790123456789012345678901234567890</a>"
	 "<a>1234567890123456790123456789012345678901234567890</a>"
	 "<a>1234567890123456790123456789012345678901234567890</a>"
	 "<a>1234567890123456790123456789012345678901234567890</a>"
	 "<a>1234567890123456790123456789012345678901234567890</a>"
	 "<a>1234567890123456790123456789012345678901234567890</a>"
	 "<a>1234567890123456790123456789012345678901234567890</a>"
	 "</t>"
       >>,
       fxml:element_to_binary(
	 #xmlel{name = <<"t">>,
		children = [A, A, A, A, A,
			    A, A, A, A, A,
			    A, A, A, A, A,
			    A, A, A, A, A]
	       })).

element_to_binary_test() ->
    ?assertEqual(
       <<"<iq from='hag66@shakespeare.lit/pda' id='ik3vs715' "
	 "to='coven@chat.shakespeare.lit' type='get'>"
	 "<query xmlns='http://jabber.org/protocol/disco#info'/></iq>">>,
       fxml:element_to_binary(
	 #xmlel{name = <<"iq">>,
		attrs = [{<<"from">>,<<"hag66@shakespeare.lit/pda">>},
			 {<<"id">>,<<"ik3vs715">>},
			 {<<"to">>,<<"coven@chat.shakespeare.lit">>},
			 {<<"type">>,<<"get">>}],
		children = [#xmlel{name = <<"query">>,
				   attrs = [{<<"xmlns">>,
					     <<"http://jabber.org/protocol/disco#info">>}],
				   children = []}]})).

element_to_header_test() ->
    ?assertEqual(
       <<"<?xml version='1.0'?><stream:stream "
	 "xmlns:stream='http://etherx.jabber.org/streams' "
	 "xmlns='jabber:server' xmlns:db='jabber:server:dialback' "
	 "id='4774242664715222330' version='1.0'>">>,
       fxml:element_to_header(
	 #xmlel{name = <<"stream:stream">>,
		attrs = [{<<"xmlns:stream">>,<<"http://etherx.jabber.org/streams">>},
			 {<<"xmlns">>,<<"jabber:server">>},
			 {<<"xmlns:db">>,<<"jabber:server:dialback">>},
			 {<<"id">>,<<"4774242664715222330">>},
			 {<<"version">>,<<"1.0">>}]})).

element_to_header_xmlcdata_test() ->
    ?assertError(badarg, fxml:element_to_header({xmlcdata, <<"">>})).

crypt_test() ->
    ?assertEqual(
       <<"a&amp;b&lt;c&gt;d&quot;e&apos;f">>,
       fxml:crypt(<<"a&b<c>d\"e\'f">>)).

remove_cdata_test() ->
    ?assertEqual(
       [#xmlel{name = <<"b">>}],
       fxml:remove_cdata(
	 [{xmlcdata, <<"x">>},
	  {xmlcdata, <<"y">>},
	  #xmlel{name = <<"b">>},
	  {xmlcdata, <<"z">>}])).

remove_subtags_test() ->
    ?assertMatch(
       #xmlel{name = <<"root">>,
	      children = [#xmlel{name = <<"2">>,
				 attrs = [{<<"n1">>, <<"v1">>}]},
			  #xmlel{name = <<"1">>,
				 attrs = [{<<"n1">>, <<"v2">>}]},
			  #xmlel{name = <<"1">>,
				 attrs = [{<<"n2">>, <<"v1">>}]},
			  #xmlel{name = <<"3">>}]},
       fxml:remove_subtags(
	 #xmlel{name = <<"root">>,
		children = [#xmlel{name = <<"1">>,
				   attrs = [{<<"n1">>, <<"v1">>}]},
			    #xmlel{name = <<"2">>,
				   attrs = [{<<"n1">>, <<"v1">>}]},
			    #xmlel{name = <<"1">>,
				   attrs = [{<<"n1">>, <<"v2">>}]},
			    #xmlel{name = <<"1">>,
				   attrs = [{<<"n2">>, <<"v1">>}]},
			    #xmlel{name = <<"1">>,
				   attrs = [{<<"n1">>, <<"v1">>}]},
			    #xmlel{name = <<"3">>}]},
	 <<"1">>, {<<"n1">>, <<"v1">>})).

get_cdata_test() ->
    ?assertEqual(
       <<"xyz">>,
       fxml:get_cdata(
	 [{xmlcdata, <<"x">>},
	  {xmlcdata, <<"y">>},
	  #xmlel{name = <<"b">>},
	  {xmlcdata, <<"z">>}])).

get_tag_cdata_test() ->
    ?assertEqual(
       <<"xyz">>,
       fxml:get_tag_cdata(
	 #xmlel{name = <<"a">>,
		children = [{xmlcdata, <<"x">>},
			    {xmlcdata, <<"y">>},
			    #xmlel{name = <<"b">>},
			    {xmlcdata, <<"z">>}]})).

get_attr_test() ->
    ?assertEqual(
       {value, <<"2">>},
       fxml:get_attr(
	 <<"y">>,
	 [{<<"x">>, <<"1">>},
	  {<<"y">>, <<"2">>},
	  {<<"z">>, <<"3">>}])).

get_attr_empty_test() ->
    ?assertEqual(
       false,
       fxml:get_attr(
	 <<"a">>,
	 [{<<"x">>, <<"1">>},
	  {<<"y">>, <<"2">>},
	  {<<"z">>, <<"3">>}])).

get_attr_s_test() ->
    ?assertEqual(
       <<"2">>,
       fxml:get_attr_s(
	 <<"y">>,
	 [{<<"x">>, <<"1">>},
	  {<<"y">>, <<"2">>},
	  {<<"z">>, <<"3">>}])).

get_attr_s_empty_test() ->
    ?assertEqual(
       <<"">>,
       fxml:get_attr_s(
	 <<"a">>,
	 [{<<"x">>, <<"1">>},
	  {<<"y">>, <<"2">>},
	  {<<"z">>, <<"3">>}])).

get_tag_attr_test() ->
    ?assertEqual(
       {value, <<"2">>},
       fxml:get_tag_attr(
	 <<"y">>,
	 #xmlel{name = <<"foo">>,
		attrs = [{<<"x">>, <<"1">>},
			 {<<"y">>, <<"2">>},
			 {<<"z">>, <<"3">>}]})).

get_tag_attr_empty_test() ->
    ?assertEqual(
       false,
       fxml:get_tag_attr(
	 <<"a">>,
	 #xmlel{name = <<"foo">>,
		attrs = [{<<"x">>, <<"1">>},
			 {<<"y">>, <<"2">>},
			 {<<"z">>, <<"3">>}]})).

get_tag_attr_s_test() ->
    ?assertEqual(
       <<"2">>,
       fxml:get_tag_attr_s(
	 <<"y">>,
	 #xmlel{name = <<"foo">>,
		attrs = [{<<"x">>, <<"1">>},
			 {<<"y">>, <<"2">>},
			 {<<"z">>, <<"3">>}]})).

get_tag_attr_s_empty_test() ->
    ?assertEqual(
       <<"">>,
       fxml:get_tag_attr_s(
	 <<"a">>,
	 #xmlel{name = <<"foo">>,
		attrs = [{<<"x">>, <<"1">>},
			 {<<"y">>, <<"2">>},
			 {<<"z">>, <<"3">>}]})).

get_subtag_test() ->
    ?assertMatch(
       #xmlel{name = <<"2">>},
       fxml:get_subtag(
	 #xmlel{name = <<"root">>,
		children = [#xmlel{name = <<"1">>},
			    #xmlel{name = <<"2">>},
			    #xmlel{name = <<"3">>}]},
	 <<"2">>)).

get_subtag_false_test() ->
    ?assertMatch(
       false,
       fxml:get_subtag(
	 #xmlel{name = <<"root">>,
		children = [#xmlel{name = <<"1">>},
			    #xmlel{name = <<"2">>},
			    #xmlel{name = <<"3">>}]},
	 <<"4">>)).

get_subtags_test() ->
    ?assertMatch(
       [#xmlel{name = <<"1">>, attrs = [{<<"a">>, <<"b">>}]},
	#xmlel{name = <<"1">>, attrs = [{<<"x">>, <<"y">>}]}],
       fxml:get_subtags(
	 #xmlel{name = <<"root">>,
		children = [#xmlel{name = <<"1">>,
				   attrs = [{<<"a">>, <<"b">>}]},
			    #xmlel{name = <<"2">>},
			    #xmlel{name = <<"3">>},
			    #xmlel{name = <<"1">>,
				   attrs = [{<<"x">>, <<"y">>}]}]},
	 <<"1">>)).

get_subtags_empty_test() ->
    ?assertEqual(
       [],
       fxml:get_subtags(
	 #xmlel{name = <<"root">>,
		children = [#xmlel{name = <<"1">>},
			    #xmlel{name = <<"2">>},
			    #xmlel{name = <<"3">>}]},
	 <<"4">>)).

get_subtag_with_xmlns_test() ->
    ?assertMatch(
       #xmlel{name = <<"2">>,
	      attrs = [{<<"xmlns">>, <<"ns1">>}]},
       fxml:get_subtag_with_xmlns(
	 #xmlel{name = <<"root">>,
		children = [#xmlel{name = <<"1">>,
				   attrs = [{<<"xmlns">>, <<"ns1">>}]},
			    #xmlel{name = <<"2">>,
				   attrs = [{<<"xmlns">>, <<"ns2">>}]},
			    #xmlel{name = <<"2">>,
				   attrs = [{<<"xmlns">>, <<"ns1">>}]},
			    #xmlel{name = <<"3">>,
				   attrs = [{<<"xmlns">>, <<"ns2">>}]}]},
	 <<"2">>, <<"ns1">>)).

get_subtag_with_xmlns_empty_test() ->
    ?assertMatch(
       false,
       fxml:get_subtag_with_xmlns(
	 #xmlel{name = <<"root">>,
		children = [#xmlel{name = <<"1">>,
				   attrs = [{<<"xmlns">>, <<"ns1">>}]},
			    #xmlel{name = <<"2">>,
				   attrs = [{<<"xmlns">>, <<"ns2">>}]},
			    #xmlel{name = <<"2">>,
				   attrs = [{<<"xmlns">>, <<"ns1">>}]},
			    #xmlel{name = <<"3">>,
				   attrs = [{<<"xmlns">>, <<"ns2">>}]}]},
	 <<"4">>, <<"ns2">>)).

get_subtags_with_xmlns_test() ->
    ?assertMatch(
       [#xmlel{name = <<"2">>,
	       attrs = [{<<"xmlns">>, <<"ns1">>}],
	       children = [{xmlcdata, <<"foo">>}]},
	#xmlel{name = <<"2">>,
	       attrs = [{<<"xmlns">>, <<"ns1">>}],
	       children = [{xmlcdata, <<"bar">>}]}],
       fxml:get_subtags_with_xmlns(
	 #xmlel{name = <<"root">>,
		children = [#xmlel{name = <<"1">>,
				   attrs = [{<<"xmlns">>, <<"ns1">>}]},
			    #xmlel{name = <<"2">>,
				   children = [{xmlcdata, <<"foo">>}],
				   attrs = [{<<"xmlns">>, <<"ns1">>}]},
			    #xmlel{name = <<"2">>,
				   attrs = [{<<"xmlns">>, <<"ns2">>}]},
			    #xmlel{name = <<"2">>,
				   children = [{xmlcdata, <<"bar">>}],
				   attrs = [{<<"xmlns">>, <<"ns1">>}]},
			    #xmlel{name = <<"3">>,
				   attrs = [{<<"xmlns">>, <<"ns2">>}]}]},
	 <<"2">>, <<"ns1">>)).

get_subtag_cdata_test() ->
    ?assertEqual(
       <<"ab">>,
       fxml:get_subtag_cdata(
	 #xmlel{name = <<"root">>,
		children = [#xmlel{name = <<"1">>,
				   children = [{xmlcdata, <<"a">>},
					       #xmlel{name = <<"3">>},
					       {xmlcdata, <<"b">>}]},
			    #xmlel{name = <<"2">>}]},
	 <<"1">>)).

get_subtag_cdata_empty_test() ->
    ?assertEqual(
       <<"">>,
       fxml:get_subtag_cdata(
	 #xmlel{name = <<"root">>,
		children = [#xmlel{name = <<"2">>}]},
	 <<"1">>)).

append_subtags_test() ->
    ?assertMatch(
       #xmlel{name = <<"root">>,
	      children = [#xmlel{name = <<"1">>},
			  #xmlel{name = <<"2">>},
			  #xmlel{name = <<"3">>}]},
       fxml:append_subtags(
	 #xmlel{name = <<"root">>,
		children = [#xmlel{name = <<"1">>}]},
	 [#xmlel{name = <<"2">>}, #xmlel{name = <<"3">>}])).

get_path_s_tag_test() ->
    ?assertMatch(
       #xmlel{name = <<"2">>},
       fxml:get_path_s(
	 #xmlel{name = <<"root">>,
		children = [#xmlel{name = <<"1">>},
			    #xmlel{name = <<"2">>}]},
	 [{elem, <<"2">>}])).

get_path_s_empty_tag_test() ->
    ?assertEqual(
       <<"">>,
       fxml:get_path_s(
	 #xmlel{name = <<"root">>,
		children = [#xmlel{name = <<"1">>},
			    #xmlel{name = <<"2">>}]},
	 [{elem, <<"3">>}])).

get_path_s_attr_test() ->
    ?assertEqual(
       <<"v1">>,
       fxml:get_path_s(
	 #xmlel{name = <<"root">>,
		children = [#xmlel{name = <<"a">>,
				   children =
				       [#xmlel{name = <<"a1">>,
					       attrs = [{<<"x">>, <<"y">>},
							{<<"n1">>, <<"v1">>}]},
					#xmlel{name = <<"b">>}]},
			    #xmlel{name = <<"b">>}]},
	 [{elem, <<"a">>}, {elem, <<"a1">>}, {attr, <<"n1">>}])).

get_path_s_cdata_test() ->
    ?assertEqual(
       <<"d1">>,
       fxml:get_path_s(
	 #xmlel{name = <<"root">>,
		children = [#xmlel{name = <<"a">>,
				   children = [#xmlel{name = <<"a1">>},
					       {xmlcdata, <<"d1">>}]},
			    #xmlel{name = <<"b">>}]},
	 [{elem, <<"a">>}, cdata])).

replace_tag_attr_test() ->
    ?assertMatch(
       #xmlel{name = <<"foo">>,
	      attrs = [{<<"2">>, <<"d">>},
		       {<<"1">>, <<"a">>},
		       {<<"2">>, <<"c">>}]},
       fxml:replace_tag_attr(
	 <<"2">>, <<"d">>,
	 #xmlel{name = <<"foo">>,
		attrs = [{<<"1">>, <<"a">>},
			 {<<"2">>, <<"b">>},
			 {<<"2">>, <<"c">>}]})).

replace_subtag_test() ->
    ?assertMatch(
       #xmlel{name = <<"root">>,
	      children = [#xmlel{name = <<"2">>, children = []},
			  #xmlel{name = <<"1">>},
			  #xmlel{name = <<"2">>,
				 children = [{xmlcdata, <<"b">>}]}]},
       fxml:replace_subtag(
	 #xmlel{name = <<"2">>},
	 #xmlel{name = <<"root">>,
		children = [#xmlel{name = <<"1">>},
			    #xmlel{name = <<"2">>,
				   children = [{xmlcdata, <<"a">>}]},
			    #xmlel{name = <<"2">>,
				   children =[{xmlcdata, <<"b">>}]}]})).

to_xmlel_test() ->
    ?assertEqual(
       #xmlel{name = <<"foo">>,
	      attrs = [{<<"a">>, <<"b">>}],
	      children = [{xmlcdata, <<"xyz">>}]},
       fxml:to_xmlel({xmlelement, "foo", [{"a", "b"}], [{xmlcdata, "xyz"}]})).

rpc_fault_test() ->
    Fault = {xmlel,<<"methodResponse">>,[],
	     [{xmlel,<<"fault">>,[],
	       [{xmlel,<<"value">>,[],
		 [{xmlel,<<"struct">>,[],
		   [{xmlel,<<"member">>,[],
		     [{xmlel,<<"name">>,[],[{xmlcdata,<<"faultCode">>}]},
		      {xmlel,<<"value">>,[],
		       [{xmlel,<<"int">>,[],[{xmlcdata,<<"4">>}]}]}]},
		    {xmlel,<<"member">>,[],
		     [{xmlel,<<"name">>,[],[{xmlcdata,<<"faultString">>}]},
		      {xmlel,<<"value">>,[],
		       [{xmlel,<<"string">>,[],
			 [{xmlcdata,<<"Too many parameters.">>}]}]}]}]}]}]}]},
    Result = {response, {fault, 4, <<"Too many parameters.">>}},
    ?assertEqual({ok, Result}, fxmlrpc:decode(Fault)),
    ?assertEqual(Fault, fxmlrpc:encode(Result)).

rpc_call_test() ->
    Call = {xmlel,<<"methodCall">>,[],
	    [{xmlel,<<"methodName">>,[],[{xmlcdata,<<"examples.getStateName">>}]},
	     {xmlel,<<"params">>,[],
	      [{xmlel,<<"param">>,[],
		[{xmlel,<<"value">>,[],
		  [{xmlel,<<"int">>,[],[{xmlcdata,<<"40">>}]}]}]},
	       {xmlel,<<"param">>,[],
		[{xmlel,<<"value">>,[],
		  [{xmlel,<<"int">>,[],[{xmlcdata,<<"30">>}]}]}]},
	       {xmlel,<<"param">>,[],
		[{xmlel,<<"value">>,[],
		  [{xmlel,<<"boolean">>,[],[{xmlcdata,<<"0">>}]}]}]},
	       {xmlel,<<"param">>,[],
		[{xmlel,<<"value">>,[],
		  [{xmlel,<<"base64">>,[],
		    [{xmlcdata,<<"eW91IGNhbid0IHJlYWQgdGhpcyE=">>}]}]}]},
	       {xmlel,<<"param">>,[],
		[{xmlel,<<"value">>,[],
		  [{xmlel,<<"dateTime.iso8601">>,[],
		    [{xmlcdata,<<"19980717T14:08:55">>}]}]}]},
	       {xmlel,<<"param">>,[],
		[{xmlel,<<"value">>,[],
		  [{xmlel,<<"string">>,[],
		    [{xmlcdata,<<"Hello world!">>}]}]}]},
	       {xmlel,<<"param">>,[],
		[{xmlel,<<"value">>,[],[{xmlel,<<"nil">>,[],[]}]}]},
	       {xmlel,<<"param">>,[],
		[{xmlel,<<"value">>,[],
		  [{xmlel,<<"array">>,[],
		    [{xmlel,<<"data">>,[],
		      [{xmlel,<<"value">>,[],
			[{xmlel,<<"int">>,[],
			  [{xmlcdata,<<"1404">>}]}]},
		       {xmlel,<<"value">>,[],
			[{xmlel,<<"string">>,[],
			  [{xmlcdata,<<"Something here">>}]}]},
		       {xmlel,<<"value">>,[],
			[{xmlel,<<"int">>,[],
			  [{xmlcdata,<<"1">>}]}]}]}]}]}]},
	       {xmlel,<<"param">>,[],
		[{xmlel,<<"value">>,[],
		  [{xmlel,<<"struct">>,[],
		    [{xmlel,<<"member">>,[],
		      [{xmlel,<<"name">>,[],[{xmlcdata,<<"foo">>}]},
		       {xmlel,<<"value">>,[],
			[{xmlel,<<"int">>,[],
			  [{xmlcdata,<<"1">>}]}]}]},
		     {xmlel,<<"member">>,[],
		      [{xmlel,<<"name">>,[],[{xmlcdata,<<"bar">>}]},
		       {xmlel,<<"value">>,[],
			[{xmlel,<<"int">>,[],
			  [{xmlcdata,<<"2">>}]}]}]}]}]}]}]}]},
    Result = {call,'examples.getStateName',
	      [40,30,false,
	       {base64,<<"eW91IGNhbid0IHJlYWQgdGhpcyE=">>},
	       {date,<<"19980717T14:08:55">>},
	       <<"Hello world!">>,nil,
	       {array,[1404,<<"Something here">>,1]},
	       {struct,[{foo,1},{bar,2}]}]},
    ?assertEqual({ok, Result}, fxmlrpc:decode(Call)),
    ?assertEqual(Call, fxmlrpc:encode(Result)).

response_test() ->
    Response = {xmlel,<<"methodResponse">>, [],
		[{xmlel,<<"params">>,[],
		  [{xmlel,<<"param">>,[],
		    [{xmlel,<<"value">>,[],
		      [{xmlel,<<"string">>,[],
			[{xmlcdata,<<"South Dakota">>}]}]}]}]}]},
    Result = {response,[<<"South Dakota">>]},
    ?assertEqual({ok, Result}, fxmlrpc:decode(Response)),
    ?assertEqual(Response, fxmlrpc:encode(Result)).

rpc_empty_call_test() ->
    Call = {xmlel,<<"methodCall">>,[],
	    [{xmlel,<<"methodName">>,[],
	      [{xmlcdata,<<"some_method">>}]}]},
    Result = {call, some_method, []},
    ?assertEqual({ok, Result}, fxmlrpc:decode(Call)),
    ?assertEqual(Call, fxmlrpc:encode(Result)).

rpc_empty_response_test() ->
    Response = {xmlel,<<"methodResponse">>, [], []},
    Result = {response, []},
    ?assertEqual({ok, Result}, fxmlrpc:decode(Response)),
    ?assertEqual(Response, fxmlrpc:encode(Result)).
