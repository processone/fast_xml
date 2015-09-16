%%%-------------------------------------------------------------------
%%% File    : p1_xmlrpc.erl
%%% Author  : Evgeny Khramtsov <ekhramtsov@process-one.net>
%%% Purpose : XMLRPC encoder/decoder
%%% Created : 3 Oct 2014 by Evgeny Khramtsov <ekhramtsov@process-one.net>
%%%
%%%
%%% xml, Copyright (C) 2002-2015   ProcessOne
%%%
%%% This program is free software; you can redistribute it and/or
%%% modify it under the terms of the GNU General Public License as
%%% published by the Free Software Foundation; either version 2 of the
%%% License, or (at your option) any later version.
%%%
%%% This program is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
%%% General Public License for more details.
%%%
%%% You should have received a copy of the GNU General Public License
%%% along with this program; if not, write to the Free Software
%%% Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
%%% 02111-1307 USA
%%%
%%%-------------------------------------------------------------------
-module(p1_xmlrpc).

%% API
-export([decode/1, encode/1]).

-include("xml.hrl").

-type value() :: number() | binary() | boolean() | nil |
		 {base64, binary()} |
		 {date, binary()} |
		 {array, [{atom(), value()}]} |
		 {struct, [value()]}.

-type fault() :: {fault, integer(), binary()}.
-type call() :: {call, atom(), [value()]}.
-type response() :: {response, [value()] | fault()}.

%%%===================================================================
%%% API
%%%===================================================================
-spec decode(xmlel()) -> {ok, call()} | {ok, response()} | {error, any()}.

decode(El) ->
    try xmlrpc_codec:decode(El) of
	{call, Name, Params} ->
	    {ok, {call, Name, [decode_param(Param) || Param <- Params]}};
	{response, Params} when is_list(Params) ->
	    {ok, {response, [decode_param(Param) || Param <- Params]}};
	{response, {fault, {{struct, Struct}, _}}} ->
	    case proplists:get_value(faultCode, Struct) of
		{{Tag, Code}, _} when Tag == int; Tag == i4 ->
		    case  proplists:get_value(faultString, Struct) of
			{{string, String}, _} ->
			    {ok, {response, {fault, Code, String}}};
			{undefined, undefined} ->
			    {ok, {response, {fault, Code, <<"">>}}};
			{undefined, String} when is_binary(String) ->
			    {ok, {response, {fault, Code, String}}};
			_ ->
			    {error, {bad_struct, Struct}}
		    end;
		_ ->
		    {error, {bad_struct, Struct}}
	    end;
	Other ->
	    {error, {unexpected_element, Other}}
    catch error:{xmlrpc_codec, Reason} ->
	    {error, Reason}
    end.

-spec encode(call() | response()) -> xmlel().

encode({call, Name, Params}) ->
    xmlrpc_codec:encode({call, Name, [encode_param(Param) || Param <- Params]});
encode({response, Params}) when is_list(Params) ->
    xmlrpc_codec:encode({response, [encode_param(Param) || Param <- Params]});
encode({response, {fault, Code, String}}) ->
    xmlrpc_codec:encode(
      {response, {fault, {{struct, [{faultCode, {{int, Code}, undefined}},
				    {faultString, {{string, String}, undefined}}]},
			  undefined}}}).

%%%===================================================================
%%% Internal functions
%%%===================================================================
decode_param({undefined, B}) when is_binary(B) ->
    B;
decode_param({undefined, undefined}) ->
    <<"">>;
decode_param({{int, Int}, _}) ->
    Int;
decode_param({{i4, Int}, _}) ->
    Int;
decode_param({{boolean, Bool}, _}) ->
    Bool;
decode_param({{string, S}, _}) ->
    S;
decode_param({{double, D}, _}) ->
    D;
decode_param({{array, L}, _}) ->
    {array, [decode_param(E) || E <- L]};
decode_param({{struct, S}, _}) ->
    {struct, [{Name, decode_param(Value)} || {Name, Value} <- S]};
decode_param({{base64, B64}, _}) ->
    {base64, B64};
decode_param({{date, Date}, _}) ->
    {date, Date};
decode_param({nil, _}) ->
    nil.

encode_param(Int) when is_integer(Int) ->
    {{int, Int}, undefined};
encode_param(B) when is_boolean(B) ->
    {{boolean, B}, undefined};
encode_param(S) when is_binary(S) ->
    {{string, S}, undefined};
encode_param(S) when is_list(S) ->
    {{string, iolist_to_binary(S)}, undefined};
encode_param(D) when is_float(D) ->
    {{double, D}, undefined};
encode_param({array, L}) ->
    {{array, [encode_param(E) || E <- L]}, undefined};
encode_param({struct, S}) ->
    {{struct, [{Name, encode_param(Value)} || {Name, Value} <- S]}, undefined};
encode_param({base64, B64}) ->
    {{base64, B64}, undefined};
encode_param({date, Date}) ->
    {{date, Date}, undefined};
encode_param(nil) ->
    {nil, undefined}.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

fault_test() ->
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
    ?assertEqual({ok, Result}, ?MODULE:decode(Fault)),
    ?assertEqual(Fault, ?MODULE:encode(Result)).

call_test() ->
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
    ?assertEqual({ok, Result}, ?MODULE:decode(Call)),
    ?assertEqual(Call, ?MODULE:encode(Result)).

response_test() ->
    Response = {xmlel,<<"methodResponse">>, [],
		[{xmlel,<<"params">>,[],
		  [{xmlel,<<"param">>,[],
		    [{xmlel,<<"value">>,[],
		      [{xmlel,<<"string">>,[],
			[{xmlcdata,<<"South Dakota">>}]}]}]}]}]},
    Result = {response,[<<"South Dakota">>]},
    ?assertEqual({ok, Result}, ?MODULE:decode(Response)),
    ?assertEqual(Response, ?MODULE:encode(Result)).

empty_call_test() ->
    Call = {xmlel,<<"methodCall">>,[],
	    [{xmlel,<<"methodName">>,[],
	      [{xmlcdata,<<"some_method">>}]}]},
    Result = {call, some_method, []},
    ?assertEqual({ok, Result}, ?MODULE:decode(Call)),
    ?assertEqual(Call, ?MODULE:encode(Result)).

empty_response_test() ->
    Response = {xmlel,<<"methodResponse">>, [], []},
    Result = {response, []},
    ?assertEqual({ok, Result}, ?MODULE:decode(Response)),
    ?assertEqual(Response, ?MODULE:encode(Result)).

-endif.
