%%%-------------------------------------------------------------------
%%% File    : fxmlrpc.erl
%%% Author  : Evgeny Khramtsov <ekhramtsov@process-one.net>
%%% Purpose : XMLRPC encoder/decoder
%%% Created : 3 Oct 2014 by Evgeny Khramtsov <ekhramtsov@process-one.net>
%%%
%%%
%%% Copyright (C) 2002-2024 ProcessOne, SARL. All Rights Reserved.
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
%%%-------------------------------------------------------------------
-module(fxmlrpc).

%% API
-export([decode/1, encode/1]).

-include("fxml.hrl").
-define(NS_XMLRPC, <<"xmlrpc">>).

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
    try fxmlrpc_codec:decode(El, ?NS_XMLRPC, []) of
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
    catch error:{fxmlrpc_codec, Reason} ->
	    {error, Reason}
    end.

-spec encode(call() | response()) -> xmlel().

encode({call, Name, Params}) ->
    fxmlrpc_codec:encode(
      {call, Name, [encode_param(Param) || Param <- Params]},
      ?NS_XMLRPC);
encode({response, Params}) when is_list(Params) ->
    fxmlrpc_codec:encode(
      {response, [encode_param(Param) || Param <- Params]},
      ?NS_XMLRPC);
encode({response, {fault, Code, String}}) ->
    fxmlrpc_codec:encode(
      {response, {fault, {{struct, [{faultCode, {{int, Code}, undefined}},
				    {faultString, {{string, String}, undefined}}]},
			  undefined}}},
      ?NS_XMLRPC).

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
