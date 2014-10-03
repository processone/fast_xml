%%%-------------------------------------------------------------------
%%% @author Evgeny Khramtsov <ekhramtsov@process-one.net>
%%% @copyright (C) 2014, Evgeny Khramtsov
%%% @doc
%%%
%%% @end
%%% Created :  3 Oct 2014 by Evgeny Khramtsov <ekhramtsov@process-one.net>
%%%-------------------------------------------------------------------
-module(xmlrpc).

%% API
-export([decode/1, encode/1]).

%%%===================================================================
%%% API
%%%===================================================================
decode(El) ->
    try xmlrpc_codec:decode(El) of
	{call, Name, Params} ->
	    {call, Name, [decode_param(Param) || Param <- Params]};
	{response, Params} when is_list(Params) ->
	    {response, [decode_param(Param) || Param <- Params]};
	{response, {fault, {struct, Struct}}} ->
	    case {proplists:get_value(faultCode, Struct),
		  proplists:get_value(faultString, Struct)} of
		{{Tag, Code}, {string, String}} when Tag == int; Tag == i4 ->
		    {response, {fault, Code, String}};
		R ->
		    {error, {bad_struct, Struct, R}}
	    end;
	Other ->
	    {error, {unexpected_element, Other}}
    catch error:{xmlrpc_codec, Reason} ->
	    {error, Reason}
    end.

encode({call, Name, Params}) ->
    xmlrpc_codec:encode({call, Name, [encode_param(Param) || Param <- Params]});
encode({response, Params}) when is_list(Params) ->
    xmlrpc_codec:encode({response, [encode_param(Param) || Param <- Params]});
encode({response, {fault, Code, String}}) ->
    xmlrpc_codec:encode(
      {response, {fault, {struct, [{faultCode, {int, Code}},
				   {faultString, {string, String}}]}}}).

%%%===================================================================
%%% Internal functions
%%%===================================================================
decode_param({int, Int}) ->
    Int;
decode_param({i4, Int}) ->
    Int;
decode_param({boolean, Bool}) ->
    Bool;
decode_param({string, S}) ->
    S;
decode_param({double, D}) ->
    D;
decode_param({array, L}) ->
    {array, [decode_param(E) || E <- L]};
decode_param({struct, S}) ->
    {struct, [{Name, decode_param(Value)} || {Name, Value} <- S]};
decode_param({base64, B64}) ->
    {base64, B64};
decode_param({date, Date}) ->
    {date, Date};
decode_param(nil) ->
    nil.

encode_param(Int) when is_integer(Int) ->
    {int, Int};
encode_param(B) when is_boolean(B) ->
    {boolean, B};
encode_param(S) when is_binary(S) ->
    {string, S};
encode_param(D) when is_float(D) ->
    {double, D};
encode_param({array, L}) ->
    {array, [encode_param(E) || E <- L]};
encode_param({struct, S}) ->
    {struct, [{Name, encode_param(Value)} || {Name, Value} <- S]};
encode_param({base64, B64}) ->
    {base64, B64};
encode_param({date, Date}) ->
    {date, Date};
encode_param(nil) ->
    nil.

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
    ?assertEqual(Result, ?MODULE:decode(Fault)),
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
    ?assertEqual(Result, ?MODULE:decode(Call)),
    ?assertEqual(Call, ?MODULE:encode(Result)).

response_test() ->
    Response = {xmlel,<<"methodResponse">>, [],
		[{xmlel,<<"params">>,[],
		  [{xmlel,<<"param">>,[],
		    [{xmlel,<<"value">>,[],
		      [{xmlel,<<"string">>,[],
			[{xmlcdata,<<"South Dakota">>}]}]}]}]}]},
    Result = {response,[<<"South Dakota">>]},
    ?assertEqual(Result, ?MODULE:decode(Response)),
    ?assertEqual(Response, ?MODULE:encode(Result)).

-endif.
