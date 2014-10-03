%%%-------------------------------------------------------------------
%%% @author Evgeny Khramtsov <ekhramtsov@process-one.net>
%%% @copyright (C) 2014, Evgeny Khramtsov
%%% @doc
%%%
%%% @end
%%% Created :  3 Oct 2014 by Evgeny Khramtsov <ekhramtsov@process-one.net>
%%%-------------------------------------------------------------------
-module(xmlrpc_decode).

%% API
-export([payload/1]).

%%%===================================================================
%%% API
%%%===================================================================
payload(S) ->
    case xml_stream:parse_element(S) of
	{error, _} = Err ->
	    Err;
	El ->
	    xmlrpc:decode(El)
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================
