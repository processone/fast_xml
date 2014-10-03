%%%-------------------------------------------------------------------
%%% @author Evgeny Khramtsov <ekhramtsov@process-one.net>
%%% @copyright (C) 2014, Evgeny Khramtsov
%%% @doc
%%%   Legacy interface
%%% @end
%%% Created :  3 Oct 2014 by Evgeny Khramtsov <ekhramtsov@process-one.net>
%%%-------------------------------------------------------------------
-module(xmlrpc_encode).

%% API
-export([payload/1]).

%%%===================================================================
%%% API
%%%===================================================================
payload(P) ->
    XML = xml:element_to_binary(xmlrpc:encode(P)),
    {ok, <<"<?xml version=\"1.0\"?>", XML/binary>>}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
