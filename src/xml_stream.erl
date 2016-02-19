%%%----------------------------------------------------------------------
%%% File    : xml_stream.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : Parse XML streams
%%% Created : 17 Nov 2002 by Alexey Shchepin <alexey@process-one.net>
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
%%%----------------------------------------------------------------------

-module(xml_stream).

-author('alexey@process-one.net').

-behaviour(gen_server).

-export([new/1, new/2, parse/2, close/1,
	 change_callback_pid/2, parse_element/1, memory_counter_nif/0]).

%% Internal exports, call-back functions.
-export([start_link/0, init/1, handle_call/3, handle_cast/2,
	 handle_info/2, code_change/3, terminate/2]).

-include("xml.hrl").

-record(xml_stream_state,
	{callback_pid = self() :: pid(),
         port                  :: port(),
         stack = []            :: stack(),
         size = 0              :: non_neg_integer(),
         maxsize = infinity    :: non_neg_integer() | infinity}).

-type xml_stream_el() :: {xmlstreamraw, binary()} |
                         {xmlstreamcdata, binary()} |
                         {xmlstreamelement, xmlel()} |
                         {xmlstreamend, binary()} |
                         {xmlstreamstart, binary(), [attr()]} |
                         {xmlstreamerror, binary()}.

-type xml_stream_state() :: #xml_stream_state{}.
-type stack() :: [xmlel()].

-export_type([xml_stream_state/0, xml_stream_el/0]).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [],
			  []).

init([]) ->
    case load_dlls() of
        ok ->
            {ok, []};
        {error, Why} ->
            {stop, Why}
    end.

%%% --------------------------------------------------------
%%% The call-back functions.
%%% --------------------------------------------------------

handle_call(_, _, State) -> {noreply, State}.

handle_cast(_, State) -> {noreply, State}.

handle_info({'EXIT', Port, Reason}, Port) ->
    {stop, {port_died, Reason}, Port};
handle_info({'EXIT', _Pid, _Reason}, Port) ->
    {noreply, Port};
handle_info(_, State) -> {noreply, State}.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

terminate(_Reason, _State) ->
    ok.

-spec new(pid()) -> xml_stream_state().

new(CallbackPid) ->
    new(CallbackPid, infinity).

-spec new(pid(), non_neg_integer() | infinity) -> xml_stream_state().

new(_CallbackPid, _MaxSize) ->
    erlang:nif_error(nif_not_loaded).

-spec change_callback_pid(xml_stream_state(), pid()) -> xml_stream_state().

change_callback_pid(_State, _CallbackPid) ->
    erlang:nif_error(nif_not_loaded).

-spec parse(xml_stream_state(), iodata()) -> xml_stream_state().

parse(_State, _Data) ->
    erlang:nif_error(nif_not_loaded).

-spec close(xml_stream_state()) -> true.

close(_State) ->
    erlang:nif_error(nif_not_loaded).

-spec parse_element(iodata()) -> xmlel() |
                                 {error, parse_error} |
                                 {error, binary()}.

memory_counter_nif() ->
    erlang:nif_error(nif_not_loaded).

parse_element(_Str) ->
    erlang:nif_error(nif_not_loaded).

get_so_path() ->
    EbinDir = filename:dirname(code:which(?MODULE)),
    AppDir = filename:dirname(EbinDir),
    filename:join([AppDir, "priv", "lib"]).

load_dlls() ->
    NifFile = filename:join([get_so_path(), atom_to_list(?MODULE)]),
    case erlang:load_nif(NifFile, 0) of
	ok ->
	    ok;
        {error, {Reason, Txt}} ->
            error_logger:error_msg("failed to load NIF ~s: ~s",
                                   [NifFile, Txt]),
            {error, Reason}
    end.
