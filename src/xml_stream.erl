%%%----------------------------------------------------------------------
%%% File    : xml_stream.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : Parse XML streams
%%% Created : 17 Nov 2002 by Alexey Shchepin <alexey@process-one.net>
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

-module(xml_stream).

-author('alexey@process-one.net').

-export([new/1, new/2, parse/2, close/1,
	 change_callback_pid/2, parse_element/1]).

-export([load_nif/0]).

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

load_nif() ->
    NifFile = p1_nif_utils:get_so_path(?MODULE, [p1_xml, xml], "xml_stream"),
    case erlang:load_nif(NifFile, 0) of
	ok ->
	    ok;
        {error, {Reason, Txt}} ->
            error_logger:error_msg("failed to load NIF ~s: ~s",
                                   [NifFile, Txt]),
            {error, Reason}
    end.

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

parse_element(_Str) ->
    erlang:nif_error(nif_not_loaded).
