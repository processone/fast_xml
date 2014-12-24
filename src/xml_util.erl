%%%----------------------------------------------------------------------
%%% File    : xml_util.erl
%%% Author  : Holger Weiss <holger@zedat.fu-berlin.de>
%%% Purpose : Provide replacements for newer Erlang functions
%%% Created : 23 Dec 2014 by Holger Weiss <holger@zedat.fu-berlin.de>
%%%
%%%
%%% ejabberd, Copyright (C) 2014   ProcessOne
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
%%% You should have received a copy of the GNU General Public License along
%%% with this program; if not, write to the Free Software Foundation, Inc.,
%%% 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
%%%
%%%----------------------------------------------------------------------

-module(xml_util).
-author('holger@zedat.fu-berlin.de').

-export([binary_to_float/1, binary_to_integer/1, binary_to_integer/2,
	 float_to_binary/1, integer_to_binary/1, integer_to_binary/2]).

-spec(binary_to_float/1 ::
(
  Binary :: binary())
    -> float()
).

binary_to_float(Binary) ->
    list_to_float(binary_to_list(Binary)).

-spec(binary_to_integer/1 ::
(
  Binary :: binary())
    -> integer()
).

binary_to_integer(Binary) ->
    list_to_integer(binary_to_list(Binary)).

-spec(binary_to_integer/2 ::
(
  Binary :: binary(),
  Base   :: 2..36)
    -> integer()
).

binary_to_integer(Binary, Base) ->
    list_to_integer(binary_to_list(Binary), Base).

-spec(float_to_binary/1 ::
(
  Float :: float())
    -> binary()
).

float_to_binary(Float) ->
    list_to_binary(float_to_list(Float)).

-spec(integer_to_binary/1 ::
(
  Integer :: integer())
    -> binary()
).

integer_to_binary(Integer) ->
    list_to_binary(integer_to_list(Integer)).

-spec(integer_to_binary/2 ::
(
  Integer :: integer(),
  Base    :: 2..36)
    -> binary()
).

integer_to_binary(Integer, Base) ->
    list_to_binary(integer_to_list(Integer, Base)).
