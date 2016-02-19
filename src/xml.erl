%%%----------------------------------------------------------------------
%%% File    : xml.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : XML utils for parsing, matching, processing XML
%%% Created : 20 Nov 2002 by Alexey Shchepin <alexey@process-one.net>
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

-module(xml).

-author('alexey@process-one.net').

-behaviour(gen_server).

-export([element_to_binary/1, get_so_path/0,
	 crypt/1, remove_cdata/1,
	 remove_subtags/3, get_cdata/1, get_tag_cdata/1,
	 get_attr/2, get_attr_s/2, get_tag_attr/2,
	 get_tag_attr_s/2, get_subtag/2, get_subtags/2, get_subtag_cdata/2,
         get_subtag_with_xmlns/3, get_subtags_with_xmlns/3,
	 append_subtags/2, get_path_s/2,
	 replace_tag_attr/3, replace_subtag/2, to_xmlel/1,
	 memory_counter_nif/0]).

%% Internal exports, call-back functions.
-export([start_link/0, init/1, handle_call/3, handle_cast/2,
	 handle_info/2, code_change/3, terminate/2]).

-include("xml.hrl").

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [],
			  []).

%% Replace element_to_binary/1 with NIF
init([]) ->
    SOPath = filename:join(get_so_path(), "xml"),
    case catch erlang:load_nif(SOPath, 0) of
        ok -> ok;
        Err -> error_logger:warning_msg("unable to load xml NIF: ~p~n", [Err])
    end,
    {ok, []}.

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


memory_counter_nif() ->
    erlang:nif_error(nif_not_loaded).

%%
-spec(element_to_binary/1 ::
(
  El :: xmlel() | cdata())
    -> binary()
).

element_to_binary(El) ->
    iolist_to_binary(element_to_string(El)).

%%
-spec(element_to_string/1 ::
(
  El :: xmlel() | cdata())
    -> string()
).

element_to_string(El) ->
    case catch element_to_string_nocatch(El) of
      {'EXIT', Reason} -> erlang:error({badxml, El, Reason});
      Result -> Result
    end.

-spec(element_to_string_nocatch/1 ::
(
  El :: xmlel() | cdata())
    -> iolist()
).

element_to_string_nocatch(El) ->
    case El of
      #xmlel{name = Name, attrs = Attrs, children = Els} ->
	  if Els /= [] ->
		 [$<, Name, attrs_to_list(Attrs), $>,
		  [element_to_string_nocatch(E) || E <- Els], $<, $/,
		  Name, $>];
	     true -> [$<, Name, attrs_to_list(Attrs), $/, $>]
	  end;
      %% We do not crypt CDATA binary, but we enclose it in XML CDATA
      {xmlcdata, CData} ->
	  crypt(CData)
    end.

attrs_to_list(Attrs) -> [attr_to_list(A) || A <- Attrs].

attr_to_list({Name, Value}) ->
    [$\s, Name, $=, $', crypt(Value), $'].

crypt(S) ->
    << <<(case C of
              $& -> <<"&amp;">>;
              $< -> <<"&lt;">>;
              $> -> <<"&gt;">>;
              $" -> <<"&quot;">>;
              $' -> <<"&apos;">>;
              _ -> <<C>>
          end)/binary>>
       || <<C>> <= S >>.

%%
-spec(remove_cdata_p/1 ::
(
  El :: xmlel() | cdata())
    -> boolean()
).

remove_cdata_p(#xmlel{}) -> true;
remove_cdata_p(_) -> false.

%%
-spec(remove_cdata/1 ::
(
  L :: [xmlel() | cdata()])
    -> [xmlel()]
).

remove_cdata(L) -> [E || E <- L, remove_cdata_p(E)].

%% This function is intended to remove subtags based on a name and an
%% attribute, usually an xmlns attribute for a specific XMPP
%% extension.
-spec(remove_subtags/3 ::
(
  Xmlel :: xmlel(),
  Name  :: binary(),
  Attr  :: attr())
    -> Xmlel :: xmlel()
).

remove_subtags(#xmlel{name = TagName, attrs = TagAttrs, children = Els},
  Name, Attr) ->
    #xmlel{name = TagName, attrs = TagAttrs,
        children = remove_subtags1(Els, [], Name, Attr)}.

%%
-spec(remove_subtags1/4 ::
(
  Els    :: [xmlel() | cdata()],
  NewEls :: [xmlel()],
  Name   :: binary(),
  Attr   :: attr())
    -> NewEls :: [xmlel()]
).

remove_subtags1([], NewEls, _Name, _Attr) ->
    lists:reverse(NewEls);
remove_subtags1([El | Els], NewEls, Name,
		{AttrName, AttrValue} = Attr) ->
    case El of
      #xmlel{name = Name, attrs = Attrs} ->
	  case get_attr(AttrName, Attrs) of
	    false ->
		remove_subtags1(Els, [El | NewEls], Name, Attr);
	    {value, AttrValue} ->
		remove_subtags1(Els, NewEls, Name, Attr);
	    _ -> remove_subtags1(Els, [El | NewEls], Name, Attr)
	  end;
      _ -> remove_subtags1(Els, [El | NewEls], Name, Attr)
    end.

-spec(get_cdata/1 ::
(
  L :: [xmlel() | cdata()])
    -> binary()
).

get_cdata(L) ->
    (iolist_to_binary(get_cdata(L, <<"">>))).

-spec(get_cdata/2 ::
(
  L :: [xmlel() | cdata()],
  S :: binary() | iolist())
    -> binary() | iolist()
).

get_cdata([{xmlcdata, CData} | L], S) ->
     get_cdata(L, [S, CData]);
get_cdata([_ | L], S) -> get_cdata(L, S);
get_cdata([], S) -> S.

-spec(get_tag_cdata/1 ::
(
  Xmlel :: xmlel())
    -> binary()
).

get_tag_cdata(#xmlel{children = Els}) -> get_cdata(Els).

%%
-spec(get_attr/2 ::
(
  AttrName :: binary(),
  Attrs    :: [attr()])
    -> {value, binary()}
     | false
).

get_attr(AttrName, Attrs) ->
    case lists:keysearch(AttrName, 1, Attrs) of
      {value, {_, Val}} -> {value, Val};
      _ -> false
    end.

%%
-spec(get_attr_s/2 ::
(
  AttrName :: binary(),
  Attrs    :: [attr()])
    -> Val :: binary()
).

get_attr_s(AttrName, Attrs) ->
    case lists:keysearch(AttrName, 1, Attrs) of
      {value, {_, Val}} -> Val;
      _ -> <<"">>
    end.

%%
-spec(get_tag_attr/2 ::
(
  AttrName :: binary(),
  Xmlel    :: xmlel())
    -> {value, binary()}
     | false
).

get_tag_attr(AttrName, #xmlel{attrs = Attrs}) ->
    get_attr(AttrName, Attrs).

%%
-spec(get_tag_attr_s/2 ::
(
  AttrName :: binary(),
  Xmlel    :: xmlel())
    -> binary()
).

get_tag_attr_s(AttrName, #xmlel{attrs = Attrs}) ->
    get_attr_s(AttrName, Attrs).

%%
-spec(get_subtag/2 ::
(
  Xmlel :: xmlel(),
  Name  :: binary())
    -> xmlel() | false
).

get_subtag(#xmlel{children = Els}, Name) ->
    get_subtag1(Els, Name).

%%
-spec(get_subtag1/2 ::
(
  Els  :: [xmlel() | cdata()],
  Name :: binary())
    -> xmlel() | false
).

get_subtag1( [El | Els], Name) ->
    case El of
      #xmlel{name = Name} -> El;
      _ -> get_subtag1(Els, Name)
    end;
get_subtag1([], _) -> false.

-spec(get_subtags/2 ::
(
  Xmlel :: xmlel(),
  Name  :: binary())
    -> [xmlel()]
).

get_subtags(#xmlel{children = Els}, Name) ->
    get_subtags1(Els, Name, []).

get_subtags1([], _Name, Acc) ->
    lists:reverse(Acc);
get_subtags1([El | Els], Name, Acc) ->
    case El of
        #xmlel{name = Name} -> get_subtags1(Els, Name, [El|Acc]);
        _ -> get_subtags1(Els, Name, Acc)
    end.

%%
-spec(get_subtag_with_xmlns/3 ::
(
  Xmlel :: xmlel(),
  Name  :: binary(),
  XMLNS :: binary())
    -> xmlel() | false
).

get_subtag_with_xmlns(#xmlel{children = Els}, Name, XMLNS) ->
    get_subtag_with_xmlns1(Els, Name, XMLNS).

%%
-spec(get_subtag_with_xmlns1/3 ::
(
  Els  :: [xmlel() | cdata()],
  Name :: binary(),
  XMLNS :: binary())
    -> xmlel() | false
).

get_subtag_with_xmlns1([El | Els], Name, XMLNS) ->
    case El of
	#xmlel{name = Name, attrs = Attrs} ->
            case get_attr(<<"xmlns">>, Attrs) of
                {value, XMLNS} ->
                    El;
                _ ->
                    get_subtag_with_xmlns1(Els, Name, XMLNS)
            end;
	_ ->
	    get_subtag_with_xmlns1(Els, Name, XMLNS)
    end;
get_subtag_with_xmlns1([], _, _) ->
    false.

-spec(get_subtags_with_xmlns/3 ::
(
  Xmlel :: xmlel(),
  Name  :: binary(),
  XMLNS :: binary())
    -> [xmlel()]
).

get_subtags_with_xmlns(#xmlel{children = Els}, Name, XMLNS) ->
    get_subtags_with_xmlns1(Els, Name, XMLNS, []).

get_subtags_with_xmlns1([], _Name, _XMLNS, Acc) ->
    lists:reverse(Acc);
get_subtags_with_xmlns1([El | Els], Name, XMLNS, Acc) ->
    case El of
	#xmlel{name = Name, attrs = Attrs} ->
            case get_attr(<<"xmlns">>, Attrs) of
                {value, XMLNS} ->
                    get_subtags_with_xmlns1(Els, Name, XMLNS, [El|Acc]);
                _ ->
                    get_subtags_with_xmlns1(Els, Name, XMLNS, Acc)
            end;
	_ ->
	    get_subtags_with_xmlns1(Els, Name, XMLNS, Acc)
    end.

%%
-spec(get_subtag_cdata/2 ::
(
  Tag  :: xmlel(),
  Name :: binary())
    -> binary()
).

get_subtag_cdata(Tag, Name) ->
    case get_subtag(Tag, Name) of
      false -> <<"">>;
      Subtag -> get_tag_cdata(Subtag)
    end.

%%
-spec(append_subtags/2 ::
(
  Xmlel    :: xmlel(),
  SubTags2 :: [xmlel() | cdata()])
    -> Xmlel :: xmlel()
).

append_subtags(#xmlel{name = Name, attrs = Attrs, children = SubTags1}, SubTags2) ->
    #xmlel{name = Name, attrs = Attrs, children = SubTags1 ++ SubTags2}.

%%
-spec(get_path_s/2 ::
(
  El   :: xmlel(),
  Path :: [{elem, Name::binary()}
          |{attr, Name::binary()}
          |cdata])
    -> xmlel()
     | binary()
).

get_path_s(El, []) -> El;
get_path_s(El, [{elem, Name} | Path]) ->
    case get_subtag(El, Name) of
      false -> <<"">>;
      SubEl -> get_path_s(SubEl, Path)
    end;
get_path_s(El, [{attr, Name}]) ->
    get_tag_attr_s(Name, El);
get_path_s(El, [cdata]) -> get_tag_cdata(El).

%%
-spec(replace_tag_attr/3 ::
(
  Name  :: binary(),
  Value :: binary(),
  Xmlel :: xmlel())
    -> Xmlel :: #xmlel{
           name     :: binary(),
           attrs    :: [attr(),...],
           children :: [xmlel() | cdata()]
       }
).

replace_tag_attr(Name, Value, Xmlel) ->
    Xmlel#xmlel{
        attrs = [{Name, Value} | lists:keydelete(Name, 1, Xmlel#xmlel.attrs)]
    }.


-spec(replace_subtag/2 ::
(
  Tag   :: xmlel(),
  Xmlel :: xmlel())
    -> Xmlel :: #xmlel{
           name     :: binary(),
           attrs    :: [attr(),...],
           children :: [xmlel() | cdata()]
       }
).

replace_subtag(#xmlel{name = Name} = Tag, Xmlel) ->
    Xmlel#xmlel{
        children = [Tag | lists:keydelete(Name, #xmlel.name, Xmlel#xmlel.children)]
    }.

to_xmlel({_, Name, Attrs, Els}) ->
    #xmlel{name = iolist_to_binary(Name),
           attrs = [{iolist_to_binary(K), iolist_to_binary(V)}
                    || {K, V} <- Attrs],
           children = [to_xmlel(El) || El <- Els]};
to_xmlel({xmlcdata, CData}) ->
    {xmlcdata, iolist_to_binary(CData)}.

get_so_path() ->
    EbinDir = filename:dirname(code:which(?MODULE)),
    AppDir = filename:dirname(EbinDir),
    filename:join([AppDir, "priv", "lib"]).
