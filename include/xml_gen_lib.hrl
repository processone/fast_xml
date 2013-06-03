%%====================================================================
%% Auxiliary functions
%%====================================================================
get_els(Els, Name, XMLNS, Min, Max) ->
    get_els(Els, Name, XMLNS, [], Min, Max, 0).

get_els([{xmlel, Name, _, _} = El | Els],
	Name, <<"">>, Acc, Min, Max, Cur) ->
    get_els(Els, Name, <<"">>, [El | Acc], Min, Max, Cur + 1);
get_els([{xmlel, Name, Attrs, _} = El | Els],
	Name, XMLNS, Acc, Min, Max, Cur) ->
    case xml:get_attr_s(<<"xmlns">>, Attrs) of
	XMLNS ->
	    get_els(Els, Name, XMLNS, [El | Acc], Min, Max, Cur + 1);
	_ ->
	    get_els(Els, Name, XMLNS, Acc, Min, Max, Cur)
    end;
get_els([_ | Els], Name, XMLNS, Acc, Min, Max, Cur) ->
    get_els(Els, Name, XMLNS, Acc, Min, Max, Cur);
get_els([], _Name, _XMLNS, Acc, Min, Max, Cur)
  when Min =< Cur, Cur =< Max ->
    lists:reverse(Acc).

to_bool(<<"false">>) ->
    false;
to_bool(<<"true">>) ->
    true;
to_bool(<<"0">>) ->
    false;
to_bool(<<"1">>) ->
    true.

from_bool(false) ->
    <<"0">>;
from_bool(true) ->
    <<"1">>.

to_integer(Val, Min, Max) ->
    case erlang:binary_to_integer(Val) of
	Int when Min == unlimited, Int =< Max ->
	    {ok, Int};
	Int when Min =< Int, Int =< Max ->
	    {ok, Int}
    end.

from_integer(Int, unlimited, Max)
  when Int =< Max ->
    erlang:integer_to_binary(Int);
from_integer(Int, Min, Max)
  when Min =< Int, Int =< Max ->
    erlang:integer_to_binary(Int).

to_float(Val, Min, Max) ->
    case erlang:binary_to_float(Val) of
	Float when Min == unlimited, Float =< Max ->
	    {ok, Float};
	Float when Min =< Float, Float =< Max ->
	    {ok, Float}
    end.

from_float(Float, unlimited, Max)
  when Float =< Max ->
    erlang:float_to_binary(Float);
from_float(Float, Min, Max)
  when Min =< Float, Float =< Max ->
    erlang:float_to_binary(Float).

to_enum(Val, [H|_] = List) when is_binary(H) ->
    case lists:member(Val, List) of
	true ->
	    {ok, Val}
    end;
to_enum(Val, [H|_] = List) when is_atom(H) ->
    A = erlang:binary_to_existing_atom(Val, utf8),
    case lists:member(A, List) of
	true ->
	    {ok, A}
    end;
to_enum(Val, [H|_] = List) when is_integer(H) ->
    I = erlang:binary_to_integer(Val),
    case lists:member(I, List) of
	true ->
	    {ok, I}
    end;
to_enum(Val, [H|_] = List) when is_float(H) ->
    F = erlang:binary_to_float(Val),
    case lists:member(F, List) of
	true ->
	    {ok, F}
    end.

from_enum(Val, List) ->
    case lists:member(Val, List) of
	true when is_atom(Val) ->
	    erlang:atom_to_binary(Val, utf8);
	true when is_integer(Val) ->
	    erlang:integer_to_binary(Val);
	true when is_float(Val) ->
	    erlang:float_to_binary(Val);
	true when is_binary(Val) ->
	    Val
    end.

to_jid(Val) ->
    case jlib:string_to_jid(Val) of
	JID when JID /= error ->
	    {ok, JID}
    end.

from_jid(JID) ->
    case jlib:jid_to_string(JID) of
	JIDString when is_binary(JIDString),
		       JIDString /= <<"">> ->
	    JIDString
    end.

to_base64(Val) ->
    {ok, base64:decode(Val)}.

from_base64(Val) ->
    base64:encode(Val).

from_not_empty(<<_, _/binary>> = Val) ->
    Val.

to_not_empty(<<_, _/binary>> = Val) ->
    {ok, Val}.
