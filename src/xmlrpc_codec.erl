%% Created automatically by XML generator (xml_gen.erl)
%% Source: xmlrpc_codec.spec

-module(xmlrpc_codec).

-compile({nowarn_unused_function,
	  [{dec_int, 3}, {dec_int, 1}, {dec_enum, 2},
	   {enc_int, 1}, {get_attr, 2}, {enc_enum, 1}]}).

-export([pp/1, format_error/1, decode/1, decode/2,
	 is_known_tag/1, encode/1, get_ns/1]).

decode(_el) -> decode(_el, []).

decode(_el, Opts) ->
    IgnoreEls = proplists:get_bool(ignore_els, Opts),
    case _el of
      {xmlel, <<"name">>, _, _} ->
	  decode_name(<<>>, IgnoreEls, _el);
      {xmlel, <<"member">>, _, _} ->
	  decode_member(<<>>, IgnoreEls, _el);
      {xmlel, <<"struct">>, _, _} ->
	  decode_struct(<<>>, IgnoreEls, _el);
      {xmlel, <<"nil">>, _, _} ->
	  decode_nil(<<>>, IgnoreEls, _el);
      {xmlel, <<"data">>, _, _} ->
	  decode_data(<<>>, IgnoreEls, _el);
      {xmlel, <<"array">>, _, _} ->
	  decode_array(<<>>, IgnoreEls, _el);
      {xmlel, <<"boolean">>, _, _} ->
	  decode_boolean(<<>>, IgnoreEls, _el);
      {xmlel, <<"dateTime.iso8601">>, _, _} ->
	  decode_dateTime(<<>>, IgnoreEls, _el);
      {xmlel, <<"base64">>, _, _} ->
	  decode_base64(<<>>, IgnoreEls, _el);
      {xmlel, <<"double">>, _, _} ->
	  decode_double(<<>>, IgnoreEls, _el);
      {xmlel, <<"string">>, _, _} ->
	  decode_string(<<>>, IgnoreEls, _el);
      {xmlel, <<"int">>, _, _} ->
	  decode_int(<<>>, IgnoreEls, _el);
      {xmlel, <<"i4">>, _, _} ->
	  decode_i4(<<>>, IgnoreEls, _el);
      {xmlel, <<"value">>, _, _} ->
	  decode_value(<<>>, IgnoreEls, _el);
      {xmlel, <<"param">>, _, _} ->
	  decode_param(<<>>, IgnoreEls, _el);
      {xmlel, <<"params">>, _, _} ->
	  decode_params(<<>>, IgnoreEls, _el);
      {xmlel, <<"methodName">>, _, _} ->
	  decode_methodName(<<>>, IgnoreEls, _el);
      {xmlel, <<"fault">>, _, _} ->
	  decode_fault(<<>>, IgnoreEls, _el);
      {xmlel, <<"methodResponse">>, _, _} ->
	  decode_methodResponse(<<>>, IgnoreEls, _el);
      {xmlel, <<"methodCall">>, _, _} ->
	  decode_methodCall(<<>>, IgnoreEls, _el);
      {xmlel, _name, _, _} ->
	  erlang:error({xmlrpc_codec, {unknown_tag, _name, <<>>}})
    end.

is_known_tag({xmlel, <<"name">>, _, _}) -> true;
is_known_tag({xmlel, <<"member">>, _, _}) -> true;
is_known_tag({xmlel, <<"struct">>, _, _}) -> true;
is_known_tag({xmlel, <<"nil">>, _, _}) -> true;
is_known_tag({xmlel, <<"data">>, _, _}) -> true;
is_known_tag({xmlel, <<"array">>, _, _}) -> true;
is_known_tag({xmlel, <<"boolean">>, _, _}) -> true;
is_known_tag({xmlel, <<"dateTime.iso8601">>, _, _}) ->
    true;
is_known_tag({xmlel, <<"base64">>, _, _}) -> true;
is_known_tag({xmlel, <<"double">>, _, _}) -> true;
is_known_tag({xmlel, <<"string">>, _, _}) -> true;
is_known_tag({xmlel, <<"int">>, _, _}) -> true;
is_known_tag({xmlel, <<"i4">>, _, _}) -> true;
is_known_tag({xmlel, <<"value">>, _, _}) -> true;
is_known_tag({xmlel, <<"param">>, _, _}) -> true;
is_known_tag({xmlel, <<"params">>, _, _}) -> true;
is_known_tag({xmlel, <<"methodName">>, _, _}) -> true;
is_known_tag({xmlel, <<"fault">>, _, _}) -> true;
is_known_tag({xmlel, <<"methodResponse">>, _, _}) ->
    true;
is_known_tag({xmlel, <<"methodCall">>, _, _}) -> true;
is_known_tag(_) -> true.

encode({xmlel, _, _, _} = El) -> El;
encode({call, _, _} = Methodcall) ->
    encode_methodCall(Methodcall, []);
encode({response, _} = Methodresponse) ->
    encode_methodResponse(Methodresponse, []);
encode({fault, _} = Fault) -> encode_fault(Fault, []);
encode({i4, _} = I4) -> encode_i4(I4, []);
encode({int, _} = Int) -> encode_int(Int, []);
encode({string, _} = String) ->
    encode_string(String, []);
encode({double, _} = Double) ->
    encode_double(Double, []);
encode({base64, _} = Base64) ->
    encode_base64(Base64, []);
encode({date, _} = Datetime_iso8601) ->
    encode_dateTime(Datetime_iso8601, []);
encode({boolean, _} = Boolean) ->
    encode_boolean(Boolean, []);
encode({array, _} = Array) -> encode_array(Array, []);
encode({struct, _} = Struct) ->
    encode_struct(Struct, []).

get_ns(_) -> <<>>.

dec_int(Val) -> dec_int(Val, infinity, infinity).

dec_int(Val, Min, Max) ->
    case list_to_integer(binary_to_list(Val)) of
      Int when Int =< Max, Min == infinity -> Int;
      Int when Int =< Max, Int >= Min -> Int
    end.

enc_int(Int) -> list_to_binary(integer_to_list(Int)).

dec_enum(Val, Enums) ->
    AtomVal = erlang:binary_to_existing_atom(Val, utf8),
    case lists:member(AtomVal, Enums) of
      true -> AtomVal
    end.

enc_enum(Atom) -> erlang:atom_to_binary(Atom, utf8).

format_error({bad_attr_value, Attr, Tag, XMLNS}) ->
    <<"Bad value of attribute '", Attr/binary, "' in tag <",
      Tag/binary, "/> qualified by namespace '", XMLNS/binary,
      "'">>;
format_error({bad_cdata_value, <<>>, Tag, XMLNS}) ->
    <<"Bad value of cdata in tag <", Tag/binary,
      "/> qualified by namespace '", XMLNS/binary, "'">>;
format_error({missing_tag, Tag, XMLNS}) ->
    <<"Missing tag <", Tag/binary,
      "/> qualified by namespace '", XMLNS/binary, "'">>;
format_error({missing_attr, Attr, Tag, XMLNS}) ->
    <<"Missing attribute '", Attr/binary, "' in tag <",
      Tag/binary, "/> qualified by namespace '", XMLNS/binary,
      "'">>;
format_error({missing_cdata, <<>>, Tag, XMLNS}) ->
    <<"Missing cdata in tag <", Tag/binary,
      "/> qualified by namespace '", XMLNS/binary, "'">>;
format_error({unknown_tag, Tag, XMLNS}) ->
    <<"Unknown tag <", Tag/binary,
      "/> qualified by namespace '", XMLNS/binary, "'">>.

get_attr(Attr, Attrs) ->
    case lists:keyfind(Attr, 1, Attrs) of
      {_, Val} -> Val;
      false -> <<>>
    end.

pp(Term) -> io_lib_pretty:print(Term, fun pp/2).

pp(call, 2) -> [name, params];
pp(response, 1) -> [payload];
pp(fault, 1) -> [value];
pp(i4, 1) -> [cdata];
pp(int, 1) -> [cdata];
pp(string, 1) -> [cdata];
pp(double, 1) -> [cdata];
pp(base64, 1) -> [cdata];
pp(date, 1) -> [cdata];
pp(boolean, 1) -> [cdata];
pp(array, 1) -> [data];
pp(struct, 1) -> [members];
pp(_, _) -> no.

enc_bool(false) -> <<"0">>;
enc_bool(true) -> <<"1">>.

dec_bool(<<"false">>) -> false;
dec_bool(<<"0">>) -> false;
dec_bool(<<"true">>) -> true;
dec_bool(<<"1">>) -> true.

decode_name(__TopXMLNS, __IgnoreEls,
	    {xmlel, <<"name">>, _attrs, _els}) ->
    Cdata = decode_name_els(__TopXMLNS, __IgnoreEls, _els,
			    <<>>),
    Cdata.

decode_name_els(__TopXMLNS, __IgnoreEls, [], Cdata) ->
    decode_name_cdata(__TopXMLNS, Cdata);
decode_name_els(__TopXMLNS, __IgnoreEls,
		[{xmlcdata, _data} | _els], Cdata) ->
    decode_name_els(__TopXMLNS, __IgnoreEls, _els,
		    <<Cdata/binary, _data/binary>>);
decode_name_els(__TopXMLNS, __IgnoreEls, [_ | _els],
		Cdata) ->
    decode_name_els(__TopXMLNS, __IgnoreEls, _els, Cdata).

encode_name(Cdata, _xmlns_attrs) ->
    _els = encode_name_cdata(Cdata, []),
    _attrs = _xmlns_attrs,
    {xmlel, <<"name">>, _attrs, _els}.

decode_name_cdata(__TopXMLNS, <<>>) ->
    erlang:error({xmlrpc_codec,
		  {missing_cdata, <<>>, <<"name">>, __TopXMLNS}});
decode_name_cdata(__TopXMLNS, _val) ->
    case catch erlang:binary_to_atom(_val, utf8) of
      {'EXIT', _} ->
	  erlang:error({xmlrpc_codec,
			{bad_cdata_value, <<>>, <<"name">>, __TopXMLNS}});
      _res -> _res
    end.

encode_name_cdata(_val, _acc) ->
    [{xmlcdata, erlang:atom_to_binary(_val, utf8)} | _acc].

decode_member(__TopXMLNS, __IgnoreEls,
	      {xmlel, <<"member">>, _attrs, _els}) ->
    {Value, Name} = decode_member_els(__TopXMLNS,
				      __IgnoreEls, _els, error, error),
    {Name, Value}.

decode_member_els(__TopXMLNS, __IgnoreEls, [], Value,
		  Name) ->
    {case Value of
       error ->
	   erlang:error({xmlrpc_codec,
			 {missing_tag, <<"value">>, __TopXMLNS}});
       {value, Value1} -> Value1
     end,
     case Name of
       error ->
	   erlang:error({xmlrpc_codec,
			 {missing_tag, <<"name">>, __TopXMLNS}});
       {value, Name1} -> Name1
     end};
decode_member_els(__TopXMLNS, __IgnoreEls,
		  [{xmlel, <<"name">>, _attrs, _} = _el | _els], Value,
		  _) ->
    decode_member_els(__TopXMLNS, __IgnoreEls, _els, Value,
		      {value, decode_name(__TopXMLNS, __IgnoreEls, _el)});
decode_member_els(__TopXMLNS, __IgnoreEls,
		  [{xmlel, <<"value">>, _attrs, _} = _el | _els], _,
		  Name) ->
    decode_member_els(__TopXMLNS, __IgnoreEls, _els,
		      {value, decode_value(__TopXMLNS, __IgnoreEls, _el)},
		      Name);
decode_member_els(__TopXMLNS, __IgnoreEls, [_ | _els],
		  Value, Name) ->
    decode_member_els(__TopXMLNS, __IgnoreEls, _els, Value,
		      Name).

encode_member({Name, Value}, _xmlns_attrs) ->
    _els = lists:reverse('encode_member_$value'(Value,
						'encode_member_$name'(Name,
								      []))),
    _attrs = _xmlns_attrs,
    {xmlel, <<"member">>, _attrs, _els}.

'encode_member_$value'(Value, _acc) ->
    [encode_value(Value, []) | _acc].

'encode_member_$name'(Name, _acc) ->
    [encode_name(Name, []) | _acc].

decode_struct(__TopXMLNS, __IgnoreEls,
	      {xmlel, <<"struct">>, _attrs, _els}) ->
    Members = decode_struct_els(__TopXMLNS, __IgnoreEls,
				_els, []),
    {struct, Members}.

decode_struct_els(__TopXMLNS, __IgnoreEls, [],
		  Members) ->
    lists:reverse(Members);
decode_struct_els(__TopXMLNS, __IgnoreEls,
		  [{xmlel, <<"member">>, _attrs, _} = _el | _els],
		  Members) ->
    decode_struct_els(__TopXMLNS, __IgnoreEls, _els,
		      [decode_member(__TopXMLNS, __IgnoreEls, _el)
		       | Members]);
decode_struct_els(__TopXMLNS, __IgnoreEls, [_ | _els],
		  Members) ->
    decode_struct_els(__TopXMLNS, __IgnoreEls, _els,
		      Members).

encode_struct({struct, Members}, _xmlns_attrs) ->
    _els = lists:reverse('encode_struct_$members'(Members,
						  [])),
    _attrs = _xmlns_attrs,
    {xmlel, <<"struct">>, _attrs, _els}.

'encode_struct_$members'([], _acc) -> _acc;
'encode_struct_$members'([Members | _els], _acc) ->
    'encode_struct_$members'(_els,
			     [encode_member(Members, []) | _acc]).

decode_nil(__TopXMLNS, __IgnoreEls,
	   {xmlel, <<"nil">>, _attrs, _els}) ->
    nil.

encode_nil(nil, _xmlns_attrs) ->
    _els = [],
    _attrs = _xmlns_attrs,
    {xmlel, <<"nil">>, _attrs, _els}.

decode_data(__TopXMLNS, __IgnoreEls,
	    {xmlel, <<"data">>, _attrs, _els}) ->
    V = decode_data_els(__TopXMLNS, __IgnoreEls, _els, []),
    V.

decode_data_els(__TopXMLNS, __IgnoreEls, [], V) ->
    lists:reverse(V);
decode_data_els(__TopXMLNS, __IgnoreEls,
		[{xmlel, <<"value">>, _attrs, _} = _el | _els], V) ->
    decode_data_els(__TopXMLNS, __IgnoreEls, _els,
		    case decode_value(__TopXMLNS, __IgnoreEls, _el) of
		      undefined -> V;
		      _new_el -> [_new_el | V]
		    end);
decode_data_els(__TopXMLNS, __IgnoreEls, [_ | _els],
		V) ->
    decode_data_els(__TopXMLNS, __IgnoreEls, _els, V).

encode_data(V, _xmlns_attrs) ->
    _els = lists:reverse('encode_data_$v'(V, [])),
    _attrs = _xmlns_attrs,
    {xmlel, <<"data">>, _attrs, _els}.

'encode_data_$v'([], _acc) -> _acc;
'encode_data_$v'([V | _els], _acc) ->
    'encode_data_$v'(_els, [encode_value(V, []) | _acc]).

decode_array(__TopXMLNS, __IgnoreEls,
	     {xmlel, <<"array">>, _attrs, _els}) ->
    Data = decode_array_els(__TopXMLNS, __IgnoreEls, _els,
			    error),
    {array, Data}.

decode_array_els(__TopXMLNS, __IgnoreEls, [], Data) ->
    case Data of
      error ->
	  erlang:error({xmlrpc_codec,
			{missing_tag, <<"data">>, __TopXMLNS}});
      {value, Data1} -> Data1
    end;
decode_array_els(__TopXMLNS, __IgnoreEls,
		 [{xmlel, <<"data">>, _attrs, _} = _el | _els], _) ->
    decode_array_els(__TopXMLNS, __IgnoreEls, _els,
		     {value, decode_data(__TopXMLNS, __IgnoreEls, _el)});
decode_array_els(__TopXMLNS, __IgnoreEls, [_ | _els],
		 Data) ->
    decode_array_els(__TopXMLNS, __IgnoreEls, _els, Data).

encode_array({array, Data}, _xmlns_attrs) ->
    _els = lists:reverse('encode_array_$data'(Data, [])),
    _attrs = _xmlns_attrs,
    {xmlel, <<"array">>, _attrs, _els}.

'encode_array_$data'(Data, _acc) ->
    [encode_data(Data, []) | _acc].

decode_boolean(__TopXMLNS, __IgnoreEls,
	       {xmlel, <<"boolean">>, _attrs, _els}) ->
    Cdata = decode_boolean_els(__TopXMLNS, __IgnoreEls,
			       _els, <<>>),
    {boolean, Cdata}.

decode_boolean_els(__TopXMLNS, __IgnoreEls, [],
		   Cdata) ->
    decode_boolean_cdata(__TopXMLNS, Cdata);
decode_boolean_els(__TopXMLNS, __IgnoreEls,
		   [{xmlcdata, _data} | _els], Cdata) ->
    decode_boolean_els(__TopXMLNS, __IgnoreEls, _els,
		       <<Cdata/binary, _data/binary>>);
decode_boolean_els(__TopXMLNS, __IgnoreEls, [_ | _els],
		   Cdata) ->
    decode_boolean_els(__TopXMLNS, __IgnoreEls, _els,
		       Cdata).

encode_boolean({boolean, Cdata}, _xmlns_attrs) ->
    _els = encode_boolean_cdata(Cdata, []),
    _attrs = _xmlns_attrs,
    {xmlel, <<"boolean">>, _attrs, _els}.

decode_boolean_cdata(__TopXMLNS, <<>>) ->
    erlang:error({xmlrpc_codec,
		  {missing_cdata, <<>>, <<"boolean">>, __TopXMLNS}});
decode_boolean_cdata(__TopXMLNS, _val) ->
    case catch dec_bool(_val) of
      {'EXIT', _} ->
	  erlang:error({xmlrpc_codec,
			{bad_cdata_value, <<>>, <<"boolean">>, __TopXMLNS}});
      _res -> _res
    end.

encode_boolean_cdata(_val, _acc) ->
    [{xmlcdata, enc_bool(_val)} | _acc].

decode_dateTime(__TopXMLNS, __IgnoreEls,
		{xmlel, <<"dateTime.iso8601">>, _attrs, _els}) ->
    Cdata = decode_dateTime_els(__TopXMLNS, __IgnoreEls,
				_els, <<>>),
    {date, Cdata}.

decode_dateTime_els(__TopXMLNS, __IgnoreEls, [],
		    Cdata) ->
    decode_dateTime_cdata(__TopXMLNS, Cdata);
decode_dateTime_els(__TopXMLNS, __IgnoreEls,
		    [{xmlcdata, _data} | _els], Cdata) ->
    decode_dateTime_els(__TopXMLNS, __IgnoreEls, _els,
			<<Cdata/binary, _data/binary>>);
decode_dateTime_els(__TopXMLNS, __IgnoreEls, [_ | _els],
		    Cdata) ->
    decode_dateTime_els(__TopXMLNS, __IgnoreEls, _els,
			Cdata).

encode_dateTime({date, Cdata}, _xmlns_attrs) ->
    _els = encode_dateTime_cdata(Cdata, []),
    _attrs = _xmlns_attrs,
    {xmlel, <<"dateTime.iso8601">>, _attrs, _els}.

decode_dateTime_cdata(__TopXMLNS, <<>>) ->
    erlang:error({xmlrpc_codec,
		  {missing_cdata, <<>>, <<"dateTime.iso8601">>,
		   __TopXMLNS}});
decode_dateTime_cdata(__TopXMLNS, _val) -> _val.

encode_dateTime_cdata(_val, _acc) ->
    [{xmlcdata, _val} | _acc].

decode_base64(__TopXMLNS, __IgnoreEls,
	      {xmlel, <<"base64">>, _attrs, _els}) ->
    Cdata = decode_base64_els(__TopXMLNS, __IgnoreEls, _els,
			      <<>>),
    {base64, Cdata}.

decode_base64_els(__TopXMLNS, __IgnoreEls, [], Cdata) ->
    decode_base64_cdata(__TopXMLNS, Cdata);
decode_base64_els(__TopXMLNS, __IgnoreEls,
		  [{xmlcdata, _data} | _els], Cdata) ->
    decode_base64_els(__TopXMLNS, __IgnoreEls, _els,
		      <<Cdata/binary, _data/binary>>);
decode_base64_els(__TopXMLNS, __IgnoreEls, [_ | _els],
		  Cdata) ->
    decode_base64_els(__TopXMLNS, __IgnoreEls, _els, Cdata).

encode_base64({base64, Cdata}, _xmlns_attrs) ->
    _els = encode_base64_cdata(Cdata, []),
    _attrs = _xmlns_attrs,
    {xmlel, <<"base64">>, _attrs, _els}.

decode_base64_cdata(__TopXMLNS, <<>>) ->
    erlang:error({xmlrpc_codec,
		  {missing_cdata, <<>>, <<"base64">>, __TopXMLNS}});
decode_base64_cdata(__TopXMLNS, _val) -> _val.

encode_base64_cdata(_val, _acc) ->
    [{xmlcdata, _val} | _acc].

decode_double(__TopXMLNS, __IgnoreEls,
	      {xmlel, <<"double">>, _attrs, _els}) ->
    Cdata = decode_double_els(__TopXMLNS, __IgnoreEls, _els,
			      <<>>),
    {double, Cdata}.

decode_double_els(__TopXMLNS, __IgnoreEls, [], Cdata) ->
    decode_double_cdata(__TopXMLNS, Cdata);
decode_double_els(__TopXMLNS, __IgnoreEls,
		  [{xmlcdata, _data} | _els], Cdata) ->
    decode_double_els(__TopXMLNS, __IgnoreEls, _els,
		      <<Cdata/binary, _data/binary>>);
decode_double_els(__TopXMLNS, __IgnoreEls, [_ | _els],
		  Cdata) ->
    decode_double_els(__TopXMLNS, __IgnoreEls, _els, Cdata).

encode_double({double, Cdata}, _xmlns_attrs) ->
    _els = encode_double_cdata(Cdata, []),
    _attrs = _xmlns_attrs,
    {xmlel, <<"double">>, _attrs, _els}.

decode_double_cdata(__TopXMLNS, <<>>) ->
    erlang:error({xmlrpc_codec,
		  {missing_cdata, <<>>, <<"double">>, __TopXMLNS}});
decode_double_cdata(__TopXMLNS, _val) ->
    case catch erlang:binary_to_float(_val) of
      {'EXIT', _} ->
	  erlang:error({xmlrpc_codec,
			{bad_cdata_value, <<>>, <<"double">>, __TopXMLNS}});
      _res -> _res
    end.

encode_double_cdata(_val, _acc) ->
    [{xmlcdata, erlang:float_to_binary(_val)} | _acc].

decode_string(__TopXMLNS, __IgnoreEls,
	      {xmlel, <<"string">>, _attrs, _els}) ->
    Cdata = decode_string_els(__TopXMLNS, __IgnoreEls, _els,
			      <<>>),
    {string, Cdata}.

decode_string_els(__TopXMLNS, __IgnoreEls, [], Cdata) ->
    decode_string_cdata(__TopXMLNS, Cdata);
decode_string_els(__TopXMLNS, __IgnoreEls,
		  [{xmlcdata, _data} | _els], Cdata) ->
    decode_string_els(__TopXMLNS, __IgnoreEls, _els,
		      <<Cdata/binary, _data/binary>>);
decode_string_els(__TopXMLNS, __IgnoreEls, [_ | _els],
		  Cdata) ->
    decode_string_els(__TopXMLNS, __IgnoreEls, _els, Cdata).

encode_string({string, Cdata}, _xmlns_attrs) ->
    _els = encode_string_cdata(Cdata, []),
    _attrs = _xmlns_attrs,
    {xmlel, <<"string">>, _attrs, _els}.

decode_string_cdata(__TopXMLNS, <<>>) -> undefined;
decode_string_cdata(__TopXMLNS, _val) -> _val.

encode_string_cdata(undefined, _acc) -> _acc;
encode_string_cdata(_val, _acc) ->
    [{xmlcdata, _val} | _acc].

decode_int(__TopXMLNS, __IgnoreEls,
	   {xmlel, <<"int">>, _attrs, _els}) ->
    Cdata = decode_int_els(__TopXMLNS, __IgnoreEls, _els,
			   <<>>),
    {int, Cdata}.

decode_int_els(__TopXMLNS, __IgnoreEls, [], Cdata) ->
    decode_int_cdata(__TopXMLNS, Cdata);
decode_int_els(__TopXMLNS, __IgnoreEls,
	       [{xmlcdata, _data} | _els], Cdata) ->
    decode_int_els(__TopXMLNS, __IgnoreEls, _els,
		   <<Cdata/binary, _data/binary>>);
decode_int_els(__TopXMLNS, __IgnoreEls, [_ | _els],
	       Cdata) ->
    decode_int_els(__TopXMLNS, __IgnoreEls, _els, Cdata).

encode_int({int, Cdata}, _xmlns_attrs) ->
    _els = encode_int_cdata(Cdata, []),
    _attrs = _xmlns_attrs,
    {xmlel, <<"int">>, _attrs, _els}.

decode_int_cdata(__TopXMLNS, <<>>) ->
    erlang:error({xmlrpc_codec,
		  {missing_cdata, <<>>, <<"int">>, __TopXMLNS}});
decode_int_cdata(__TopXMLNS, _val) ->
    case catch erlang:binary_to_integer(_val) of
      {'EXIT', _} ->
	  erlang:error({xmlrpc_codec,
			{bad_cdata_value, <<>>, <<"int">>, __TopXMLNS}});
      _res -> _res
    end.

encode_int_cdata(_val, _acc) ->
    [{xmlcdata, erlang:integer_to_binary(_val)} | _acc].

decode_i4(__TopXMLNS, __IgnoreEls,
	  {xmlel, <<"i4">>, _attrs, _els}) ->
    Cdata = decode_i4_els(__TopXMLNS, __IgnoreEls, _els,
			  <<>>),
    {i4, Cdata}.

decode_i4_els(__TopXMLNS, __IgnoreEls, [], Cdata) ->
    decode_i4_cdata(__TopXMLNS, Cdata);
decode_i4_els(__TopXMLNS, __IgnoreEls,
	      [{xmlcdata, _data} | _els], Cdata) ->
    decode_i4_els(__TopXMLNS, __IgnoreEls, _els,
		  <<Cdata/binary, _data/binary>>);
decode_i4_els(__TopXMLNS, __IgnoreEls, [_ | _els],
	      Cdata) ->
    decode_i4_els(__TopXMLNS, __IgnoreEls, _els, Cdata).

encode_i4({i4, Cdata}, _xmlns_attrs) ->
    _els = encode_i4_cdata(Cdata, []),
    _attrs = _xmlns_attrs,
    {xmlel, <<"i4">>, _attrs, _els}.

decode_i4_cdata(__TopXMLNS, <<>>) ->
    erlang:error({xmlrpc_codec,
		  {missing_cdata, <<>>, <<"i4">>, __TopXMLNS}});
decode_i4_cdata(__TopXMLNS, _val) ->
    case catch erlang:binary_to_integer(_val) of
      {'EXIT', _} ->
	  erlang:error({xmlrpc_codec,
			{bad_cdata_value, <<>>, <<"i4">>, __TopXMLNS}});
      _res -> _res
    end.

encode_i4_cdata(_val, _acc) ->
    [{xmlcdata, erlang:integer_to_binary(_val)} | _acc].

decode_value(__TopXMLNS, __IgnoreEls,
	     {xmlel, <<"value">>, _attrs, _els}) ->
    Val = decode_value_els(__TopXMLNS, __IgnoreEls, _els,
			   undefined),
    Val.

decode_value_els(__TopXMLNS, __IgnoreEls, [], Val) ->
    Val;
decode_value_els(__TopXMLNS, __IgnoreEls,
		 [{xmlel, <<"i4">>, _attrs, _} = _el | _els], _) ->
    decode_value_els(__TopXMLNS, __IgnoreEls, _els,
		     decode_i4(__TopXMLNS, __IgnoreEls, _el));
decode_value_els(__TopXMLNS, __IgnoreEls,
		 [{xmlel, <<"int">>, _attrs, _} = _el | _els], _) ->
    decode_value_els(__TopXMLNS, __IgnoreEls, _els,
		     decode_int(__TopXMLNS, __IgnoreEls, _el));
decode_value_els(__TopXMLNS, __IgnoreEls,
		 [{xmlel, <<"string">>, _attrs, _} = _el | _els], _) ->
    decode_value_els(__TopXMLNS, __IgnoreEls, _els,
		     decode_string(__TopXMLNS, __IgnoreEls, _el));
decode_value_els(__TopXMLNS, __IgnoreEls,
		 [{xmlel, <<"double">>, _attrs, _} = _el | _els], _) ->
    decode_value_els(__TopXMLNS, __IgnoreEls, _els,
		     decode_double(__TopXMLNS, __IgnoreEls, _el));
decode_value_els(__TopXMLNS, __IgnoreEls,
		 [{xmlel, <<"base64">>, _attrs, _} = _el | _els], _) ->
    decode_value_els(__TopXMLNS, __IgnoreEls, _els,
		     decode_base64(__TopXMLNS, __IgnoreEls, _el));
decode_value_els(__TopXMLNS, __IgnoreEls,
		 [{xmlel, <<"boolean">>, _attrs, _} = _el | _els], _) ->
    decode_value_els(__TopXMLNS, __IgnoreEls, _els,
		     decode_boolean(__TopXMLNS, __IgnoreEls, _el));
decode_value_els(__TopXMLNS, __IgnoreEls,
		 [{xmlel, <<"array">>, _attrs, _} = _el | _els], _) ->
    decode_value_els(__TopXMLNS, __IgnoreEls, _els,
		     decode_array(__TopXMLNS, __IgnoreEls, _el));
decode_value_els(__TopXMLNS, __IgnoreEls,
		 [{xmlel, <<"nil">>, _attrs, _} = _el | _els], _) ->
    decode_value_els(__TopXMLNS, __IgnoreEls, _els,
		     decode_nil(__TopXMLNS, __IgnoreEls, _el));
decode_value_els(__TopXMLNS, __IgnoreEls,
		 [{xmlel, <<"struct">>, _attrs, _} = _el | _els], _) ->
    decode_value_els(__TopXMLNS, __IgnoreEls, _els,
		     decode_struct(__TopXMLNS, __IgnoreEls, _el));
decode_value_els(__TopXMLNS, __IgnoreEls,
		 [{xmlel, <<"dateTime.iso8601">>, _attrs, _} = _el
		  | _els],
		 _) ->
    decode_value_els(__TopXMLNS, __IgnoreEls, _els,
		     decode_dateTime(__TopXMLNS, __IgnoreEls, _el));
decode_value_els(__TopXMLNS, __IgnoreEls, [_ | _els],
		 Val) ->
    decode_value_els(__TopXMLNS, __IgnoreEls, _els, Val).

encode_value(Val, _xmlns_attrs) ->
    _els = lists:reverse('encode_value_$val'(Val, [])),
    _attrs = _xmlns_attrs,
    {xmlel, <<"value">>, _attrs, _els}.

'encode_value_$val'(undefined, _acc) -> _acc;
'encode_value_$val'({i4, _} = Val, _acc) ->
    [encode_i4(Val, []) | _acc];
'encode_value_$val'({int, _} = Val, _acc) ->
    [encode_int(Val, []) | _acc];
'encode_value_$val'({string, _} = Val, _acc) ->
    [encode_string(Val, []) | _acc];
'encode_value_$val'({double, _} = Val, _acc) ->
    [encode_double(Val, []) | _acc];
'encode_value_$val'({base64, _} = Val, _acc) ->
    [encode_base64(Val, []) | _acc];
'encode_value_$val'({boolean, _} = Val, _acc) ->
    [encode_boolean(Val, []) | _acc];
'encode_value_$val'({array, _} = Val, _acc) ->
    [encode_array(Val, []) | _acc];
'encode_value_$val'(nil = Val, _acc) ->
    [encode_nil(Val, []) | _acc];
'encode_value_$val'({struct, _} = Val, _acc) ->
    [encode_struct(Val, []) | _acc];
'encode_value_$val'({date, _} = Val, _acc) ->
    [encode_dateTime(Val, []) | _acc].

decode_param(__TopXMLNS, __IgnoreEls,
	     {xmlel, <<"param">>, _attrs, _els}) ->
    Value = decode_param_els(__TopXMLNS, __IgnoreEls, _els,
			     error),
    Value.

decode_param_els(__TopXMLNS, __IgnoreEls, [], Value) ->
    case Value of
      error ->
	  erlang:error({xmlrpc_codec,
			{missing_tag, <<"value">>, __TopXMLNS}});
      {value, Value1} -> Value1
    end;
decode_param_els(__TopXMLNS, __IgnoreEls,
		 [{xmlel, <<"value">>, _attrs, _} = _el | _els], _) ->
    decode_param_els(__TopXMLNS, __IgnoreEls, _els,
		     {value, decode_value(__TopXMLNS, __IgnoreEls, _el)});
decode_param_els(__TopXMLNS, __IgnoreEls, [_ | _els],
		 Value) ->
    decode_param_els(__TopXMLNS, __IgnoreEls, _els, Value).

encode_param(Value, _xmlns_attrs) ->
    _els = lists:reverse('encode_param_$value'(Value, [])),
    _attrs = _xmlns_attrs,
    {xmlel, <<"param">>, _attrs, _els}.

'encode_param_$value'(Value, _acc) ->
    [encode_value(Value, []) | _acc].

decode_params(__TopXMLNS, __IgnoreEls,
	      {xmlel, <<"params">>, _attrs, _els}) ->
    Params = decode_params_els(__TopXMLNS, __IgnoreEls,
			       _els, []),
    Params.

decode_params_els(__TopXMLNS, __IgnoreEls, [],
		  Params) ->
    lists:reverse(Params);
decode_params_els(__TopXMLNS, __IgnoreEls,
		  [{xmlel, <<"param">>, _attrs, _} = _el | _els],
		  Params) ->
    decode_params_els(__TopXMLNS, __IgnoreEls, _els,
		      case decode_param(__TopXMLNS, __IgnoreEls, _el) of
			undefined -> Params;
			_new_el -> [_new_el | Params]
		      end);
decode_params_els(__TopXMLNS, __IgnoreEls, [_ | _els],
		  Params) ->
    decode_params_els(__TopXMLNS, __IgnoreEls, _els,
		      Params).

encode_params(Params, _xmlns_attrs) ->
    _els = lists:reverse('encode_params_$params'(Params,
						 [])),
    _attrs = _xmlns_attrs,
    {xmlel, <<"params">>, _attrs, _els}.

'encode_params_$params'([], _acc) -> _acc;
'encode_params_$params'([Params | _els], _acc) ->
    'encode_params_$params'(_els,
			    [encode_param(Params, []) | _acc]).

decode_methodName(__TopXMLNS, __IgnoreEls,
		  {xmlel, <<"methodName">>, _attrs, _els}) ->
    Cdata = decode_methodName_els(__TopXMLNS, __IgnoreEls,
				  _els, <<>>),
    Cdata.

decode_methodName_els(__TopXMLNS, __IgnoreEls, [],
		      Cdata) ->
    decode_methodName_cdata(__TopXMLNS, Cdata);
decode_methodName_els(__TopXMLNS, __IgnoreEls,
		      [{xmlcdata, _data} | _els], Cdata) ->
    decode_methodName_els(__TopXMLNS, __IgnoreEls, _els,
			  <<Cdata/binary, _data/binary>>);
decode_methodName_els(__TopXMLNS, __IgnoreEls,
		      [_ | _els], Cdata) ->
    decode_methodName_els(__TopXMLNS, __IgnoreEls, _els,
			  Cdata).

encode_methodName(Cdata, _xmlns_attrs) ->
    _els = encode_methodName_cdata(Cdata, []),
    _attrs = _xmlns_attrs,
    {xmlel, <<"methodName">>, _attrs, _els}.

decode_methodName_cdata(__TopXMLNS, <<>>) ->
    erlang:error({xmlrpc_codec,
		  {missing_cdata, <<>>, <<"methodName">>, __TopXMLNS}});
decode_methodName_cdata(__TopXMLNS, _val) ->
    case catch erlang:binary_to_atom(_val, utf8) of
      {'EXIT', _} ->
	  erlang:error({xmlrpc_codec,
			{bad_cdata_value, <<>>, <<"methodName">>, __TopXMLNS}});
      _res -> _res
    end.

encode_methodName_cdata(_val, _acc) ->
    [{xmlcdata, erlang:atom_to_binary(_val, utf8)} | _acc].

decode_fault(__TopXMLNS, __IgnoreEls,
	     {xmlel, <<"fault">>, _attrs, _els}) ->
    Value = decode_fault_els(__TopXMLNS, __IgnoreEls, _els,
			     error),
    {fault, Value}.

decode_fault_els(__TopXMLNS, __IgnoreEls, [], Value) ->
    case Value of
      error ->
	  erlang:error({xmlrpc_codec,
			{missing_tag, <<"value">>, __TopXMLNS}});
      {value, Value1} -> Value1
    end;
decode_fault_els(__TopXMLNS, __IgnoreEls,
		 [{xmlel, <<"value">>, _attrs, _} = _el | _els], _) ->
    decode_fault_els(__TopXMLNS, __IgnoreEls, _els,
		     {value, decode_value(__TopXMLNS, __IgnoreEls, _el)});
decode_fault_els(__TopXMLNS, __IgnoreEls, [_ | _els],
		 Value) ->
    decode_fault_els(__TopXMLNS, __IgnoreEls, _els, Value).

encode_fault({fault, Value}, _xmlns_attrs) ->
    _els = lists:reverse('encode_fault_$value'(Value, [])),
    _attrs = _xmlns_attrs,
    {xmlel, <<"fault">>, _attrs, _els}.

'encode_fault_$value'(Value, _acc) ->
    [encode_value(Value, []) | _acc].

decode_methodResponse(__TopXMLNS, __IgnoreEls,
		      {xmlel, <<"methodResponse">>, _attrs, _els}) ->
    Payload = decode_methodResponse_els(__TopXMLNS,
					__IgnoreEls, _els, undefined),
    {response, Payload}.

decode_methodResponse_els(__TopXMLNS, __IgnoreEls, [],
			  Payload) ->
    Payload;
decode_methodResponse_els(__TopXMLNS, __IgnoreEls,
			  [{xmlel, <<"fault">>, _attrs, _} = _el | _els], _) ->
    decode_methodResponse_els(__TopXMLNS, __IgnoreEls, _els,
			      decode_fault(__TopXMLNS, __IgnoreEls, _el));
decode_methodResponse_els(__TopXMLNS, __IgnoreEls,
			  [{xmlel, <<"params">>, _attrs, _} = _el | _els], _) ->
    decode_methodResponse_els(__TopXMLNS, __IgnoreEls, _els,
			      decode_params(__TopXMLNS, __IgnoreEls, _el));
decode_methodResponse_els(__TopXMLNS, __IgnoreEls,
			  [_ | _els], Payload) ->
    decode_methodResponse_els(__TopXMLNS, __IgnoreEls, _els,
			      Payload).

encode_methodResponse({response, Payload},
		      _xmlns_attrs) ->
    _els =
	lists:reverse('encode_methodResponse_$payload'(Payload,
						       [])),
    _attrs = _xmlns_attrs,
    {xmlel, <<"methodResponse">>, _attrs, _els}.

'encode_methodResponse_$payload'(undefined, _acc) ->
    _acc;
'encode_methodResponse_$payload'({fault, _} = Payload,
				 _acc) ->
    [encode_fault(Payload, []) | _acc];
'encode_methodResponse_$payload'(_ = Payload, _acc) ->
    [encode_params(Payload, []) | _acc].

decode_methodCall(__TopXMLNS, __IgnoreEls,
		  {xmlel, <<"methodCall">>, _attrs, _els}) ->
    {Params, Name} = decode_methodCall_els(__TopXMLNS,
					   __IgnoreEls, _els, undefined, error),
    {call, Name, Params}.

decode_methodCall_els(__TopXMLNS, __IgnoreEls, [],
		      Params, Name) ->
    {Params,
     case Name of
       error ->
	   erlang:error({xmlrpc_codec,
			 {missing_tag, <<"methodName">>, __TopXMLNS}});
       {value, Name1} -> Name1
     end};
decode_methodCall_els(__TopXMLNS, __IgnoreEls,
		      [{xmlel, <<"methodName">>, _attrs, _} = _el | _els],
		      Params, _) ->
    decode_methodCall_els(__TopXMLNS, __IgnoreEls, _els,
			  Params,
			  {value,
			   decode_methodName(__TopXMLNS, __IgnoreEls, _el)});
decode_methodCall_els(__TopXMLNS, __IgnoreEls,
		      [{xmlel, <<"params">>, _attrs, _} = _el | _els], _,
		      Name) ->
    decode_methodCall_els(__TopXMLNS, __IgnoreEls, _els,
			  decode_params(__TopXMLNS, __IgnoreEls, _el), Name);
decode_methodCall_els(__TopXMLNS, __IgnoreEls,
		      [_ | _els], Params, Name) ->
    decode_methodCall_els(__TopXMLNS, __IgnoreEls, _els,
			  Params, Name).

encode_methodCall({call, Name, Params}, _xmlns_attrs) ->
    _els = lists:reverse('encode_methodCall_$params'(Params,
						     'encode_methodCall_$name'(Name,
									       []))),
    _attrs = _xmlns_attrs,
    {xmlel, <<"methodCall">>, _attrs, _els}.

'encode_methodCall_$params'(undefined, _acc) -> _acc;
'encode_methodCall_$params'(Params, _acc) ->
    [encode_params(Params, []) | _acc].

'encode_methodCall_$name'(Name, _acc) ->
    [encode_methodName(Name, []) | _acc].
