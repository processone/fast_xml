%% Created automatically by XML generator (fxml_gen.erl)
%% Source: fxmlrpc_codec.spec

-module(fxmlrpc_codec).

-compile(export_all).

decode(El) -> decode(El, <<>>, []).

decode(El, Opts) -> decode(El, <<>>, Opts).

decode({xmlel, Name, Attrs, _} = El, TopXMLNS, Opts) ->
    XMLNS = get_attr(<<"xmlns">>, Attrs, TopXMLNS),
    case get_mod(Name, XMLNS) of
      undefined when XMLNS == <<>> ->
	  erlang:error({fxmlrpc_codec,
			{missing_tag_xmlns, Name}});
      undefined ->
	  erlang:error({fxmlrpc_codec,
			{unknown_tag, Name, XMLNS}});
      Mod -> Mod:do_decode(Name, XMLNS, El, Opts)
    end.

encode(El) -> encode(El, <<>>).

encode({xmlel, _, _, _} = El, _) -> El;
encode({xmlcdata, _} = CData, _) -> CData;
encode(El, TopXMLNS) ->
    Mod = get_mod(El), Mod:do_encode(El, TopXMLNS).

get_name(El) -> Mod = get_mod(El), Mod:do_get_name(El).

get_ns(El) -> Mod = get_mod(El), Mod:do_get_ns(El).

is_known_tag({xmlel, Name, Attrs, _}, TopXMLNS) ->
    XMLNS = get_attr(<<"xmlns">>, Attrs, TopXMLNS),
    get_mod(Name, XMLNS) /= undefined.

get_els(Term) -> Mod = get_mod(Term), Mod:get_els(Term).

set_els(Term, Els) ->
    Mod = get_mod(Term), Mod:set_els(Term, Els).

do_decode(<<"name">>, <<"xmlrpc">>, El, Opts) ->
    decode_name(<<"xmlrpc">>, Opts, El);
do_decode(<<"member">>, <<"xmlrpc">>, El, Opts) ->
    decode_member(<<"xmlrpc">>, Opts, El);
do_decode(<<"struct">>, <<"xmlrpc">>, El, Opts) ->
    decode_struct(<<"xmlrpc">>, Opts, El);
do_decode(<<"nil">>, <<"xmlrpc">>, El, Opts) ->
    decode_nil(<<"xmlrpc">>, Opts, El);
do_decode(<<"data">>, <<"xmlrpc">>, El, Opts) ->
    decode_data(<<"xmlrpc">>, Opts, El);
do_decode(<<"array">>, <<"xmlrpc">>, El, Opts) ->
    decode_array(<<"xmlrpc">>, Opts, El);
do_decode(<<"boolean">>, <<"xmlrpc">>, El, Opts) ->
    decode_boolean(<<"xmlrpc">>, Opts, El);
do_decode(<<"dateTime.iso8601">>, <<"xmlrpc">>, El,
	  Opts) ->
    decode_dateTime(<<"xmlrpc">>, Opts, El);
do_decode(<<"base64">>, <<"xmlrpc">>, El, Opts) ->
    decode_base64(<<"xmlrpc">>, Opts, El);
do_decode(<<"double">>, <<"xmlrpc">>, El, Opts) ->
    decode_double(<<"xmlrpc">>, Opts, El);
do_decode(<<"string">>, <<"xmlrpc">>, El, Opts) ->
    decode_string(<<"xmlrpc">>, Opts, El);
do_decode(<<"int">>, <<"xmlrpc">>, El, Opts) ->
    decode_int(<<"xmlrpc">>, Opts, El);
do_decode(<<"i4">>, <<"xmlrpc">>, El, Opts) ->
    decode_i4(<<"xmlrpc">>, Opts, El);
do_decode(<<"value">>, <<"xmlrpc">>, El, Opts) ->
    decode_value(<<"xmlrpc">>, Opts, El);
do_decode(<<"param">>, <<"xmlrpc">>, El, Opts) ->
    decode_param(<<"xmlrpc">>, Opts, El);
do_decode(<<"params">>, <<"xmlrpc">>, El, Opts) ->
    decode_params(<<"xmlrpc">>, Opts, El);
do_decode(<<"methodName">>, <<"xmlrpc">>, El, Opts) ->
    decode_methodName(<<"xmlrpc">>, Opts, El);
do_decode(<<"fault">>, <<"xmlrpc">>, El, Opts) ->
    decode_fault(<<"xmlrpc">>, Opts, El);
do_decode(<<"methodResponse">>, <<"xmlrpc">>, El,
	  Opts) ->
    decode_methodResponse(<<"xmlrpc">>, Opts, El);
do_decode(<<"methodCall">>, <<"xmlrpc">>, El, Opts) ->
    decode_methodCall(<<"xmlrpc">>, Opts, El);
do_decode(Name, <<>>, _, _) ->
    erlang:error({fxmlrpc_codec,
		  {missing_tag_xmlns, Name}});
do_decode(Name, XMLNS, _, _) ->
    erlang:error({fxmlrpc_codec,
		  {unknown_tag, Name, XMLNS}}).

tags() ->
    [{<<"name">>, <<"xmlrpc">>},
     {<<"member">>, <<"xmlrpc">>},
     {<<"struct">>, <<"xmlrpc">>}, {<<"nil">>, <<"xmlrpc">>},
     {<<"data">>, <<"xmlrpc">>}, {<<"array">>, <<"xmlrpc">>},
     {<<"boolean">>, <<"xmlrpc">>},
     {<<"dateTime.iso8601">>, <<"xmlrpc">>},
     {<<"base64">>, <<"xmlrpc">>},
     {<<"double">>, <<"xmlrpc">>},
     {<<"string">>, <<"xmlrpc">>}, {<<"int">>, <<"xmlrpc">>},
     {<<"i4">>, <<"xmlrpc">>}, {<<"value">>, <<"xmlrpc">>},
     {<<"param">>, <<"xmlrpc">>},
     {<<"params">>, <<"xmlrpc">>},
     {<<"methodName">>, <<"xmlrpc">>},
     {<<"fault">>, <<"xmlrpc">>},
     {<<"methodResponse">>, <<"xmlrpc">>},
     {<<"methodCall">>, <<"xmlrpc">>}].

do_encode({call, _, _} = Methodcall, TopXMLNS) ->
    encode_methodCall(Methodcall, TopXMLNS);
do_encode({response, _} = Methodresponse, TopXMLNS) ->
    encode_methodResponse(Methodresponse, TopXMLNS);
do_encode({fault, _} = Fault, TopXMLNS) ->
    encode_fault(Fault, TopXMLNS);
do_encode({i4, _} = I4, TopXMLNS) ->
    encode_i4(I4, TopXMLNS);
do_encode({int, _} = Int, TopXMLNS) ->
    encode_int(Int, TopXMLNS);
do_encode({string, _} = String, TopXMLNS) ->
    encode_string(String, TopXMLNS);
do_encode({double, _} = Double, TopXMLNS) ->
    encode_double(Double, TopXMLNS);
do_encode({base64, _} = Base64, TopXMLNS) ->
    encode_base64(Base64, TopXMLNS);
do_encode({date, _} = Datetime_iso8601, TopXMLNS) ->
    encode_dateTime(Datetime_iso8601, TopXMLNS);
do_encode({boolean, _} = Boolean, TopXMLNS) ->
    encode_boolean(Boolean, TopXMLNS);
do_encode({array, _} = Array, TopXMLNS) ->
    encode_array(Array, TopXMLNS);
do_encode({struct, _} = Struct, TopXMLNS) ->
    encode_struct(Struct, TopXMLNS).

do_get_name({array, _}) -> <<"array">>;
do_get_name({base64, _}) -> <<"base64">>;
do_get_name({boolean, _}) -> <<"boolean">>;
do_get_name({call, _, _}) -> <<"methodCall">>;
do_get_name({date, _}) -> <<"dateTime.iso8601">>;
do_get_name({double, _}) -> <<"double">>;
do_get_name({fault, _}) -> <<"fault">>;
do_get_name({i4, _}) -> <<"i4">>;
do_get_name({int, _}) -> <<"int">>;
do_get_name({response, _}) -> <<"methodResponse">>;
do_get_name({string, _}) -> <<"string">>;
do_get_name({struct, _}) -> <<"struct">>.

do_get_ns({array, _}) -> <<"xmlrpc">>;
do_get_ns({base64, _}) -> <<"xmlrpc">>;
do_get_ns({boolean, _}) -> <<"xmlrpc">>;
do_get_ns({call, _, _}) -> <<"xmlrpc">>;
do_get_ns({date, _}) -> <<"xmlrpc">>;
do_get_ns({double, _}) -> <<"xmlrpc">>;
do_get_ns({fault, _}) -> <<"xmlrpc">>;
do_get_ns({i4, _}) -> <<"xmlrpc">>;
do_get_ns({int, _}) -> <<"xmlrpc">>;
do_get_ns({response, _}) -> <<"xmlrpc">>;
do_get_ns({string, _}) -> <<"xmlrpc">>;
do_get_ns({struct, _}) -> <<"xmlrpc">>.

register_module(Mod) ->
    register_module(Mod, fxmlrpc_codec_external).

unregister_module(Mod) ->
    unregister_module(Mod, fxmlrpc_codec_external).

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
      "/> qualified by namespace '", XMLNS/binary, "'">>;
format_error({missing_tag_xmlns, Tag}) ->
    <<"Missing namespace for tag <", Tag/binary, "/>">>.

io_format_error({bad_attr_value, Attr, Tag, XMLNS}) ->
    {<<"Bad value of attribute '~s' in tag <~s/> "
       "qualified by namespace '~s'">>,
     [Attr, Tag, XMLNS]};
io_format_error({bad_cdata_value, <<>>, Tag, XMLNS}) ->
    {<<"Bad value of cdata in tag <~s/> qualified "
       "by namespace '~s'">>,
     [Tag, XMLNS]};
io_format_error({missing_tag, Tag, XMLNS}) ->
    {<<"Missing tag <~s/> qualified by namespace "
       "'~s'">>,
     [Tag, XMLNS]};
io_format_error({missing_attr, Attr, Tag, XMLNS}) ->
    {<<"Missing attribute '~s' in tag <~s/> "
       "qualified by namespace '~s'">>,
     [Attr, Tag, XMLNS]};
io_format_error({missing_cdata, <<>>, Tag, XMLNS}) ->
    {<<"Missing cdata in tag <~s/> qualified "
       "by namespace '~s'">>,
     [Tag, XMLNS]};
io_format_error({unknown_tag, Tag, XMLNS}) ->
    {<<"Unknown tag <~s/> qualified by namespace "
       "'~s'">>,
     [Tag, XMLNS]};
io_format_error({missing_tag_xmlns, Tag}) ->
    {<<"Missing namespace for tag <~s/>">>, [Tag]}.

get_attr(Attr, Attrs, Default) ->
    case lists:keyfind(Attr, 1, Attrs) of
      {_, Val} -> Val;
      false -> Default
    end.

enc_xmlns_attrs(XMLNS, XMLNS) -> [];
enc_xmlns_attrs(XMLNS, _) -> [{<<"xmlns">>, XMLNS}].

choose_top_xmlns(<<>>, NSList, TopXMLNS) ->
    case lists:member(TopXMLNS, NSList) of
      true -> TopXMLNS;
      false -> hd(NSList)
    end;
choose_top_xmlns(XMLNS, _, _) -> XMLNS.

register_module(Mod, ResolverMod) ->
    MD5Sum = try Mod:module_info(md5) of
	       Val -> Val
	     catch
	       error:badarg ->
		   {ok, {Mod, Val}} = beam_lib:md5(code:which(Mod)), Val
	     end,
    case orddict:find(Mod, ResolverMod:modules()) of
      {ok, MD5Sum} -> ok;
      _ ->
	  Mods = orddict:store(Mod, MD5Sum,
			       ResolverMod:modules()),
	  recompile_resolver(Mods, ResolverMod)
    end.

unregister_module(Mod, ResolverMod) ->
    case orddict:find(Mod, ResolverMod:modules()) of
      {ok, _} ->
	  Mods = orddict:erase(Mod, ResolverMod:modules()),
	  recompile_resolver(Mods, ResolverMod);
      error -> ok
    end.

recompile_resolver(Mods, ResolverMod) ->
    Tags = lists:flatmap(fun (M) ->
				 [{Name, XMLNS, M} || {Name, XMLNS} <- M:tags()]
			 end,
			 orddict:fetch_keys(Mods)),
    Records = lists:flatmap(fun (M) ->
				    [{RecName, RecSize, M}
				     || {RecName, RecSize} <- M:records()]
			    end,
			    orddict:fetch_keys(Mods)),
    Lookup1 = string:join(lists:map(fun ({RecName, RecSize,
					  M}) ->
					    io_lib:format("lookup({~s}) -> '~s'",
							  [string:join([io_lib:format("'~s'",
										      [RecName])
									| ["_"
									   || _
										  <- lists:seq(1,
											       RecSize)]],
								       ","),
							   M])
				    end,
				    Records)
			    ++ ["lookup(_) -> erlang:error(badarg)."],
			  ";" ++ io_lib:nl()),
    Lookup2 = string:join(lists:map(fun ({Name, XMLNS,
					  M}) ->
					    io_lib:format("lookup(~w, ~w) -> '~s'",
							  [Name, XMLNS, M])
				    end,
				    Tags)
			    ++ ["lookup(_, _) -> undefined."],
			  ";" ++ io_lib:nl()),
    Modules = io_lib:format("modules() -> [~s].",
			    [string:join([io_lib:format("{'~s', ~w}", [M, S])
					  || {M, S} <- Mods],
					 ",")]),
    Module = io_lib:format("-module(~s).", [ResolverMod]),
    Compile = "-compile(export_all).",
    Forms = lists:map(fun (Expr) ->
			      {ok, Tokens, _} =
				  erl_scan:string(lists:flatten(Expr)),
			      {ok, Form} = erl_parse:parse_form(Tokens),
			      Form
		      end,
		      [Module, Compile, Modules, Lookup1, Lookup2]),
    {ok, Code} = case compile:forms(Forms, []) of
		   {ok, ResolverMod, Bin} -> {ok, Bin};
		   {ok, ResolverMod, Bin, _Warnings} -> {ok, Bin};
		   Error -> Error
		 end,
    {module, ResolverMod} = code:load_binary(ResolverMod,
					     "nofile", Code),
    ok.

dec_bool(<<"false">>) -> false;
dec_bool(<<"0">>) -> false;
dec_bool(<<"true">>) -> true;
dec_bool(<<"1">>) -> true.

enc_bool(false) -> <<"0">>;
enc_bool(true) -> <<"1">>.

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
pp(xmlel, 3) -> [name, attrs, children];
pp(Name, Arity) ->
    try get_mod(erlang:make_tuple(Arity + 1, undefined,
				  [{1, Name}]))
    of
      Mod -> Mod:pp(Name, Arity)
    catch
      error:badarg -> no
    end.

records() ->
    [{call, 2}, {response, 1}, {fault, 1}, {i4, 1},
     {int, 1}, {string, 1}, {double, 1}, {base64, 1},
     {date, 1}, {boolean, 1}, {array, 1}, {struct, 1}].

get_mod(<<"int">>, <<"xmlrpc">>) -> fxmlrpc_codec;
get_mod(<<"array">>, <<"xmlrpc">>) -> fxmlrpc_codec;
get_mod(<<"dateTime.iso8601">>, <<"xmlrpc">>) ->
    fxmlrpc_codec;
get_mod(<<"methodResponse">>, <<"xmlrpc">>) ->
    fxmlrpc_codec;
get_mod(<<"nil">>, <<"xmlrpc">>) -> fxmlrpc_codec;
get_mod(<<"value">>, <<"xmlrpc">>) -> fxmlrpc_codec;
get_mod(<<"methodCall">>, <<"xmlrpc">>) ->
    fxmlrpc_codec;
get_mod(<<"string">>, <<"xmlrpc">>) -> fxmlrpc_codec;
get_mod(<<"boolean">>, <<"xmlrpc">>) -> fxmlrpc_codec;
get_mod(<<"name">>, <<"xmlrpc">>) -> fxmlrpc_codec;
get_mod(<<"param">>, <<"xmlrpc">>) -> fxmlrpc_codec;
get_mod(<<"data">>, <<"xmlrpc">>) -> fxmlrpc_codec;
get_mod(<<"i4">>, <<"xmlrpc">>) -> fxmlrpc_codec;
get_mod(<<"base64">>, <<"xmlrpc">>) -> fxmlrpc_codec;
get_mod(<<"member">>, <<"xmlrpc">>) -> fxmlrpc_codec;
get_mod(<<"fault">>, <<"xmlrpc">>) -> fxmlrpc_codec;
get_mod(<<"methodName">>, <<"xmlrpc">>) ->
    fxmlrpc_codec;
get_mod(<<"params">>, <<"xmlrpc">>) -> fxmlrpc_codec;
get_mod(<<"double">>, <<"xmlrpc">>) -> fxmlrpc_codec;
get_mod(<<"struct">>, <<"xmlrpc">>) -> fxmlrpc_codec;
get_mod(Name, XMLNS) ->
    fxmlrpc_codec_external:lookup(Name, XMLNS).

get_mod({boolean, _}) -> fxmlrpc_codec;
get_mod({fault, _}) -> fxmlrpc_codec;
get_mod({i4, _}) -> fxmlrpc_codec;
get_mod({int, _}) -> fxmlrpc_codec;
get_mod({base64, _}) -> fxmlrpc_codec;
get_mod({struct, _}) -> fxmlrpc_codec;
get_mod({call, _, _}) -> fxmlrpc_codec;
get_mod({response, _}) -> fxmlrpc_codec;
get_mod({double, _}) -> fxmlrpc_codec;
get_mod({date, _}) -> fxmlrpc_codec;
get_mod({string, _}) -> fxmlrpc_codec;
get_mod({array, _}) -> fxmlrpc_codec;
get_mod(Record) ->
    fxmlrpc_codec_external:lookup(Record).

decode_name(__TopXMLNS, __Opts,
	    {xmlel, <<"name">>, _attrs, _els}) ->
    Cdata = decode_name_els(__TopXMLNS, __Opts, _els, <<>>),
    Cdata.

decode_name_els(__TopXMLNS, __Opts, [], Cdata) ->
    decode_name_cdata(__TopXMLNS, Cdata);
decode_name_els(__TopXMLNS, __Opts,
		[{xmlcdata, _data} | _els], Cdata) ->
    decode_name_els(__TopXMLNS, __Opts, _els,
		    <<Cdata/binary, _data/binary>>);
decode_name_els(__TopXMLNS, __Opts, [_ | _els],
		Cdata) ->
    decode_name_els(__TopXMLNS, __Opts, _els, Cdata).

encode_name(Cdata, __TopXMLNS) ->
    __NewTopXMLNS =
	fxmlrpc_codec:choose_top_xmlns(<<"xmlrpc">>, [],
				       __TopXMLNS),
    _els = encode_name_cdata(Cdata, []),
    _attrs = fxmlrpc_codec:enc_xmlns_attrs(__NewTopXMLNS,
					   __TopXMLNS),
    {xmlel, <<"name">>, _attrs, _els}.

decode_name_cdata(__TopXMLNS, <<>>) ->
    erlang:error({fxmlrpc_codec,
		  {missing_cdata, <<>>, <<"name">>, __TopXMLNS}});
decode_name_cdata(__TopXMLNS, _val) ->
    case catch erlang:binary_to_atom(_val, utf8) of
      {'EXIT', _} ->
	  erlang:error({fxmlrpc_codec,
			{bad_cdata_value, <<>>, <<"name">>, __TopXMLNS}});
      _res -> _res
    end.

encode_name_cdata(_val, _acc) ->
    [{xmlcdata, erlang:atom_to_binary(_val, utf8)} | _acc].

decode_member(__TopXMLNS, __Opts,
	      {xmlel, <<"member">>, _attrs, _els}) ->
    {Value, Name} = decode_member_els(__TopXMLNS, __Opts,
				      _els, error, error),
    {Name, Value}.

decode_member_els(__TopXMLNS, __Opts, [], Value,
		  Name) ->
    {case Value of
       error ->
	   erlang:error({fxmlrpc_codec,
			 {missing_tag, <<"value">>, __TopXMLNS}});
       {value, Value1} -> Value1
     end,
     case Name of
       error ->
	   erlang:error({fxmlrpc_codec,
			 {missing_tag, <<"name">>, __TopXMLNS}});
       {value, Name1} -> Name1
     end};
decode_member_els(__TopXMLNS, __Opts,
		  [{xmlel, <<"name">>, _attrs, _} = _el | _els], Value,
		  Name) ->
    case fxmlrpc_codec:get_attr(<<"xmlns">>, _attrs,
				__TopXMLNS)
	of
      <<"xmlrpc">> ->
	  decode_member_els(__TopXMLNS, __Opts, _els, Value,
			    {value, decode_name(<<"xmlrpc">>, __Opts, _el)});
      _ ->
	  decode_member_els(__TopXMLNS, __Opts, _els, Value, Name)
    end;
decode_member_els(__TopXMLNS, __Opts,
		  [{xmlel, <<"value">>, _attrs, _} = _el | _els], Value,
		  Name) ->
    case fxmlrpc_codec:get_attr(<<"xmlns">>, _attrs,
				__TopXMLNS)
	of
      <<"xmlrpc">> ->
	  decode_member_els(__TopXMLNS, __Opts, _els,
			    {value, decode_value(<<"xmlrpc">>, __Opts, _el)},
			    Name);
      _ ->
	  decode_member_els(__TopXMLNS, __Opts, _els, Value, Name)
    end;
decode_member_els(__TopXMLNS, __Opts, [_ | _els], Value,
		  Name) ->
    decode_member_els(__TopXMLNS, __Opts, _els, Value,
		      Name).

encode_member({Name, Value}, __TopXMLNS) ->
    __NewTopXMLNS =
	fxmlrpc_codec:choose_top_xmlns(<<"xmlrpc">>, [],
				       __TopXMLNS),
    _els = lists:reverse('encode_member_$value'(Value,
						__NewTopXMLNS,
						'encode_member_$name'(Name,
								      __NewTopXMLNS,
								      []))),
    _attrs = fxmlrpc_codec:enc_xmlns_attrs(__NewTopXMLNS,
					   __TopXMLNS),
    {xmlel, <<"member">>, _attrs, _els}.

'encode_member_$value'(Value, __TopXMLNS, _acc) ->
    [encode_value(Value, __TopXMLNS) | _acc].

'encode_member_$name'(Name, __TopXMLNS, _acc) ->
    [encode_name(Name, __TopXMLNS) | _acc].

decode_struct(__TopXMLNS, __Opts,
	      {xmlel, <<"struct">>, _attrs, _els}) ->
    Members = decode_struct_els(__TopXMLNS, __Opts, _els,
				[]),
    {struct, Members}.

decode_struct_els(__TopXMLNS, __Opts, [], Members) ->
    lists:reverse(Members);
decode_struct_els(__TopXMLNS, __Opts,
		  [{xmlel, <<"member">>, _attrs, _} = _el | _els],
		  Members) ->
    case fxmlrpc_codec:get_attr(<<"xmlns">>, _attrs,
				__TopXMLNS)
	of
      <<"xmlrpc">> ->
	  decode_struct_els(__TopXMLNS, __Opts, _els,
			    [decode_member(<<"xmlrpc">>, __Opts, _el)
			     | Members]);
      _ ->
	  decode_struct_els(__TopXMLNS, __Opts, _els, Members)
    end;
decode_struct_els(__TopXMLNS, __Opts, [_ | _els],
		  Members) ->
    decode_struct_els(__TopXMLNS, __Opts, _els, Members).

encode_struct({struct, Members}, __TopXMLNS) ->
    __NewTopXMLNS =
	fxmlrpc_codec:choose_top_xmlns(<<"xmlrpc">>, [],
				       __TopXMLNS),
    _els = lists:reverse('encode_struct_$members'(Members,
						  __NewTopXMLNS, [])),
    _attrs = fxmlrpc_codec:enc_xmlns_attrs(__NewTopXMLNS,
					   __TopXMLNS),
    {xmlel, <<"struct">>, _attrs, _els}.

'encode_struct_$members'([], __TopXMLNS, _acc) -> _acc;
'encode_struct_$members'([Members | _els], __TopXMLNS,
			 _acc) ->
    'encode_struct_$members'(_els, __TopXMLNS,
			     [encode_member(Members, __TopXMLNS) | _acc]).

decode_nil(__TopXMLNS, __Opts,
	   {xmlel, <<"nil">>, _attrs, _els}) ->
    nil.

encode_nil(nil, __TopXMLNS) ->
    __NewTopXMLNS =
	fxmlrpc_codec:choose_top_xmlns(<<"xmlrpc">>, [],
				       __TopXMLNS),
    _els = [],
    _attrs = fxmlrpc_codec:enc_xmlns_attrs(__NewTopXMLNS,
					   __TopXMLNS),
    {xmlel, <<"nil">>, _attrs, _els}.

decode_data(__TopXMLNS, __Opts,
	    {xmlel, <<"data">>, _attrs, _els}) ->
    V = decode_data_els(__TopXMLNS, __Opts, _els, []), V.

decode_data_els(__TopXMLNS, __Opts, [], V) ->
    lists:reverse(V);
decode_data_els(__TopXMLNS, __Opts,
		[{xmlel, <<"value">>, _attrs, _} = _el | _els], V) ->
    case fxmlrpc_codec:get_attr(<<"xmlns">>, _attrs,
				__TopXMLNS)
	of
      <<"xmlrpc">> ->
	  decode_data_els(__TopXMLNS, __Opts, _els,
			  [decode_value(<<"xmlrpc">>, __Opts, _el) | V]);
      _ -> decode_data_els(__TopXMLNS, __Opts, _els, V)
    end;
decode_data_els(__TopXMLNS, __Opts, [_ | _els], V) ->
    decode_data_els(__TopXMLNS, __Opts, _els, V).

encode_data(V, __TopXMLNS) ->
    __NewTopXMLNS =
	fxmlrpc_codec:choose_top_xmlns(<<"xmlrpc">>, [],
				       __TopXMLNS),
    _els = lists:reverse('encode_data_$v'(V, __NewTopXMLNS,
					  [])),
    _attrs = fxmlrpc_codec:enc_xmlns_attrs(__NewTopXMLNS,
					   __TopXMLNS),
    {xmlel, <<"data">>, _attrs, _els}.

'encode_data_$v'([], __TopXMLNS, _acc) -> _acc;
'encode_data_$v'([V | _els], __TopXMLNS, _acc) ->
    'encode_data_$v'(_els, __TopXMLNS,
		     [encode_value(V, __TopXMLNS) | _acc]).

decode_array(__TopXMLNS, __Opts,
	     {xmlel, <<"array">>, _attrs, _els}) ->
    Data = decode_array_els(__TopXMLNS, __Opts, _els,
			    error),
    {array, Data}.

decode_array_els(__TopXMLNS, __Opts, [], Data) ->
    case Data of
      error ->
	  erlang:error({fxmlrpc_codec,
			{missing_tag, <<"data">>, __TopXMLNS}});
      {value, Data1} -> Data1
    end;
decode_array_els(__TopXMLNS, __Opts,
		 [{xmlel, <<"data">>, _attrs, _} = _el | _els], Data) ->
    case fxmlrpc_codec:get_attr(<<"xmlns">>, _attrs,
				__TopXMLNS)
	of
      <<"xmlrpc">> ->
	  decode_array_els(__TopXMLNS, __Opts, _els,
			   {value, decode_data(<<"xmlrpc">>, __Opts, _el)});
      _ -> decode_array_els(__TopXMLNS, __Opts, _els, Data)
    end;
decode_array_els(__TopXMLNS, __Opts, [_ | _els],
		 Data) ->
    decode_array_els(__TopXMLNS, __Opts, _els, Data).

encode_array({array, Data}, __TopXMLNS) ->
    __NewTopXMLNS =
	fxmlrpc_codec:choose_top_xmlns(<<"xmlrpc">>, [],
				       __TopXMLNS),
    _els = lists:reverse('encode_array_$data'(Data,
					      __NewTopXMLNS, [])),
    _attrs = fxmlrpc_codec:enc_xmlns_attrs(__NewTopXMLNS,
					   __TopXMLNS),
    {xmlel, <<"array">>, _attrs, _els}.

'encode_array_$data'(Data, __TopXMLNS, _acc) ->
    [encode_data(Data, __TopXMLNS) | _acc].

decode_boolean(__TopXMLNS, __Opts,
	       {xmlel, <<"boolean">>, _attrs, _els}) ->
    Cdata = decode_boolean_els(__TopXMLNS, __Opts, _els,
			       <<>>),
    {boolean, Cdata}.

decode_boolean_els(__TopXMLNS, __Opts, [], Cdata) ->
    decode_boolean_cdata(__TopXMLNS, Cdata);
decode_boolean_els(__TopXMLNS, __Opts,
		   [{xmlcdata, _data} | _els], Cdata) ->
    decode_boolean_els(__TopXMLNS, __Opts, _els,
		       <<Cdata/binary, _data/binary>>);
decode_boolean_els(__TopXMLNS, __Opts, [_ | _els],
		   Cdata) ->
    decode_boolean_els(__TopXMLNS, __Opts, _els, Cdata).

encode_boolean({boolean, Cdata}, __TopXMLNS) ->
    __NewTopXMLNS =
	fxmlrpc_codec:choose_top_xmlns(<<"xmlrpc">>, [],
				       __TopXMLNS),
    _els = encode_boolean_cdata(Cdata, []),
    _attrs = fxmlrpc_codec:enc_xmlns_attrs(__NewTopXMLNS,
					   __TopXMLNS),
    {xmlel, <<"boolean">>, _attrs, _els}.

decode_boolean_cdata(__TopXMLNS, <<>>) ->
    erlang:error({fxmlrpc_codec,
		  {missing_cdata, <<>>, <<"boolean">>, __TopXMLNS}});
decode_boolean_cdata(__TopXMLNS, _val) ->
    case catch dec_bool(_val) of
      {'EXIT', _} ->
	  erlang:error({fxmlrpc_codec,
			{bad_cdata_value, <<>>, <<"boolean">>, __TopXMLNS}});
      _res -> _res
    end.

encode_boolean_cdata(_val, _acc) ->
    [{xmlcdata, enc_bool(_val)} | _acc].

decode_dateTime(__TopXMLNS, __Opts,
		{xmlel, <<"dateTime.iso8601">>, _attrs, _els}) ->
    Cdata = decode_dateTime_els(__TopXMLNS, __Opts, _els,
				<<>>),
    {date, Cdata}.

decode_dateTime_els(__TopXMLNS, __Opts, [], Cdata) ->
    decode_dateTime_cdata(__TopXMLNS, Cdata);
decode_dateTime_els(__TopXMLNS, __Opts,
		    [{xmlcdata, _data} | _els], Cdata) ->
    decode_dateTime_els(__TopXMLNS, __Opts, _els,
			<<Cdata/binary, _data/binary>>);
decode_dateTime_els(__TopXMLNS, __Opts, [_ | _els],
		    Cdata) ->
    decode_dateTime_els(__TopXMLNS, __Opts, _els, Cdata).

encode_dateTime({date, Cdata}, __TopXMLNS) ->
    __NewTopXMLNS =
	fxmlrpc_codec:choose_top_xmlns(<<"xmlrpc">>, [],
				       __TopXMLNS),
    _els = encode_dateTime_cdata(Cdata, []),
    _attrs = fxmlrpc_codec:enc_xmlns_attrs(__NewTopXMLNS,
					   __TopXMLNS),
    {xmlel, <<"dateTime.iso8601">>, _attrs, _els}.

decode_dateTime_cdata(__TopXMLNS, <<>>) ->
    erlang:error({fxmlrpc_codec,
		  {missing_cdata, <<>>, <<"dateTime.iso8601">>,
		   __TopXMLNS}});
decode_dateTime_cdata(__TopXMLNS, _val) -> _val.

encode_dateTime_cdata(_val, _acc) ->
    [{xmlcdata, _val} | _acc].

decode_base64(__TopXMLNS, __Opts,
	      {xmlel, <<"base64">>, _attrs, _els}) ->
    Cdata = decode_base64_els(__TopXMLNS, __Opts, _els,
			      <<>>),
    {base64, Cdata}.

decode_base64_els(__TopXMLNS, __Opts, [], Cdata) ->
    decode_base64_cdata(__TopXMLNS, Cdata);
decode_base64_els(__TopXMLNS, __Opts,
		  [{xmlcdata, _data} | _els], Cdata) ->
    decode_base64_els(__TopXMLNS, __Opts, _els,
		      <<Cdata/binary, _data/binary>>);
decode_base64_els(__TopXMLNS, __Opts, [_ | _els],
		  Cdata) ->
    decode_base64_els(__TopXMLNS, __Opts, _els, Cdata).

encode_base64({base64, Cdata}, __TopXMLNS) ->
    __NewTopXMLNS =
	fxmlrpc_codec:choose_top_xmlns(<<"xmlrpc">>, [],
				       __TopXMLNS),
    _els = encode_base64_cdata(Cdata, []),
    _attrs = fxmlrpc_codec:enc_xmlns_attrs(__NewTopXMLNS,
					   __TopXMLNS),
    {xmlel, <<"base64">>, _attrs, _els}.

decode_base64_cdata(__TopXMLNS, <<>>) ->
    erlang:error({fxmlrpc_codec,
		  {missing_cdata, <<>>, <<"base64">>, __TopXMLNS}});
decode_base64_cdata(__TopXMLNS, _val) -> _val.

encode_base64_cdata(_val, _acc) ->
    [{xmlcdata, _val} | _acc].

decode_double(__TopXMLNS, __Opts,
	      {xmlel, <<"double">>, _attrs, _els}) ->
    Cdata = decode_double_els(__TopXMLNS, __Opts, _els,
			      <<>>),
    {double, Cdata}.

decode_double_els(__TopXMLNS, __Opts, [], Cdata) ->
    decode_double_cdata(__TopXMLNS, Cdata);
decode_double_els(__TopXMLNS, __Opts,
		  [{xmlcdata, _data} | _els], Cdata) ->
    decode_double_els(__TopXMLNS, __Opts, _els,
		      <<Cdata/binary, _data/binary>>);
decode_double_els(__TopXMLNS, __Opts, [_ | _els],
		  Cdata) ->
    decode_double_els(__TopXMLNS, __Opts, _els, Cdata).

encode_double({double, Cdata}, __TopXMLNS) ->
    __NewTopXMLNS =
	fxmlrpc_codec:choose_top_xmlns(<<"xmlrpc">>, [],
				       __TopXMLNS),
    _els = encode_double_cdata(Cdata, []),
    _attrs = fxmlrpc_codec:enc_xmlns_attrs(__NewTopXMLNS,
					   __TopXMLNS),
    {xmlel, <<"double">>, _attrs, _els}.

decode_double_cdata(__TopXMLNS, <<>>) ->
    erlang:error({fxmlrpc_codec,
		  {missing_cdata, <<>>, <<"double">>, __TopXMLNS}});
decode_double_cdata(__TopXMLNS, _val) ->
    case catch erlang:binary_to_float(_val) of
      {'EXIT', _} ->
	  erlang:error({fxmlrpc_codec,
			{bad_cdata_value, <<>>, <<"double">>, __TopXMLNS}});
      _res -> _res
    end.

encode_double_cdata(_val, _acc) ->
    [{xmlcdata, erlang:float_to_binary(_val)} | _acc].

decode_string(__TopXMLNS, __Opts,
	      {xmlel, <<"string">>, _attrs, _els}) ->
    Cdata = decode_string_els(__TopXMLNS, __Opts, _els,
			      <<>>),
    {string, Cdata}.

decode_string_els(__TopXMLNS, __Opts, [], Cdata) ->
    decode_string_cdata(__TopXMLNS, Cdata);
decode_string_els(__TopXMLNS, __Opts,
		  [{xmlcdata, _data} | _els], Cdata) ->
    decode_string_els(__TopXMLNS, __Opts, _els,
		      <<Cdata/binary, _data/binary>>);
decode_string_els(__TopXMLNS, __Opts, [_ | _els],
		  Cdata) ->
    decode_string_els(__TopXMLNS, __Opts, _els, Cdata).

encode_string({string, Cdata}, __TopXMLNS) ->
    __NewTopXMLNS =
	fxmlrpc_codec:choose_top_xmlns(<<"xmlrpc">>, [],
				       __TopXMLNS),
    _els = encode_string_cdata(Cdata, []),
    _attrs = fxmlrpc_codec:enc_xmlns_attrs(__NewTopXMLNS,
					   __TopXMLNS),
    {xmlel, <<"string">>, _attrs, _els}.

decode_string_cdata(__TopXMLNS, <<>>) -> <<>>;
decode_string_cdata(__TopXMLNS, _val) -> _val.

encode_string_cdata(<<>>, _acc) -> _acc;
encode_string_cdata(_val, _acc) ->
    [{xmlcdata, _val} | _acc].

decode_int(__TopXMLNS, __Opts,
	   {xmlel, <<"int">>, _attrs, _els}) ->
    Cdata = decode_int_els(__TopXMLNS, __Opts, _els, <<>>),
    {int, Cdata}.

decode_int_els(__TopXMLNS, __Opts, [], Cdata) ->
    decode_int_cdata(__TopXMLNS, Cdata);
decode_int_els(__TopXMLNS, __Opts,
	       [{xmlcdata, _data} | _els], Cdata) ->
    decode_int_els(__TopXMLNS, __Opts, _els,
		   <<Cdata/binary, _data/binary>>);
decode_int_els(__TopXMLNS, __Opts, [_ | _els], Cdata) ->
    decode_int_els(__TopXMLNS, __Opts, _els, Cdata).

encode_int({int, Cdata}, __TopXMLNS) ->
    __NewTopXMLNS =
	fxmlrpc_codec:choose_top_xmlns(<<"xmlrpc">>, [],
				       __TopXMLNS),
    _els = encode_int_cdata(Cdata, []),
    _attrs = fxmlrpc_codec:enc_xmlns_attrs(__NewTopXMLNS,
					   __TopXMLNS),
    {xmlel, <<"int">>, _attrs, _els}.

decode_int_cdata(__TopXMLNS, <<>>) ->
    erlang:error({fxmlrpc_codec,
		  {missing_cdata, <<>>, <<"int">>, __TopXMLNS}});
decode_int_cdata(__TopXMLNS, _val) ->
    case catch erlang:binary_to_integer(_val) of
      {'EXIT', _} ->
	  erlang:error({fxmlrpc_codec,
			{bad_cdata_value, <<>>, <<"int">>, __TopXMLNS}});
      _res -> _res
    end.

encode_int_cdata(_val, _acc) ->
    [{xmlcdata, erlang:integer_to_binary(_val)} | _acc].

decode_i4(__TopXMLNS, __Opts,
	  {xmlel, <<"i4">>, _attrs, _els}) ->
    Cdata = decode_i4_els(__TopXMLNS, __Opts, _els, <<>>),
    {i4, Cdata}.

decode_i4_els(__TopXMLNS, __Opts, [], Cdata) ->
    decode_i4_cdata(__TopXMLNS, Cdata);
decode_i4_els(__TopXMLNS, __Opts,
	      [{xmlcdata, _data} | _els], Cdata) ->
    decode_i4_els(__TopXMLNS, __Opts, _els,
		  <<Cdata/binary, _data/binary>>);
decode_i4_els(__TopXMLNS, __Opts, [_ | _els], Cdata) ->
    decode_i4_els(__TopXMLNS, __Opts, _els, Cdata).

encode_i4({i4, Cdata}, __TopXMLNS) ->
    __NewTopXMLNS =
	fxmlrpc_codec:choose_top_xmlns(<<"xmlrpc">>, [],
				       __TopXMLNS),
    _els = encode_i4_cdata(Cdata, []),
    _attrs = fxmlrpc_codec:enc_xmlns_attrs(__NewTopXMLNS,
					   __TopXMLNS),
    {xmlel, <<"i4">>, _attrs, _els}.

decode_i4_cdata(__TopXMLNS, <<>>) ->
    erlang:error({fxmlrpc_codec,
		  {missing_cdata, <<>>, <<"i4">>, __TopXMLNS}});
decode_i4_cdata(__TopXMLNS, _val) ->
    case catch erlang:binary_to_integer(_val) of
      {'EXIT', _} ->
	  erlang:error({fxmlrpc_codec,
			{bad_cdata_value, <<>>, <<"i4">>, __TopXMLNS}});
      _res -> _res
    end.

encode_i4_cdata(_val, _acc) ->
    [{xmlcdata, erlang:integer_to_binary(_val)} | _acc].

decode_value(__TopXMLNS, __Opts,
	     {xmlel, <<"value">>, _attrs, _els}) ->
    {Cdata, Val} = decode_value_els(__TopXMLNS, __Opts,
				    _els, <<>>, undefined),
    {Val, Cdata}.

decode_value_els(__TopXMLNS, __Opts, [], Cdata, Val) ->
    {decode_value_cdata(__TopXMLNS, Cdata), Val};
decode_value_els(__TopXMLNS, __Opts,
		 [{xmlcdata, _data} | _els], Cdata, Val) ->
    decode_value_els(__TopXMLNS, __Opts, _els,
		     <<Cdata/binary, _data/binary>>, Val);
decode_value_els(__TopXMLNS, __Opts,
		 [{xmlel, <<"i4">>, _attrs, _} = _el | _els], Cdata,
		 Val) ->
    case fxmlrpc_codec:get_attr(<<"xmlns">>, _attrs,
				__TopXMLNS)
	of
      <<"xmlrpc">> ->
	  decode_value_els(__TopXMLNS, __Opts, _els, Cdata,
			   decode_i4(<<"xmlrpc">>, __Opts, _el));
      _ ->
	  decode_value_els(__TopXMLNS, __Opts, _els, Cdata, Val)
    end;
decode_value_els(__TopXMLNS, __Opts,
		 [{xmlel, <<"int">>, _attrs, _} = _el | _els], Cdata,
		 Val) ->
    case fxmlrpc_codec:get_attr(<<"xmlns">>, _attrs,
				__TopXMLNS)
	of
      <<"xmlrpc">> ->
	  decode_value_els(__TopXMLNS, __Opts, _els, Cdata,
			   decode_int(<<"xmlrpc">>, __Opts, _el));
      _ ->
	  decode_value_els(__TopXMLNS, __Opts, _els, Cdata, Val)
    end;
decode_value_els(__TopXMLNS, __Opts,
		 [{xmlel, <<"string">>, _attrs, _} = _el | _els], Cdata,
		 Val) ->
    case fxmlrpc_codec:get_attr(<<"xmlns">>, _attrs,
				__TopXMLNS)
	of
      <<"xmlrpc">> ->
	  decode_value_els(__TopXMLNS, __Opts, _els, Cdata,
			   decode_string(<<"xmlrpc">>, __Opts, _el));
      _ ->
	  decode_value_els(__TopXMLNS, __Opts, _els, Cdata, Val)
    end;
decode_value_els(__TopXMLNS, __Opts,
		 [{xmlel, <<"double">>, _attrs, _} = _el | _els], Cdata,
		 Val) ->
    case fxmlrpc_codec:get_attr(<<"xmlns">>, _attrs,
				__TopXMLNS)
	of
      <<"xmlrpc">> ->
	  decode_value_els(__TopXMLNS, __Opts, _els, Cdata,
			   decode_double(<<"xmlrpc">>, __Opts, _el));
      _ ->
	  decode_value_els(__TopXMLNS, __Opts, _els, Cdata, Val)
    end;
decode_value_els(__TopXMLNS, __Opts,
		 [{xmlel, <<"base64">>, _attrs, _} = _el | _els], Cdata,
		 Val) ->
    case fxmlrpc_codec:get_attr(<<"xmlns">>, _attrs,
				__TopXMLNS)
	of
      <<"xmlrpc">> ->
	  decode_value_els(__TopXMLNS, __Opts, _els, Cdata,
			   decode_base64(<<"xmlrpc">>, __Opts, _el));
      _ ->
	  decode_value_els(__TopXMLNS, __Opts, _els, Cdata, Val)
    end;
decode_value_els(__TopXMLNS, __Opts,
		 [{xmlel, <<"boolean">>, _attrs, _} = _el | _els], Cdata,
		 Val) ->
    case fxmlrpc_codec:get_attr(<<"xmlns">>, _attrs,
				__TopXMLNS)
	of
      <<"xmlrpc">> ->
	  decode_value_els(__TopXMLNS, __Opts, _els, Cdata,
			   decode_boolean(<<"xmlrpc">>, __Opts, _el));
      _ ->
	  decode_value_els(__TopXMLNS, __Opts, _els, Cdata, Val)
    end;
decode_value_els(__TopXMLNS, __Opts,
		 [{xmlel, <<"array">>, _attrs, _} = _el | _els], Cdata,
		 Val) ->
    case fxmlrpc_codec:get_attr(<<"xmlns">>, _attrs,
				__TopXMLNS)
	of
      <<"xmlrpc">> ->
	  decode_value_els(__TopXMLNS, __Opts, _els, Cdata,
			   decode_array(<<"xmlrpc">>, __Opts, _el));
      _ ->
	  decode_value_els(__TopXMLNS, __Opts, _els, Cdata, Val)
    end;
decode_value_els(__TopXMLNS, __Opts,
		 [{xmlel, <<"nil">>, _attrs, _} = _el | _els], Cdata,
		 Val) ->
    case fxmlrpc_codec:get_attr(<<"xmlns">>, _attrs,
				__TopXMLNS)
	of
      <<"xmlrpc">> ->
	  decode_value_els(__TopXMLNS, __Opts, _els, Cdata,
			   decode_nil(<<"xmlrpc">>, __Opts, _el));
      _ ->
	  decode_value_els(__TopXMLNS, __Opts, _els, Cdata, Val)
    end;
decode_value_els(__TopXMLNS, __Opts,
		 [{xmlel, <<"struct">>, _attrs, _} = _el | _els], Cdata,
		 Val) ->
    case fxmlrpc_codec:get_attr(<<"xmlns">>, _attrs,
				__TopXMLNS)
	of
      <<"xmlrpc">> ->
	  decode_value_els(__TopXMLNS, __Opts, _els, Cdata,
			   decode_struct(<<"xmlrpc">>, __Opts, _el));
      _ ->
	  decode_value_els(__TopXMLNS, __Opts, _els, Cdata, Val)
    end;
decode_value_els(__TopXMLNS, __Opts,
		 [{xmlel, <<"dateTime.iso8601">>, _attrs, _} = _el
		  | _els],
		 Cdata, Val) ->
    case fxmlrpc_codec:get_attr(<<"xmlns">>, _attrs,
				__TopXMLNS)
	of
      <<"xmlrpc">> ->
	  decode_value_els(__TopXMLNS, __Opts, _els, Cdata,
			   decode_dateTime(<<"xmlrpc">>, __Opts, _el));
      _ ->
	  decode_value_els(__TopXMLNS, __Opts, _els, Cdata, Val)
    end;
decode_value_els(__TopXMLNS, __Opts, [_ | _els], Cdata,
		 Val) ->
    decode_value_els(__TopXMLNS, __Opts, _els, Cdata, Val).

encode_value({Val, Cdata}, __TopXMLNS) ->
    __NewTopXMLNS =
	fxmlrpc_codec:choose_top_xmlns(<<"xmlrpc">>, [],
				       __TopXMLNS),
    _els = lists:reverse(encode_value_cdata(Cdata,
					    'encode_value_$val'(Val,
								__NewTopXMLNS,
								[]))),
    _attrs = fxmlrpc_codec:enc_xmlns_attrs(__NewTopXMLNS,
					   __TopXMLNS),
    {xmlel, <<"value">>, _attrs, _els}.

'encode_value_$val'(undefined, __TopXMLNS, _acc) ->
    _acc;
'encode_value_$val'({i4, _} = Val, __TopXMLNS, _acc) ->
    [encode_i4(Val, __TopXMLNS) | _acc];
'encode_value_$val'({int, _} = Val, __TopXMLNS, _acc) ->
    [encode_int(Val, __TopXMLNS) | _acc];
'encode_value_$val'({string, _} = Val, __TopXMLNS,
		    _acc) ->
    [encode_string(Val, __TopXMLNS) | _acc];
'encode_value_$val'({double, _} = Val, __TopXMLNS,
		    _acc) ->
    [encode_double(Val, __TopXMLNS) | _acc];
'encode_value_$val'({base64, _} = Val, __TopXMLNS,
		    _acc) ->
    [encode_base64(Val, __TopXMLNS) | _acc];
'encode_value_$val'({boolean, _} = Val, __TopXMLNS,
		    _acc) ->
    [encode_boolean(Val, __TopXMLNS) | _acc];
'encode_value_$val'({array, _} = Val, __TopXMLNS,
		    _acc) ->
    [encode_array(Val, __TopXMLNS) | _acc];
'encode_value_$val'(nil = Val, __TopXMLNS, _acc) ->
    [encode_nil(Val, __TopXMLNS) | _acc];
'encode_value_$val'({struct, _} = Val, __TopXMLNS,
		    _acc) ->
    [encode_struct(Val, __TopXMLNS) | _acc];
'encode_value_$val'({date, _} = Val, __TopXMLNS,
		    _acc) ->
    [encode_dateTime(Val, __TopXMLNS) | _acc].

decode_value_cdata(__TopXMLNS, <<>>) -> <<>>;
decode_value_cdata(__TopXMLNS, _val) -> _val.

encode_value_cdata(<<>>, _acc) -> _acc;
encode_value_cdata(_val, _acc) ->
    [{xmlcdata, _val} | _acc].

decode_param(__TopXMLNS, __Opts,
	     {xmlel, <<"param">>, _attrs, _els}) ->
    Value = decode_param_els(__TopXMLNS, __Opts, _els,
			     error),
    Value.

decode_param_els(__TopXMLNS, __Opts, [], Value) ->
    case Value of
      error ->
	  erlang:error({fxmlrpc_codec,
			{missing_tag, <<"value">>, __TopXMLNS}});
      {value, Value1} -> Value1
    end;
decode_param_els(__TopXMLNS, __Opts,
		 [{xmlel, <<"value">>, _attrs, _} = _el | _els],
		 Value) ->
    case fxmlrpc_codec:get_attr(<<"xmlns">>, _attrs,
				__TopXMLNS)
	of
      <<"xmlrpc">> ->
	  decode_param_els(__TopXMLNS, __Opts, _els,
			   {value, decode_value(<<"xmlrpc">>, __Opts, _el)});
      _ -> decode_param_els(__TopXMLNS, __Opts, _els, Value)
    end;
decode_param_els(__TopXMLNS, __Opts, [_ | _els],
		 Value) ->
    decode_param_els(__TopXMLNS, __Opts, _els, Value).

encode_param(Value, __TopXMLNS) ->
    __NewTopXMLNS =
	fxmlrpc_codec:choose_top_xmlns(<<"xmlrpc">>, [],
				       __TopXMLNS),
    _els = lists:reverse('encode_param_$value'(Value,
					       __NewTopXMLNS, [])),
    _attrs = fxmlrpc_codec:enc_xmlns_attrs(__NewTopXMLNS,
					   __TopXMLNS),
    {xmlel, <<"param">>, _attrs, _els}.

'encode_param_$value'(Value, __TopXMLNS, _acc) ->
    [encode_value(Value, __TopXMLNS) | _acc].

decode_params(__TopXMLNS, __Opts,
	      {xmlel, <<"params">>, _attrs, _els}) ->
    Params = decode_params_els(__TopXMLNS, __Opts, _els,
			       []),
    Params.

decode_params_els(__TopXMLNS, __Opts, [], Params) ->
    lists:reverse(Params);
decode_params_els(__TopXMLNS, __Opts,
		  [{xmlel, <<"param">>, _attrs, _} = _el | _els],
		  Params) ->
    case fxmlrpc_codec:get_attr(<<"xmlns">>, _attrs,
				__TopXMLNS)
	of
      <<"xmlrpc">> ->
	  decode_params_els(__TopXMLNS, __Opts, _els,
			    [decode_param(<<"xmlrpc">>, __Opts, _el) | Params]);
      _ -> decode_params_els(__TopXMLNS, __Opts, _els, Params)
    end;
decode_params_els(__TopXMLNS, __Opts, [_ | _els],
		  Params) ->
    decode_params_els(__TopXMLNS, __Opts, _els, Params).

encode_params(Params, __TopXMLNS) ->
    __NewTopXMLNS =
	fxmlrpc_codec:choose_top_xmlns(<<"xmlrpc">>, [],
				       __TopXMLNS),
    _els = lists:reverse('encode_params_$params'(Params,
						 __NewTopXMLNS, [])),
    _attrs = fxmlrpc_codec:enc_xmlns_attrs(__NewTopXMLNS,
					   __TopXMLNS),
    {xmlel, <<"params">>, _attrs, _els}.

'encode_params_$params'([], __TopXMLNS, _acc) -> _acc;
'encode_params_$params'([Params | _els], __TopXMLNS,
			_acc) ->
    'encode_params_$params'(_els, __TopXMLNS,
			    [encode_param(Params, __TopXMLNS) | _acc]).

decode_methodName(__TopXMLNS, __Opts,
		  {xmlel, <<"methodName">>, _attrs, _els}) ->
    Cdata = decode_methodName_els(__TopXMLNS, __Opts, _els,
				  <<>>),
    Cdata.

decode_methodName_els(__TopXMLNS, __Opts, [], Cdata) ->
    decode_methodName_cdata(__TopXMLNS, Cdata);
decode_methodName_els(__TopXMLNS, __Opts,
		      [{xmlcdata, _data} | _els], Cdata) ->
    decode_methodName_els(__TopXMLNS, __Opts, _els,
			  <<Cdata/binary, _data/binary>>);
decode_methodName_els(__TopXMLNS, __Opts, [_ | _els],
		      Cdata) ->
    decode_methodName_els(__TopXMLNS, __Opts, _els, Cdata).

encode_methodName(Cdata, __TopXMLNS) ->
    __NewTopXMLNS =
	fxmlrpc_codec:choose_top_xmlns(<<"xmlrpc">>, [],
				       __TopXMLNS),
    _els = encode_methodName_cdata(Cdata, []),
    _attrs = fxmlrpc_codec:enc_xmlns_attrs(__NewTopXMLNS,
					   __TopXMLNS),
    {xmlel, <<"methodName">>, _attrs, _els}.

decode_methodName_cdata(__TopXMLNS, <<>>) ->
    erlang:error({fxmlrpc_codec,
		  {missing_cdata, <<>>, <<"methodName">>, __TopXMLNS}});
decode_methodName_cdata(__TopXMLNS, _val) ->
    case catch erlang:binary_to_atom(_val, utf8) of
      {'EXIT', _} ->
	  erlang:error({fxmlrpc_codec,
			{bad_cdata_value, <<>>, <<"methodName">>, __TopXMLNS}});
      _res -> _res
    end.

encode_methodName_cdata(_val, _acc) ->
    [{xmlcdata, erlang:atom_to_binary(_val, utf8)} | _acc].

decode_fault(__TopXMLNS, __Opts,
	     {xmlel, <<"fault">>, _attrs, _els}) ->
    Value = decode_fault_els(__TopXMLNS, __Opts, _els,
			     error),
    {fault, Value}.

decode_fault_els(__TopXMLNS, __Opts, [], Value) ->
    case Value of
      error ->
	  erlang:error({fxmlrpc_codec,
			{missing_tag, <<"value">>, __TopXMLNS}});
      {value, Value1} -> Value1
    end;
decode_fault_els(__TopXMLNS, __Opts,
		 [{xmlel, <<"value">>, _attrs, _} = _el | _els],
		 Value) ->
    case fxmlrpc_codec:get_attr(<<"xmlns">>, _attrs,
				__TopXMLNS)
	of
      <<"xmlrpc">> ->
	  decode_fault_els(__TopXMLNS, __Opts, _els,
			   {value, decode_value(<<"xmlrpc">>, __Opts, _el)});
      _ -> decode_fault_els(__TopXMLNS, __Opts, _els, Value)
    end;
decode_fault_els(__TopXMLNS, __Opts, [_ | _els],
		 Value) ->
    decode_fault_els(__TopXMLNS, __Opts, _els, Value).

encode_fault({fault, Value}, __TopXMLNS) ->
    __NewTopXMLNS =
	fxmlrpc_codec:choose_top_xmlns(<<"xmlrpc">>, [],
				       __TopXMLNS),
    _els = lists:reverse('encode_fault_$value'(Value,
					       __NewTopXMLNS, [])),
    _attrs = fxmlrpc_codec:enc_xmlns_attrs(__NewTopXMLNS,
					   __TopXMLNS),
    {xmlel, <<"fault">>, _attrs, _els}.

'encode_fault_$value'(Value, __TopXMLNS, _acc) ->
    [encode_value(Value, __TopXMLNS) | _acc].

decode_methodResponse(__TopXMLNS, __Opts,
		      {xmlel, <<"methodResponse">>, _attrs, _els}) ->
    Payload = decode_methodResponse_els(__TopXMLNS, __Opts,
					_els, []),
    {response, Payload}.

decode_methodResponse_els(__TopXMLNS, __Opts, [],
			  Payload) ->
    Payload;
decode_methodResponse_els(__TopXMLNS, __Opts,
			  [{xmlel, <<"fault">>, _attrs, _} = _el | _els],
			  Payload) ->
    case fxmlrpc_codec:get_attr(<<"xmlns">>, _attrs,
				__TopXMLNS)
	of
      <<"xmlrpc">> ->
	  decode_methodResponse_els(__TopXMLNS, __Opts, _els,
				    decode_fault(<<"xmlrpc">>, __Opts, _el));
      _ ->
	  decode_methodResponse_els(__TopXMLNS, __Opts, _els,
				    Payload)
    end;
decode_methodResponse_els(__TopXMLNS, __Opts,
			  [{xmlel, <<"params">>, _attrs, _} = _el | _els],
			  Payload) ->
    case fxmlrpc_codec:get_attr(<<"xmlns">>, _attrs,
				__TopXMLNS)
	of
      <<"xmlrpc">> ->
	  decode_methodResponse_els(__TopXMLNS, __Opts, _els,
				    decode_params(<<"xmlrpc">>, __Opts, _el));
      _ ->
	  decode_methodResponse_els(__TopXMLNS, __Opts, _els,
				    Payload)
    end;
decode_methodResponse_els(__TopXMLNS, __Opts,
			  [_ | _els], Payload) ->
    decode_methodResponse_els(__TopXMLNS, __Opts, _els,
			      Payload).

encode_methodResponse({response, Payload},
		      __TopXMLNS) ->
    __NewTopXMLNS =
	fxmlrpc_codec:choose_top_xmlns(<<"xmlrpc">>, [],
				       __TopXMLNS),
    _els =
	lists:reverse('encode_methodResponse_$payload'(Payload,
						       __NewTopXMLNS, [])),
    _attrs = fxmlrpc_codec:enc_xmlns_attrs(__NewTopXMLNS,
					   __TopXMLNS),
    {xmlel, <<"methodResponse">>, _attrs, _els}.

'encode_methodResponse_$payload'([], __TopXMLNS,
				 _acc) ->
    _acc;
'encode_methodResponse_$payload'({fault, _} = Payload,
				 __TopXMLNS, _acc) ->
    [encode_fault(Payload, __TopXMLNS) | _acc];
'encode_methodResponse_$payload'(_ = Payload,
				 __TopXMLNS, _acc) ->
    [encode_params(Payload, __TopXMLNS) | _acc].

decode_methodCall(__TopXMLNS, __Opts,
		  {xmlel, <<"methodCall">>, _attrs, _els}) ->
    {Params, Name} = decode_methodCall_els(__TopXMLNS,
					   __Opts, _els, [], error),
    {call, Name, Params}.

decode_methodCall_els(__TopXMLNS, __Opts, [], Params,
		      Name) ->
    {Params,
     case Name of
       error ->
	   erlang:error({fxmlrpc_codec,
			 {missing_tag, <<"methodName">>, __TopXMLNS}});
       {value, Name1} -> Name1
     end};
decode_methodCall_els(__TopXMLNS, __Opts,
		      [{xmlel, <<"methodName">>, _attrs, _} = _el | _els],
		      Params, Name) ->
    case fxmlrpc_codec:get_attr(<<"xmlns">>, _attrs,
				__TopXMLNS)
	of
      <<"xmlrpc">> ->
	  decode_methodCall_els(__TopXMLNS, __Opts, _els, Params,
				{value,
				 decode_methodName(<<"xmlrpc">>, __Opts, _el)});
      _ ->
	  decode_methodCall_els(__TopXMLNS, __Opts, _els, Params,
				Name)
    end;
decode_methodCall_els(__TopXMLNS, __Opts,
		      [{xmlel, <<"params">>, _attrs, _} = _el | _els], Params,
		      Name) ->
    case fxmlrpc_codec:get_attr(<<"xmlns">>, _attrs,
				__TopXMLNS)
	of
      <<"xmlrpc">> ->
	  decode_methodCall_els(__TopXMLNS, __Opts, _els,
				decode_params(<<"xmlrpc">>, __Opts, _el), Name);
      _ ->
	  decode_methodCall_els(__TopXMLNS, __Opts, _els, Params,
				Name)
    end;
decode_methodCall_els(__TopXMLNS, __Opts, [_ | _els],
		      Params, Name) ->
    decode_methodCall_els(__TopXMLNS, __Opts, _els, Params,
			  Name).

encode_methodCall({call, Name, Params}, __TopXMLNS) ->
    __NewTopXMLNS =
	fxmlrpc_codec:choose_top_xmlns(<<"xmlrpc">>, [],
				       __TopXMLNS),
    _els = lists:reverse('encode_methodCall_$params'(Params,
						     __NewTopXMLNS,
						     'encode_methodCall_$name'(Name,
									       __NewTopXMLNS,
									       []))),
    _attrs = fxmlrpc_codec:enc_xmlns_attrs(__NewTopXMLNS,
					   __TopXMLNS),
    {xmlel, <<"methodCall">>, _attrs, _els}.

'encode_methodCall_$params'([], __TopXMLNS, _acc) ->
    _acc;
'encode_methodCall_$params'(Params, __TopXMLNS, _acc) ->
    [encode_params(Params, __TopXMLNS) | _acc].

'encode_methodCall_$name'(Name, __TopXMLNS, _acc) ->
    [encode_methodName(Name, __TopXMLNS) | _acc].
