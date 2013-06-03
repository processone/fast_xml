%%%-------------------------------------------------------------------
%%% File    : xml_gen.erl
%%% Author  : Evgeniy Khramtsov <ekhramtsov@process-one.net>
%%% Description : XML code generator.
%%%
%%% Created : 22 Jun 2009 by Evgeniy Khramtsov <ekhramtsov@process-one.net>
%%%-------------------------------------------------------------------
-module(xml_gen).

%% API
-export([compile/1, compile/2, compile/3]).

-include("xml_gen.hrl").

-define(err(F, Args),
	begin
	    io:format("** xml_gen error: " ++ F ++ "~n", Args),
	    erlang:error(badarg)
	end).

-define(info(F, Args), io:format("xml_gen: " ++ F ++ "~n", Args)).
-define(warn(F, Args), io:format("* xml_gen warning: " ++ F ++ "~n", Args)).

%%====================================================================
%% API
%%====================================================================
compile(Path) ->
    case consult(Path) of
        {ok, Terms} ->
            Specs = lists:filter(
                      fun({spec, _, #spec{}}) -> true;
                         (_) -> false
                      end, Terms),
            lists:foldl(
              fun(_, error) ->
                      error;
                 ({spec, ModuleName, Spec}, ok) ->
                      compile(Spec,
                              atom_to_list(ModuleName))
              end, ok, Specs);
        Err ->
            ?err("failed to parse spec file '~s': ~p", [Path, Err]),
            Err
    end.

compile(Spec, ModuleName) ->
    compile(Spec, ModuleName, ".").

compile(Spec, ModuleName, Dir)
  when is_record(Spec, spec), is_list(ModuleName), is_list(Dir) ->
    Path = filename:absname(ModuleName ++ ".erl", Dir),
    case catch compile1(Spec, ModuleName, Dir) of
	{'EXIT', bad_spec} ->
	    file:delete(Path),
	    error;
	{'EXIT', Reason} ->
	    io:format("** xml_gen unexpected error: ~p~n", [Reason]),
	    file:delete(Path),
	    error;
	_ ->
	    ok
    end.

%%====================================================================
%% Internal functions
%%====================================================================
compile1(Spec, ModuleName, Dir) ->
    Path = filename:absname(ModuleName ++ ".erl", Dir),
    ?info("generating ~p", [Path]),
    {ok, Fd} = file:open(Path, [write]),
    put(var_inc, 1),
    put(gen_file_out, Fd),
    gen_header(ModuleName),
    emit("-export(["),
    NewSpec = prepare_spec(Spec),
    emit(["decode/1, encode/1]).",nl]),
    EncFun = enc_fun(NewSpec#spec.label),
    DecFun = dec_fun(NewSpec#spec.label),
    emit(["%%====================================================================",
	  nl,"%% API",nl,
	  "%%====================================================================",
	  nl]),
    emit(["decode(El) ->",nl,
	  "case catch ",DecFun, "(El) of",nl,
	  "{'EXIT', Err} -> {error, Err};",nl,
	  "Res -> {ok, Res}",nl,"end.",nl]),
    emit(["encode(Term) ->",nl,
	  EncFun, "(Term).",nl]),
    emit(["%%====================================================================",
	  nl,"%% Internal API",nl,
	  "%%====================================================================",
	  nl]),
    process_spec([NewSpec], []),
    gen_footer(),
    file:close(Fd),
    erl_tidy:file(Path, [{quiet, true},
     			 {verbose, false},
     			 {keep_unused, false},
     			 {backups, false}]).

process_spec([#spec{name = Name,
		    label = LabelName,
		    attrs = AttrSpecs,
		    result = Result,
		    default = Default,
		    min = Min,
		    max = Max,
		    cdata = CDataSpec,
		    els = SubSpecs} = Spec | Tail], Acc) ->
    IsRepeatedSpec = if (Tail /= []) or (Acc /= []) ->
			     true;
			true ->
			     false
		     end,
    if IsRepeatedSpec ->
	    emit([dec_fun(LabelName),
                  "([{xmlel, <<\"",
		  binary_to_list(Name),
                  "\">>, _Attrs, _Els}|_]) ->", nl]);
       true ->
	    emit([dec_fun(LabelName),
                  "({xmlel, <<\"",
		  binary_to_list(Name),
                  "\">>, _Attrs, _Els}) ->", nl])
    end,
    {Type, Labels} =
	if is_tuple(Result) ->
		{tuple, tuple_to_list(Result)};
	   is_list(Result) ->
		{list, Result};
	   true ->
		case is_label(Result) of
		    false when not ((Min == 0) and (Max == 1)) ->
			?warn("spec '~s' doesn't perform "
			      "any actions", [Name]);
		    _ ->
			ok
		end,
		{other, [Result]}
	end,
    Res = [process_label(L, AttrSpecs, CDataSpec, SubSpecs) || L <- Labels],
    gen_result(Res, Type, out),
    NewAcc = [{Res, Type, Spec} | Acc],
    if Tail /= [] ->
	    emit([";",nl]),
	    process_spec(Tail, NewAcc);
       true ->
	    if IsRepeatedSpec ->
		    emit([";",nl]),
		    emit([dec_fun(LabelName), "([_|Tail]) ->",
			  nl, dec_fun(LabelName), "(Tail)"]),
		    emit([";",nl]),
		    emit([dec_fun(LabelName), "([]) ->",
			  nl, {asis, Default}]);
	       true ->
		    ok
	    end,
	    emit([".",nl]),
	    RevAcc = lists:reverse(NewAcc),
	    gen_encoders(RevAcc),
	    lists:foreach(
	      fun({_Res, _Type, #spec{els = SubSpecs1}}) ->
		      SubSpecGroups = group_subspecs(SubSpecs1),
		      lists:foreach(
			fun(S) ->
				process_spec(S, [])
			end, SubSpecGroups)
	      end, RevAcc)
    end.

gen_enc_aux(_EncFun, _Min, Max) when Max =< 1 ->
    ok;
gen_enc_aux(EncFun, 0, unlimited) ->
    emit([EncFun,"([H|T], Acc) ->",nl,
	  EncFun,"(T, [",EncFun,"(H)|Acc]);",nl,
	  EncFun,"([], Acc) -> Acc.",nl]);
gen_enc_aux(EncFun, Min, Max) ->
    emit([EncFun,"(V, Acc) ->",
	  EncFun,"(V, Acc,",Min,com,Max,com,"0).",nl,
	  EncFun,"([H|T], Acc, Min, Max, Cur) ->",nl,
	  EncFun,"(T, [",EncFun,"(H)|Acc], Min, Max, Cur+1);",nl,
	  EncFun,"([], Acc, Min, Max, Cur)",
	  "when Min =< Cur, Cur =< Max -> Acc.",nl]).

gen_encoders([{Res, Type, #spec{name = Name,
				label = LabelName,
				min = Min,
				max = Max,
				xmlns = XMLNS}} | Tail]) ->
    gen_enc_aux(enc_fun(LabelName), Min, Max),
    emit([enc_fun(LabelName), "("]),
    gen_result(Res, Type, in),
    emit([") ->",nl]),
    gen_encoder(Res, Name, XMLNS),
    if Tail == [] ->
	    emit([".", nl]);
       true ->
	    emit([";", nl]),
	    gen_encoders(Tail)
    end.

gen_encoder(Res, Name, XMLNS) ->
    {RAs, As, Cs, REs, Es} =
	lists:foldl(
	  fun({_, #attr{required = true}} = R, {RA, A, C, RE, E}) ->
		  {[R|RA], A, C, RE, E};
	     ({_, #attr{}} = R, {RA, A, C, RE, E}) ->
		  {RA, [R|A], C, RE, E};
	     ({_, #cdata{}} = R, {RA, A, C, RE, E}) ->
		  {RA, A, [R|C], RE, E};
	     ({_, #spec{min = 1, max = 1}} = R, {RA, A, C, RE, E}) ->
		  {RA, A, C, [R|RE], E};
	     ({_, #spec{}} = R, {RA, A, C, RE, E}) ->
		  {RA, A, C, RE, [R|E]};
	     (_, Acc) ->
		  Acc
	  end, {[], [], [], [], []}, Res),
    gen_attrs(RAs, As, XMLNS),
    gen_els(Cs, REs, Es),
    emit(["{xmlel, <<\"",binary_to_list(Name),"\">>",com]),
    if RAs == [], As == [], XMLNS == <<"">> ->
            emit("[]");
       true ->
	    emit("Attrs")
    end,
    emit(com),
    if Cs == [], REs == [], Es == [] ->
            emit("[]");
       true ->
	    emit("Els")
    end,
    emit(["}"]).

gen_els([], [], []) ->
    ok;
gen_els([], [], Es) ->
    gen_optional_els(Es, "[]");
gen_els(Cs, REs, Es) ->
    Els1 = if Es /= [] ->
		   "ElsA";
	      true ->
		   "Els"
	   end,
    case Cs of
	[] ->
	    emit([Els1," = ["]),
	    gen_required_els(REs),
	    emit(["]",com]);
	[{Var, #cdata{required = true, enc = MFA}}] ->
	    emit([Els1," = ["]),
	    emit(["{xmlcdata",com]),
	    emit_mfa(Var, MFA),
	    emit("}"),
	    if REs /= [] ->
		    emit(com),
		    gen_required_els(REs);
	       true ->
		    ok
	    end,
	    emit(["]",com]);
	[{Var, #cdata{enc = MFA, default = Default}}] ->
	    if REs /= [] ->
		    emit("ElsB = ["),
		    gen_required_els(REs),
		    emit(["]",com]);
	       true ->
		    ok
	    end,
	    emit([Els1," = if ",Var," /= ",{asis,Default}," ->",nl]),
	    emit(["[{xmlcdata",com]),
	    emit_mfa(Var, MFA),
	    emit("}"),
	    if REs /= [] ->
		    emit(["|ElsB];",nl]);
	       true ->
		    emit(["];",nl])
	    end,
	    emit(["true ->",nl]),
	    if REs /= [] ->
		    emit("ElsB");
	       true ->
		    emit("[]")
	    end,
	    emit([nl,"end",com])
    end,
    gen_optional_els(Es, "ElsA").

gen_optional_els([], _) ->
    ok;
gen_optional_els([{Var, #spec{label = Label,
			      default = Default,
			      min = 0, max = 1}}], AccEls) ->
    emit(["Els = if ",Var," /= ",{asis, Default}," ->",nl,
	  "[",enc_fun(Label),"(",Var,")"]),
    if AccEls == "[]" ->
	    emit(["];",nl]);
       true ->
	    emit(["|",AccEls,"];",nl])
    end,
    emit(["true ->",AccEls,nl,"end",com,nl]);
gen_optional_els([{Var, #spec{label = Label}}], AccEls) ->
    emit(["Els = ",enc_fun(Label),"(",Var,com,AccEls,")",com,nl]);
gen_optional_els(Es, AccEls) ->
    emit(["Els = lists:foldl(",nl,"fun"]),
    lists:foreach(
      fun({_, #spec{min = 0, max = 1,
		    label = Label, default = Default}}) ->
	      emit(["({",{asis, Label}, com,{asis, Default},
		    "},Acc) -> Acc;",nl,
		    "({",{asis, Label},
		    com,"V}, Acc) -> [",
		    enc_fun(Label),"(V)|Acc];",nl]);
	 ({_, #spec{label = Label}}) ->
	      emit(["({",{asis, Label},
		    com," []}, Acc) -> Acc;",nl,
		    "({",{asis, Label},
		    com," V}, Acc) ->",nl,
		    enc_fun(Label),"(V, Acc);",nl])
      end, Es),
    emit(["(_, Acc) -> Acc",nl,"end",com,AccEls,com,"["]),
    gen_pairs([{L, {asis, E#spec.label}} || {L, E} <- Es]),
    emit(["])",com]).

gen_required_els([{Var, #spec{label = Label}}|T]) ->
    emit([enc_fun(Label),"(",Var,")"]),
    if T /= [] ->
	    emit(com);
       true ->
	    ok
    end,
    gen_required_els(T);
gen_required_els([]) ->
    ok.

gen_attrs([], [], "") ->
    ok;
gen_attrs([], As, "") ->
    gen_optional_attrs(As, "[]");
gen_attrs(RAs, As, XMLNS) ->
    Attrs1 = if As == [] ->
		     "Attrs";
		true ->
		     "AttrsA"
	     end,
    gen_xmlns(XMLNS, RAs, Attrs1),
    if XMLNS == "", RAs /= [] ->
	    emit([Attrs1," = ["]),
	    gen_required_attrs(RAs),
	    emit(["]",com,nl]);
       RAs /= [] ->
	    gen_required_attrs(RAs),
	    emit(["]",com,nl]);
       true ->
	    ok
    end,
    gen_optional_attrs(As, "AttrsA").

gen_optional_attrs([], _) ->
    ok;
gen_optional_attrs([{Var, #attr{name = Name,
				default = Default,
				enc = MFA}}], AccAttrs) ->
    emit(["Attrs = if ",Var," /= ",{asis, Default}," ->",
	  "[{<<\"",binary_to_list(Name),"\">>,"]),
    emit_mfa(Var, MFA),
    emit("}"),
    if AccAttrs == "[]" ->
	    emit("];");
       true ->
	    emit(["|",AccAttrs,"];"])
    end,
    emit(["true ->",AccAttrs,nl,"end",com,nl]);
gen_optional_attrs(Attrs, AccAttrs) ->
    emit(["Attrs = lists:foldl(","fun"]),
    lists:foreach(
      fun({_Var, #attr{name = Name, default = Default, enc = MFA}}) ->
	      emit(["({<<\"",binary_to_list(Name),"\">>,",{asis,Default},"},Acc) -> Acc;",nl,
		    "({<<\"",binary_to_list(Name),"\">>, V}, Acc) ->",nl,
		    "[{<<\"",binary_to_list(Name),"\">>,"]),
	      emit_mfa("V", MFA),
	      emit("}|Acc];")
      end, Attrs),
    emit(["(_, Acc) -> Acc",nl,"end",com,AccAttrs,com,"["]),
    gen_pairs([{L, "<<\"" ++ binary_to_list(A#attr.name) ++ "\">>"} || {L, A} <- Attrs]),
    emit(["])",com]).

gen_pairs(Pairs) ->
    gen_pairs1(lists:reverse(Pairs)).

gen_pairs1([{Var, Name}|T]) ->
    emit(["{",Name,com,Var,"}"]),
    if T /= [] ->
	    emit(com);
       true ->
	    ok
    end,
    gen_pairs1(T);
gen_pairs1([]) ->
    ok.

gen_xmlns(XMLNS, RAs, AttrsVar) ->
    if XMLNS /= "" ->
	    emit([AttrsVar," = ["]),
	    emit(["{<<\"xmlns\">>, <<\"",binary_to_list(XMLNS),"\">>}"]),
	    if RAs == [] ->
		    emit(["]",com,nl]);
	       true ->
		    emit(com)
	    end;
       true ->
	    ok
    end.

gen_required_attrs([{Var, #attr{name = Name,
				enc = MFA}}|T]) ->
    emit(["{<<\"",binary_to_list(Name),"\">>,"]),
    emit_mfa(Var, MFA),
    emit(["}",nl]),
    if T /= [] ->
	    emit(com);
       true ->
	    ok
    end,
    gen_required_attrs(T);
gen_required_attrs([]) ->
    ok.

prepare_spec(#spec{name = Name,
		   label = Label,
		   xmlns = XMLNS,
		   min = Min,
		   max = Max,
		   cdata = CData,
		   attrs = AttrSpecs,
		   els = SubSpecs} = Spec) ->
    if is_binary(Name), Name /= <<"">> ->
	    ok;
       true ->
	    ?err("bad spec 'name': ~p", [Name])
    end,
    NewLabel = prepare_label(Label, Name),
    emit([dec_fun(NewLabel),"/1",com,enc_fun(NewLabel),"/1",com]),
    if is_binary(XMLNS) ->
	    ok;
       true ->
	    ?err("bad 'xmlns': ~p", [XMLNS])
    end,
    if is_integer(Min), Min >= 0 ->
	    if (is_integer(Max) and (Max >= 1)) or (Max == unlimited) ->
		    if (Min =< Max) ->
			    ok;
		       true ->
			    ?err("'min' (~p) must be less or "
				 "equal than 'max' (~p)",
				 [Min, Max])
		    end;
	       true ->
		    ?err("invalid value for 'max': ~p", [Max])
	    end;
       true ->
	    ?err("invalid value for 'min': ~p", [Min])
    end,
    NewCData = prepare_cdata_spec(CData),
    NewAttrs = if is_list(AttrSpecs) ->
		       [prepare_attr_spec(AttrSpec)
			|| AttrSpec <- AttrSpecs];
		  true ->
		       ?err("bad 'attrs': ~p", [AttrSpecs])
	       end,
    NewSubSpecs = if is_list(SubSpecs) ->
			  [prepare_spec(SubSpec)
			   || SubSpec <- SubSpecs];
		     true ->
			  ?err("bad 'els': ~p", [SubSpecs])
		  end,
    Spec#spec{label = NewLabel,
	      cdata = NewCData,
	      attrs = NewAttrs,
	      els = NewSubSpecs};
prepare_spec(Junk) ->
    ?err("bad spec: ~p", [Junk]).

prepare_cdata_spec(#cdata{required = Required,
			  label = Label,
			  dec = Dec,
			  enc = Enc} = CDataSpec) ->
    if (Required == false) or (Required == true) ->
	    ok;
       true ->
	    ?err("bad 'required' in #cdata: ~p", [Required])
    end,
    if Label == undefined ->
	    ok;
       is_atom(Label) ->
	    case atom_to_list(Label) of
		[$$|_] ->
		    ok;
		_ ->
		    ?err("bad 'label' in #cdata: ~p", [Label])
	    end;
       true ->
	    ?err("bad 'label' in #cdata: ~p", [Label])
    end,
    {NewDec, NewEnc} = prepare_dec_enc(Dec, Enc, Required),
    CDataSpec#cdata{dec = NewDec, enc = NewEnc};
prepare_cdata_spec(Junk) ->
    ?err("bad #cdata: ~p", [Junk]).

prepare_attr_spec(#attr{name = Name,
			label = Label,
			required = Required,
			dec = Dec,
			enc = Enc} = Attr) ->
    if (Required == false) or (Required == true) ->
	    ok;
       true ->
	    ?err("bad 'required' in #attr: ~p", [Required])
    end,
    NewLabel = prepare_label(Label, Name),
    {NewDec, NewEnc} = prepare_dec_enc(Dec, Enc, Required),
    Attr#attr{label = NewLabel, dec = NewDec, enc = NewEnc};
prepare_attr_spec(Junk) ->
    ?err("bad #attr: ~p", [Junk]).

prepare_label(Label, Name) ->
    if Label == undefined ->
	    list_to_atom(string:to_lower([$$|binary_to_list(Name)]));
       is_atom(Label) ->
	    case atom_to_list(Label) of
		[$$|_] ->
		    Label;
		_ ->
		    ?err("bad 'label': ~p", [Label])
	    end;
       true ->
	    ?err("bad 'label': ~p", [Label])
    end.

prepare_dec_enc(bool, _, _) ->
    {{to_bool, []}, {from_bool, []}};
prepare_dec_enc({integer, Min, Max}, _, _) ->
    check_min_max(Min, Max),
    {{to_integer, [Min,Max]}, {from_integer, [Min, Max]}};
prepare_dec_enc({float, Min, Max}, _, _) ->
    check_min_max(Min, Max),
    {{to_float, [Min,Max]}, {from_float, [Min, Max]}};
prepare_dec_enc({enum, List}, _, _)
  when is_list(List), List /= [] ->
    {{to_enum, [List]}, {from_enum, [List]}};
prepare_dec_enc(jid, _, _) ->
    {{to_jid, []}, {from_jid, []}};
prepare_dec_enc(base64, _, _) ->
    {{to_base64, []}, {from_base64, []}};
prepare_dec_enc({M1, F1, A1}, {M2, F2, A2}, _)
  when is_atom(M1), is_atom(F1), is_list(A1),
       is_atom(M2), is_atom(F2), is_list(A2) ->
    {{M1, F1, A1}, {M2, F2, A2}};
prepare_dec_enc(undefined, undefined, true) ->
    {{to_not_empty, []}, {from_not_empty, []}};
prepare_dec_enc(undefined, undefined, _) ->
    {undefined, undefined};
prepare_dec_enc(Dec, Enc, _) ->
    ?err("bad 'dec' or 'enc': {~p, ~p}", [Dec, Enc]).

check_min_max(unlimited, unlimited) ->
    ok;
check_min_max(unlimited, Max) when is_integer(Max) ->
    ok;
check_min_max(Min, unlimited) when is_integer(Min) ->
    ok;
check_min_max(Min, Max)
  when is_integer(Min), is_integer(Max), Min =< Max ->
    ok;
check_min_max(Min, Max) ->
    ?err("invalid 'min' (~p) or 'max' (~p)", [Min, Max]).

process_label(Label, Attrs, CData, SubSpecs) ->
    case is_label(Label) of
	true ->
	    Var = label_to_var(Label),
	    case find_label(Label, Attrs, CData, SubSpecs) of
		#attr{name = AttrName, required = true, dec = MFA} = Spec ->
		    emit(["{ok",com, Var, "} ="]),
		    emit_mfa("xml:get_attr_s(<<\"" ++
			     binary_to_list(AttrName) ++ "\">>, _Attrs)",
			     MFA),
		    emit(com),
		    {Var, Spec};
		#cdata{required = true, dec = MFA} = Spec ->
		    emit(["{ok",com, Var, "} ="]),
		    emit_mfa("xml:get_cdata(_Els)", MFA),
		    emit(com),
		    {Var, Spec};
		#attr{name = AttrName, dec = MFA, default = Default} = Spec ->
		    if Default == "", MFA == undefined ->
			    emit([Var," = xml:get_attr_s(<<\"",
                                  binary_to_list(AttrName),
				  "\">>,_Attrs)",com,nl]);
		       true ->
			    emit([Var," = case xml:get_attr_s(<<\"",
                                  binary_to_list(AttrName),
				  "\">>,_Attrs) of",nl]),
			    gen_dec(MFA, Default)
		    end,
		    {Var, Spec};
		#cdata{dec = MFA, default = Default} = Spec ->
		    if Default == "", MFA == undefined ->
			    emit([Var," = xml:get_cdata(_Els)",com,nl]);
		       true ->
			    emit([Var," = case xml:get_cdata(_Els) of",nl]),
			    gen_dec(MFA, Default)
		    end,
		    {Var, Spec};
		[#spec{name = SubName, xmlns = XMLNS, label = LabelName,
		       min = 0, max = 1, default = Default} = Spec] ->
		    Val = get_var(),
		    emit([Var," = case get_els(_Els,<<\"",
			  binary_to_list(SubName),"\">>,<<\"",
                          binary_to_list(XMLNS), "\">>",com,0,
			  com,1,") of",nl,"[] -> ",nl,
			  {asis, Default},";",nl,
			  "[",Val,"] -> ",dec_fun(LabelName),
			  "(",Val,") end",com,nl]),
		    {Var, Spec};
		[#spec{name = SubName, xmlns = XMLNS,
		       label = LabelName, min = 1, max = 1} = Spec] ->
		    Val = get_var(),
		    emit(["[",Val,"] = get_els(_Els,<<\"",
			  binary_to_list(SubName),"\">>,<<\"",
                          binary_to_list(XMLNS), "\">>",com,1,
			  com,1,")",com,nl,
			  Var," = ",dec_fun(LabelName),
			  "(",Val,")",com,nl]),
		    {Var, Spec};
		[#spec{name = SubName, xmlns = XMLNS,
		       label = LabelName,
		       min = Min, max = Max} = Spec] ->
		    emit([Var," = [",dec_fun(LabelName), "(El) || ",
			  "El <- get_els(_Els, <<\"",
                          binary_to_list(SubName),"\">>,<<\"",
			  binary_to_list(XMLNS),
                          "\">>",com,Min,com,Max,")]",com,nl]),
		    {Var, Spec};
		[#spec{label = LabelName} = Spec | _] ->
		    [$$|T] = atom_to_list(LabelName),
		    emit([Var, " = dec_", T, "(_Els)",com,nl]),
		    {Var, Spec}
	    end;
	false ->
	    Label
    end.

find_label(Label, AttrSpecs, CDataSpec, SubSpecs) ->
    MatchedAttrs = lists:filter(
		     fun(#attr{label = L})
			when Label == L -> true;
			(_) -> false
		     end, AttrSpecs),
    if length(MatchedAttrs) > 1 ->
	    ?err("label '~s' found more than once in attrs", [Label]);
       true ->
	    ok
    end,
    MatchedEls = lists:filter(
		   fun(#spec{label = L})
		      when Label == L -> true;
		      (_) -> false
		   end, SubSpecs),
    case MatchedEls of
	[#spec{default = Default}|[_|_]] ->
	    case lists:all(
		   fun(#spec{min = 0, max = 1}) -> true;
		      (_) -> false
		   end, MatchedEls) of
		true ->
		    case lists:all(
			   fun(#spec{default = D}) when D == Default ->
				   true;
			      (_) ->
				   false
			   end, MatchedEls) of
			true ->
			    ok;
			false ->
			    ?err("specs with label '~s' should "
				 "have identical default", [Label])
		    end;
		false ->
		    ?err("duplicated label ('~s') should always "
			 "point to specs with min = 0 and max = 1", [Label])
	    end;
	_ ->
	    ok
    end,
    MatchCData = (CDataSpec#cdata.label == Label),
    case {MatchedAttrs, MatchedEls, MatchCData} of
	{[], [], false} ->
	    ?err("unresolved label '~s'", [Label]);
	{[], [], true} ->
	    CDataSpec;
	{[MatchedAttr], [], false} ->
	    MatchedAttr;
	{[], Els, false} ->
	    Els;
	_ ->
	    ?err("ambiguous label '~s'", [Label])
    end.

gen_dec(Dec, Default) ->
    Val = get_var(),
    Res = get_var(),
    emit(["<<>> ->",{asis, Default},";",nl,Val," -> "]),
    if Dec == undefined ->
	    emit([Val,nl]);
       true ->
	    emit(["{ok",com,Res,"} = "]),
	    emit_mfa(Val, Dec),
	    emit([nl,com,Res,nl])
    end,
    emit(["end",com,nl]).

gen_result(Res, other, Direction) ->
    gen_result(Res, Direction);
gen_result(Res, list, Direction) ->
    emit("["),
    gen_result(Res, Direction),
    emit("]");
gen_result(Res, tuple, Direction) ->
    emit("{"),
    gen_result(Res, Direction),
    emit("}").

gen_result([H|T], Direction) ->
    case H of
	{Var, Spec} when is_record(Spec, attr);
			 is_record(Spec, cdata);
			 is_record(Spec, spec) ->
	    emit(Var);
	'_' when Direction == out ->
	    emit("undefined");
	'_' when Direction == in ->
	    emit("_");
	Var ->
	    emit({asis, Var})
    end,
    if T /= [] ->
	    emit([",",nl]);
       true ->
	    ok
    end,
    gen_result(T, Direction);
gen_result([], _Direction) ->
    ok.

gen_header(ModuleName) ->
    emit(["%%%----------------------------------------------------------------------",
	  nl, "%%% File: ", ModuleName ++ ".erl",
	  nl, "%%% Purpose: XML codec. Generated automatically.",
	  nl, "%%%          !!! DO NOT EDIT !!!",
	  nl, "%%% Created: ", httpd_util:rfc1123_date(), " by XML generator", nl,
	  "%%%----------------------------------------------------------------------",
	  nl]),
    emit(["-module(",ModuleName,").",nl]).

gen_footer() ->
    XMLGenHrl = "include/xml_gen_lib.hrl",
    case file:read_file(XMLGenHrl) of
	{ok, Data} ->
	    emit(binary_to_list(Data));
	{error, Err} ->
	    Reason = file:format_error(Err),
	    ?err("failed to open ~p: ~s", [XMLGenHrl, Reason])
    end.

%%====================================================================
%% Auxiliary functions
%%====================================================================
emit(V) ->
    asn1ct_gen:emit(V).

emit_mfa(Var, undefined) ->
    emit(Var);
emit_mfa(Var, {M, F, Args}) ->
    emit([M,":",F,"(",Var]),
    [emit([",",{asis, A}]) || A <- Args],
    emit(")");
emit_mfa(Var, {F, Args}) ->
    emit([F,"(",Var]),
    [emit([",",{asis, A}]) || A <- Args],
    emit(")").

is_label(Label) ->
    case catch atom_to_list(Label) of
	[$$|_] ->
	    true;
	_ ->
	    false
    end.

label_to_var(Label) ->
    [$$,H|Var] = atom_to_list(Label),
    Id = get(var_inc),
    put(var_inc, Id+1),
    [H-32|Var] ++ integer_to_list(Id).

get_var() ->
    Id = get(var_inc),
    put(var_inc, Id+1),
    "Var" ++ integer_to_list(Id).

enc_fun(Label) ->
    [$$|T] = atom_to_list(Label),
    "'enc_" ++ T ++ "'".

dec_fun(Label) ->
    [$$|T] = atom_to_list(Label),
    "'dec_" ++ T ++ "'".

group_subspecs(SubSpecs) ->
    Groups = lists:foldl(
	       fun(#spec{label = Label} = Spec, Acc) ->
		       dict:append(Label, Spec, Acc)
	       end, dict:new(), SubSpecs),
    [SpecGroup || {_, SpecGroup} <- dict:to_list(Groups)].

%%====================================================================
%% Auxiliary functions
%%====================================================================
%% @hidden
%% @doc Same as file:consult, but expands known records.
consult(Path) ->
    case file:read_file(Path) of
        {ok, Data} ->
            case get_forms(binary_to_list(Data)) of
                {ok, Forms} ->
                    Trees = lists:map(
                              fun(Form) ->
                                      erl_syntax_lib:map(
                                        fun(T) ->
                                                case erl_syntax:type(T) of
                                                    record_expr ->
                                                        record_to_tuple(T);
                                                    _ ->
                                                        T
                                                end
                                        end, erl_syntax:form_list(Form))
                              end, Forms),
                    TmpFile = filename:join([filename:dirname(Path),
                                             filename:basename(Path) ++ "~"]),
                    Str = lists:map(
                            fun(Tree) ->
                                    AbsForm = erl_syntax:revert_forms(Tree),
                                    case catch erl_eval:exprs(AbsForm, []) of
                                        {'EXIT', _} ->
                                            [];
                                        {value, Term, _} ->
                                            [io_lib:print(Term), $., io_lib:nl()]
                                    end
                            end, Trees),
                    Res = case file:write_file(TmpFile, Str) of
                              ok ->
                                  file:consult(TmpFile);
                              Err ->
                                  Err
                          end,
                    catch file:delete(TmpFile),
                    Res;
                Err ->
                    Err
            end;
        Err ->
            Err
    end.

record_to_tuple(R) ->
    Name = erl_syntax:record_expr_type(R),
    AtomName = list_to_atom(erl_syntax:atom_name(Name)),
    case lists:member(AtomName, [attr, cdata, spec]) of
        true ->
            record_to_tuple(R, tag_record(AtomName));
        false ->
            R
    end.

tag_record(attr) ->
    tag_record(#attr{}, record_info(fields, attr));
tag_record(cdata) ->
    tag_record(#cdata{}, record_info(fields, cdata));
tag_record(spec) ->
    tag_record(#spec{}, record_info(fields, spec)).

tag_record(R, Fs) ->
    [_|Vs] = tuple_to_list(R),
    lists:zip(Fs, Vs).

record_to_tuple(R, TaggedRecord) ->
    Name = erl_syntax:record_expr_type(R),
    Fs = erl_syntax:record_expr_fields(R),
    KeyVals = lists:map(
                fun(F) ->
                        Key = list_to_atom(
                                erl_syntax:atom_name(
                                  erl_syntax:record_field_name(F))),
                        Val = erl_syntax:record_field_value(F),
                        {Key, Val}
                end, Fs),
    Vals = lists:map(
             fun({K, Default}) ->
                     case lists:keyfind(K, 1, KeyVals) of
                         {_, Val} ->
                             Val;
                         false ->
                             term_to_form(Default)
                     end
             end, TaggedRecord),
    erl_syntax:tuple([Name|Vals]).

term_to_form(Term) ->
    {ok, [[Form]]} = get_forms(lists:flatten([io_lib:print(Term), $.])),
    Form.

get_forms(String) ->
    case scan(String) of
        {ok, Terms} ->
            lists:foldl(
              fun(_, {error, _} = Err) ->
                      Err;
                 (Term, {ok, Acc}) ->
                      case erl_parse:parse_exprs(Term) of
                          {ok, Form} ->
                              {ok, [Form|Acc]};
                          Err ->
                              Err
                      end
              end, {ok, []}, lists:reverse(Terms));
        Err ->
            Err
    end.

scan(String) ->
    {ok, Tokens, _} = erl_scan:string(String),
    case lists:foldl(
           fun({dot, Line}, {Acc, Res}) ->
                   {[], [lists:reverse([{dot, Line}|Acc])|Res]};
              (Token, {Acc, Res}) ->
                   {[Token|Acc], Res}
           end, {[], []}, Tokens) of
        {[], Terms} when Terms /= [] ->
            {ok, lists:reverse(Terms)};
        {[], []} ->
            {error, empty};
        {_, []} ->
            {error, parse_error}
    end.
