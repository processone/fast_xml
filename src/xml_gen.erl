%%%-------------------------------------------------------------------
%%% File    : xml_gen.erl
%%% Author  : Evgeniy Khramtsov <ekhramtsov@process-one.net>
%%% Description : XML code generator.
%%%
%%% Created : 22 Jun 2009 by Evgeniy Khramtsov <ekhramtsov@process-one.net>
%%%-------------------------------------------------------------------
-module(xml_gen).

%% Generator API
-export([compile/1]).
%% Runtime API
-export([reverse/3]).
%% Runtime built-in decoders/encoders
-export([dec_int/1, dec_int/3, dec_enum/2,
         enc_int/1, enc_enum/1, not_empty/1]).

-include("xml_gen.hrl").
-include("xml.hrl").

-define(err(F, Args),
	begin
	    io:format("** xml_gen error: " ++ F ++ "~n", Args),
	    erlang:error(badarg)
	end).

-define(info(F, Args), io:format("xml_gen: " ++ F ++ "~n", Args)).
-define(warn(F, Args), io:format("* xml_gen warning: " ++ F ++ "~n", Args)).

-record(state, {labels = [], ast = [], all_specs = [],
                known_functions = [], silent = false}).

%%====================================================================
%% Compiler API
%%====================================================================
compile(Path) ->
    case consult(Path) of
        {ok, Terms, Forms} ->
            SpecMods = lists:flatmap(
                         fun({spec, Tag, Spec}) ->
                                 [{Tag, Spec}];
                            (_) ->
                                 []
                      end, Terms),
            compile(SpecMods, Forms, Path);
        Err ->
            Err
    end.

%%====================================================================
%% Runtime API
%%====================================================================
-spec reverse([any()], pos_integer(), pos_integer() | infinity) -> [any()].

reverse(L, Min, Max) ->
    reverse(L, Min, Max, 0, []).

reverse([H|T], Min, Max, Count, Acc) ->
    reverse(T, Min, Max, Count+1, [H|Acc]);
reverse([], Min, Max, Count, Acc) when Count >= Min, Count =< Max ->
    Acc.

%%====================================================================
%% Runtime decoders/encoders
%%====================================================================
-spec dec_int(binary()) -> integer().

dec_int(Val) ->
    dec_int(Val, infinity, infinity).

dec_int(Val, Min, Max) ->
    case erlang:binary_to_integer(Val) of
        Int when Int =< Max, Min == infinity ->
            Int;
        Int when Int =< Max, Int >= Min ->
            Int
    end.

-spec enc_int(integer()) -> binary().

enc_int(Int) ->
    erlang:integer_to_binary(Int).

-spec dec_enum(binary(), [atom()]) -> atom().

dec_enum(Val, Enums) ->
    AtomVal = erlang:binary_to_existing_atom(Val, utf8),
    case lists:member(AtomVal, Enums) of
        true ->
            AtomVal
    end.

-spec enc_enum(atom()) -> binary().

enc_enum(Atom) ->
    erlang:atom_to_binary(Atom, utf8).

-spec not_empty(binary()) -> binary().

not_empty(<<_, _/binary>> = Val) ->
    Val.

%%====================================================================
%% Internal functions
%%====================================================================
compile(TaggedSpecs, Forms, Path) ->
    KnownFuns = lists:flatmap(
                  fun(F) ->
                          case erl_syntax:type(F) of
                              function ->
                                  [erl_syntax_lib:analyze_function(F)];
                              _ ->
                                  []
                          end
                  end, Forms),
    FileName = filename:basename(Path),
    ModName = filename:rootname(FileName),
    #state{ast = AST} = lists:foldl(
                          fun({Tag, Spec}, State) ->
                                  NewState = spec_to_AST(Spec, [Tag], State),
                                  NewState#state{labels = []}
                          end, #state{all_specs = TaggedSpecs,
                                      known_functions = KnownFuns},
                          TaggedSpecs),
    Decoder = make_xmlns_decoder(TaggedSpecs),
    NewAST = [Decoder|Forms ++ AST],
    Exports = erl_syntax:attribute(
                erl_syntax:atom("export"),
                [erl_syntax:list(
                   [erl_syntax:arity_qualifier(
                      erl_syntax:atom("decode"),
                      erl_syntax:integer(1))])]),
    Module = erl_syntax:attribute(
               erl_syntax:atom("module"),
               [erl_syntax:atom(list_to_atom(ModName))]),
    CompilerOpts = erl_syntax:attribute(
                     erl_syntax:atom("compile"),
                     [erl_syntax:list(
                        [erl_syntax:atom("nowarn_unused_function")])]),
    Decoder = make_xmlns_decoder(TaggedSpecs),
    ResultAST = erl_syntax:form_list([Module, CompilerOpts, Exports|NewAST]),
    DirName = filename:dirname(Path),
    file:write_file(
      filename:join([DirName, ModName ++ ".erl"]),
      [erl_prettypr:format(ResultAST), io_lib:nl()]).

make_xmlns_decoder(TaggedSpecs) ->
    Clauses = lists:map(
                fun({Tag, #spec{xmlns = XMLNS, name = Name}}) ->
                        erl_syntax:clause(
                          [erl_syntax:tuple(
                             [abstract(Name),
                              abstract(XMLNS)])],
                          none,
                          [make_function_call(
                             make_dec_fun_name([Name, Tag]),
                             [erl_syntax:variable("_el")])])
                end, TaggedSpecs),
    NilClause = erl_syntax:clause(
                  [erl_syntax:tuple(
                     [erl_syntax:variable("_name"),
                      erl_syntax:variable("_xmlns")])],
                  none,
                  [make_function_call(
                     erlang, error,
                     [erl_syntax:tuple(
                        [erl_syntax:atom("unknown_tag"),
                         erl_syntax:variable("_name"),
                         erl_syntax:variable("_xmlns")])])]),
    make_function(
      "decode",
      [erl_syntax:match_expr(
         erl_syntax:tuple([erl_syntax:atom("xmlel"),
                           erl_syntax:variable("_name"),
                           erl_syntax:variable("_attrs"),
                           erl_syntax:underscore()]),
         erl_syntax:variable("_el"))],
      [erl_syntax:case_expr(
         erl_syntax:tuple(
           [erl_syntax:variable("_name"),
            make_function_call(
              xml, get_attr_s,
              [abstract(<<"xmlns">>),
               erl_syntax:variable("_attrs")])]),
         Clauses ++ [NilClause])]).

spec_to_AST(#spec{name = Name, label = Label, xmlns = XMLNS,
                  result = Result, attrs = Attrs,
                  cdata = CData, els = Specs} = Spec,
            Parents,
            #state{silent = Silent, known_functions = KnownFuns} = State) ->
    check_spec(Spec, KnownFuns),
    NewLabel = prepare_label(Label, Name),
    NewParents = [Name|Parents],
    NewState = lists:foldl(
                 fun(SubSpec, StateAcc) ->
                         spec_to_AST(SubSpec, NewParents, StateAcc)
                 end, State, Specs),
    AttrAST = lists:map(
                fun(#attr{name = AttrName,
                          required = Required,
                          dec = AttrDecF,
                          default = AttrDefault}) ->
                        DecFun = make_dec_fun_name([AttrName|NewParents]),
                        make_decoding_MFA(DecFun, Name, XMLNS, AttrName,
                                          Required, AttrDefault,
                                          prepare_MFA(AttrDecF, KnownFuns))
                end, Attrs),
    {CDataAST, CDataLabel} =
        begin
            #cdata{label = CDataLabl,
                   required = CDataRequired,
                   dec = CDataDecF,
                   default = CDataDefault} = CData,
            CDataFun = make_dec_fun_name(["cdata"|NewParents]),
            {[make_decoding_MFA(CDataFun, Name, XMLNS, <<>>,
                                CDataRequired, CDataDefault,
                                prepare_MFA(CDataDecF, KnownFuns))],
             [{CDataLabl, CDataFun, CData}]}
        end,
    AttrLabels = lists:map(
                   fun(#attr{name = AttrName,
                             label = AttrLabel} = AttrSpec) ->
                           NewAttrLabel = prepare_label(AttrLabel, AttrName),
                           AttrFun = make_dec_fun_name([AttrName|NewParents]),
                           {NewAttrLabel, AttrFun, AttrSpec}
                   end, Attrs),
    Labels = NewState#state.labels ++ AttrLabels ++ CDataLabel,
    AST = make_el_fun(make_dec_fun_name(NewParents),
                      Result, Labels, Name),
    NewAST = if Silent ->
                     NewState#state.ast;
                true ->
                     AST ++ AttrAST ++ CDataAST ++ NewState#state.ast
             end,
    NewLabels = [{NewLabel, make_dec_fun_name(NewParents), Spec}
                 |NewState#state.labels],
    NewState#state{ast = NewAST, labels = NewLabels};
spec_to_AST(SpecName, _Parents, #state{all_specs = AllSpecs,
                                       silent = Silent} = State) ->
    case lists:keyfind(SpecName, 1, AllSpecs) of
        {_, #spec{} = Spec} ->
            NewState = spec_to_AST(Spec, [SpecName], State#state{silent = true}),
            NewState#state{silent = Silent};
        {_, _} ->
            bad_spec({wrong_spec_reference, SpecName});
        false ->
            bad_spec({unresolved_spec_reference, SpecName})
    end.

%% Replace in `Term' every label found in `Labels'
%% with the corresponding value.
subst_labels(TagName, Term, Labels) ->
    Tree = abstract(Term),
    erl_syntax_lib:mapfold(
      fun(T, Calls) ->
              case erl_syntax:type(T) of
                  atom ->
                      Label = erl_syntax:atom_value(T),
                      case is_label(Label) of
                          true ->
                              Var = label_to_var(Label),
                              Call = make_label_call(TagName,
                                                     Var,
                                                     Label,
                                                     Labels),
                              {Var, [Call|Calls]};
                          false ->
                              {T, Calls}
                      end;
                  _ ->
                      {T, Calls}
              end
      end, [], Tree).

make_label_call(TagName, Var, Label, Labels) ->
    case lists:flatmap(
           fun({L, F, S}) when L == Label ->
                   [{F, S}];
              (_) ->
                   []
           end, Labels) of
        [] when Label == '$_els' ->
            {Var, sub_els};
        [] ->
            bad_spec({unresolved_label, Label, TagName});
        [{F, #attr{} = S}] ->
            {Var, {F, S}};
        [{F, #cdata{} = S}] ->
            {Var, {F, S}};
        [{F, S}] ->
            {Var, [{F, S}]};
        FSs ->
            check_group(Label, [S || {_F, S} <- FSs]),
            {Var, FSs}
    end.

make_el_fun(FunName, Result, Labels, Name) ->
    {ResultWithVars, Calls} = subst_labels(Name, Result, Labels),
    ElCDataVars = lists:flatmap(
                    fun({V, [{_, #spec{}}|_]}) ->
                            [V];
                       ({V, {_, #cdata{}}}) ->
                            [V];
                       ({V, sub_els}) ->
                            [V];
                       (_) ->
                            []
                    end, Calls),
    AttrVars = lists:flatmap(
                 fun({V, {_, #attr{}}}) ->
                         [V];
                    (_) ->
                         []
                 end, Calls),
    AttrMatch =
        if AttrVars /= [] ->
                AttrPattern = tuple_or_single_var(AttrVars),
                AttrCall = make_function_call(
                             FunName ++ "_attrs",
                             [erl_syntax:variable("_attrs")|
                              lists:flatmap(
                                fun({_Var, {_F, #attr{}}}) ->
                                        [abstract(<<>>)];
                                   (_) ->
                                        []
                                end, Calls)]),
                [erl_syntax:match_expr(AttrPattern, AttrCall)];
           true ->
                []
        end,
    ElCDataMatch =
        if ElCDataVars /= [] ->
                [erl_syntax:match_expr(
                   tuple_or_single_var(ElCDataVars),
                   make_function_call(
                     FunName ++ "_els",
                     [erl_syntax:variable("_els")|
                      lists:flatmap(
                        fun({_Var, [{_F, #spec{min = 0, max = 1,
                                               default = Def}}|_]}) ->
                                [abstract(Def)];
                           ({_Var, {_F, #cdata{}}}) ->
                                [abstract(<<>>)];
                           ({_Var, [{_F, #spec{}}|_]}) ->
                                [erl_syntax:list([])];
                           ({_Var, sub_els}) ->
                                [erl_syntax:list([])];
                           (_) ->
                                []
                        end, Calls)]))];
           true ->
                []
        end,
    [make_function(
       FunName,
       [erl_syntax:tuple([erl_syntax:atom("xmlel"),
                          erl_syntax:underscore(),
                          erl_syntax:variable("_attrs"),
                          erl_syntax:variable("_els")])],
       AttrMatch ++ ElCDataMatch ++ [ResultWithVars])]
     ++ make_els_cdata_fun(FunName ++ "_els", Calls, ElCDataVars)
     ++ make_attrs_fun(FunName ++ "_attrs", Calls, AttrVars).

make_attrs_fun(FunName, Calls, AttrVars) ->
    Clauses =
        lists:flatmap(
          fun({Var, {_F, #attr{name = Name}}}) ->
                  Pattern = [erl_syntax:list(
                               [erl_syntax:tuple(
                                  [abstract(Name),
                                   erl_syntax:variable("_val")])],
                               erl_syntax:variable("_attrs")) |
                             lists:map(
                               fun(V) when V == Var ->
                                       VName = erl_syntax:variable_literal(V),
                                       erl_syntax:variable("_" ++ VName);
                                  (V) ->
                                       V
                               end, AttrVars)],
                  Body = [make_function_call(
                            FunName,
                            [erl_syntax:variable("_attrs") |
                             lists:map(
                               fun(V) when V == Var ->
                                       erl_syntax:variable("_val");
                                  (V) ->
                                       V
                               end, AttrVars)])],
                  [erl_syntax:clause(Pattern, none, Body)];
             (_) ->
                  []
          end, Calls),
    if Clauses /= [] ->
            PassClause = erl_syntax:clause(
                           [erl_syntax:list(
                              [erl_syntax:underscore()],
                              erl_syntax:variable("_attrs"))
                            |AttrVars],
                           none,
                           [make_function_call(
                              FunName,
                              [erl_syntax:variable("_attrs")|AttrVars])]),
            Result = lists:flatmap(
                       fun({Var, {F, #attr{}}}) ->
                               [make_function_call(F, [Var])];
                          (_) ->
                               []
                       end, Calls),
            NilClause = erl_syntax:clause(
                          [erl_syntax:list([])|AttrVars],
                          none,
                          [tuple_or_single_var(Result)]),
            [erl_syntax:function(
               erl_syntax:atom(FunName),
               Clauses ++ [PassClause, NilClause])];
       true ->
            []
    end.

make_els_cdata_fun(FunName, Calls, ElCDataVars) ->    
    Clauses =
        lists:flatmap(
          fun({Var, [{_, #spec{}}|_] = FSs}) ->
                  lists:map(
                    fun({F, #spec{name = Name, xmlns = XMLNS}}) ->
                            Pattern =
                                [erl_syntax:list(
                                   [erl_syntax:match_expr(
                                      erl_syntax:tuple(
                                        [erl_syntax:atom("xmlel"),
                                         abstract(Name),
                                         erl_syntax:variable("_attrs"),
                                         erl_syntax:underscore()]),
                                      erl_syntax:variable("_el"))],
                                   erl_syntax:variable("_els"))|
                                 ElCDataVars],
                            FunCall =
                                make_function_call(
                                  FunName,
                                  [erl_syntax:variable("_els")|
                                   lists:flatmap(
                                     fun({V, [{_, #spec{min = Min, max = Max}}|_]})
                                           when V == Var ->
                                             FCall = make_function_call(
                                                       F,
                                                       [erl_syntax:variable("_el")]),
                                             if Min == 0, Max == 1 ->
                                                     [FCall];
                                                true ->
                                                     [erl_syntax:list(
                                                        [FCall], Var)]
                                             end;
                                        ({V, sub_els}) ->
                                             [V];
                                        ({V, [{_, #spec{}}|_]}) ->
                                             [V];
                                        ({V, #cdata{}}) ->
                                             [V];
                                        (_) ->
                                             []
                                     end, Calls)]),
                            Body =
                                [erl_syntax:case_expr(
                                   make_function_call(
                                     xml, get_attr_s,
                                     [abstract(<<"xmlns">>),
                                      erl_syntax:variable("_attrs")]),
                                   [erl_syntax:clause(
                                      [abstract(XMLNS)],
                                      none,
                                      [FunCall]),
                                    erl_syntax:clause(
                                      [erl_syntax:underscore()],
                                      none,
                                      [make_function_call(
                                         FunName,
                                         [erl_syntax:variable("_els")|
                                          lists:flatmap(
                                            fun({V, sub_els}) ->
                                                    [erl_syntax:list(
                                                       [make_function_call(
                                                          decode,
                                                          [erl_syntax:variable("_el")])],
                                                       V)];
                                               ({V, [{_, #spec{}}|_]}) ->
                                                    [V];
                                               ({V, {_, #cdata{}}}) ->
                                                    [V];
                                               (_) ->
                                                    []
                                            end, Calls)])])])],
                            erl_syntax:clause(
                              [erl_syntax:underscore()],
                              none,
                              [make_function_call(
                                 FunName,
                                 [erl_syntax:variable("_els")
                                  |ElCDataVars])]),
                            erl_syntax:clause(Pattern, none, Body)
                    end, FSs);
             ({Var, {_F, #cdata{}}}) ->
                  Pattern = [erl_syntax:list(
                               [erl_syntax:tuple(
                                  [erl_syntax:atom("xmlcdata"),
                                   erl_syntax:variable("_data")])],
                               erl_syntax:variable("_els"))
                             |ElCDataVars],
                  Body = [make_function_call(
                            FunName,
                            [erl_syntax:variable("_els")|
                             lists:map(
                               fun(V) when V == Var ->
                                       erl_syntax:binary(
                                         [erl_syntax:binary_field(
                                            Var,
                                            [erl_syntax:atom("binary")]),
                                          erl_syntax:binary_field(
                                            erl_syntax:variable("_data"),
                                            [erl_syntax:atom("binary")])]);
                                  (V) ->
                                       V
                               end, ElCDataVars)])],
                  [erl_syntax:clause(Pattern, none, Body)];
             (_) ->
                  []
          end, Calls),
    if Clauses /= [] ->
            SubClause =
                case have_special_label(Calls, sub_els) of
                    true ->
                        [erl_syntax:clause(
                           [erl_syntax:list(
                              [erl_syntax:match_expr(
                                 erl_syntax:tuple(
                                   [erl_syntax:atom("xmlel"),
                                    erl_syntax:underscore(),
                                    erl_syntax:underscore(),
                                    erl_syntax:underscore()]),
                                 erl_syntax:variable("_el"))],
                              erl_syntax:variable("_els"))|
                            ElCDataVars],
                           none,
                           [make_function_call(
                              FunName,
                              [erl_syntax:variable("_els")|
                               lists:flatmap(
                                 fun({Var, sub_els}) ->
                                         [erl_syntax:list(
                                            [make_function_call(
                                               decode,
                                               [erl_syntax:variable("_el")])],
                                            Var)];
                                    ({Var, [{_, #spec{}}|_]}) ->
                                         [Var];
                                    ({Var, {_, #cdata{}}}) ->
                                         [Var];
                                    (_) ->
                                         []
                                 end, Calls)])])];
                    false ->
                        []
                end,
            PassClause = erl_syntax:clause(
                           [erl_syntax:list(
                              [erl_syntax:underscore()],
                              erl_syntax:variable("_els"))|
                            ElCDataVars],
                           none,
                           [make_function_call(
                              FunName,
                              [erl_syntax:variable("_els")|
                               ElCDataVars])]),
            Result = lists:flatmap(
                       fun({Var, [{_F, #spec{max = 1}}|_]}) ->
                               [Var];
                          ({Var, [{_F, #spec{min = 0, max = infinity}}|_]}) ->
                               [make_function_call(lists, reverse, [Var])];
                          ({Var, [{_F, #spec{min = Min, max = Max}}|_]}) ->
                               [make_function_call(
                                  xml_gen, reverse,
                                  [Var, abstract(Min), abstract(Max)])];
                          ({Var, {F, #cdata{}}}) ->
                               [make_function_call(F, [Var])];
                          ({Var, sub_els}) ->
                               [make_function_call(lists, reverse, [Var])];
                          (_) ->
                               []
                       end, Calls),
            NilClause = erl_syntax:clause(
                          [erl_syntax:list([])|ElCDataVars],
                          none,
                          [tuple_or_single_var(Result)]),
            [erl_syntax:function(
               erl_syntax:atom(FunName),
               Clauses ++ SubClause ++ [PassClause, NilClause])];
       true ->
            []
    end.

make_decoding_MFA(FunName, TagName, TagNS, AttrName,
                  IsRequired, Default, DecMFA) ->
    Type = case AttrName of
               <<>> -> "cdata";
               _ -> "attr"
           end,
    Clause1 = erl_syntax:clause(
                [abstract(<<>>)],
                none,
                [if IsRequired ->
                         make_function_call(
                           erlang, error,
                           [erl_syntax:tuple(
                              [erl_syntax:atom("missing_" ++ Type),
                               abstract(AttrName),
                               abstract(TagName),
                               abstract(TagNS)])]);
                    true ->
                         abstract(Default)
                 end]),
    Body = case DecMFA of
               {M, F, Args} ->
                   make_function_call(
                     M, F,
                     [erl_syntax:variable("_val")|
                      [abstract(Arg) || Arg <- Args]]);
               {F, Args} ->
                   make_function_call(
                     F,
                     [erl_syntax:variable("_val")|
                      [abstract(Arg) || Arg <- Args]]);
               undefined ->
                   erl_syntax:variable("_val")
           end,
    Catch = case DecMFA of
                undefined ->
                    Body;
                _ ->
                    erl_syntax:case_expr(
                      erl_syntax:catch_expr(Body),
                      [erl_syntax:clause(
                         [erl_syntax:tuple([erl_syntax:atom("EXIT"),
                                            erl_syntax:underscore()])],
                         none,
                         [make_function_call(
                            erlang, error,
                            [erl_syntax:tuple(
                               [erl_syntax:atom("bad_" ++ Type ++ "_value"),
                                abstract(AttrName),
                                abstract(TagName),
                                abstract(TagNS)])])]),
                       erl_syntax:clause(
                         [erl_syntax:variable("_res")],
                         none,
                         [erl_syntax:variable("_res")])])
            end,
    Clause2 = erl_syntax:clause([erl_syntax:variable("_val")], none, [Catch]),
    erl_syntax:function(erl_syntax:atom(FunName), [Clause1, Clause2]).

make_dec_fun_name(Vars) ->
    NewVars = lists:foldl(
                fun(Var, Acc) when is_binary(Var) ->
                        [binary_to_list(Var)|Acc];
                   (Var, Acc) when is_atom(Var) ->
                        [atom_to_list(Var)|Acc];
                   (Var, Acc) ->
                        [Var|Acc]
                end, [], Vars),
    "decode_" ++ string:join(NewVars, "_").

%% Fun(Args) -> Body.
make_function(Fun, Args, Body) ->
    erl_syntax:function(
      erl_syntax:atom(Fun),
      [erl_syntax:clause(Args, none, Body)]).

%% Fun(Args).
make_function_call(Fun, Args) ->
    erl_syntax:application(
      none,
      erl_syntax:atom(Fun),
      Args).

%% Mod:Fun(Args).
make_function_call(Mod, Fun, Args) ->
    erl_syntax:application(
      erl_syntax:atom(Mod),
      erl_syntax:atom(Fun),
      Args).

abstract(<<>>) ->
    erl_syntax:abstract(<<>>);
abstract(Bin) when is_binary(Bin) ->
    erl_syntax:binary(
      [erl_syntax:abstract(
         binary_to_list(Bin))]);
abstract(Term) ->
    erl_syntax:abstract(Term).

label_to_var(Label) ->
    case atom_to_list(Label) of
        [$$,$_|[H|T]] ->
            erl_syntax:variable([$_, $_, string:to_upper(H)|T]);
        [$$|[H|T]] ->
            erl_syntax:variable([string:to_upper(H)|T])
    end.

have_special_label(Ss, sub_els) ->
    lists:any(
      fun({_, sub_els}) ->
              true;
         (_) ->
              false
      end, Ss).

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

prepare_MFA({F, A}, KnownFuns) ->
    case lists:member({F, length(A)+1}, KnownFuns) of
        true ->
            {F, A};
        false ->
            {?MODULE, F, A}
    end;
prepare_MFA(MFA, _) ->
    MFA.

tuple_or_single_var([Var]) ->
    Var;
tuple_or_single_var([_|_] = Vars) ->
    erl_syntax:tuple(Vars).

%%====================================================================
%% Auxiliary functions
%%====================================================================
%% Checks
check_spec(#spec{name = Name}, _)
  when not is_binary(Name) ->
    bad_spec({wrong_name, Name});
check_spec(#spec{name = Name, min = Min}, _)
  when not (is_integer(Min) andalso Min >= 0) ->
    bad_spec({wrong_min, Min, Name});
check_spec(#spec{name = Name, max = Max}, _)
  when not ((is_integer(Max) andalso Max > 0) orelse (Max == infinity)) ->
    bad_spec({wrong_max, Max, Name});
check_spec(#spec{name = Name, min = Min, max = Max}, _)
  when Min > Max ->
    bad_spec({min_over_max, Min, Max, Name});
check_spec(#spec{name = Name, xmlns = XMLNS}, _) when not is_binary(XMLNS) ->
    bad_spec({wrong_xmlns, XMLNS, Name});
check_spec(#spec{name = Name, els = SubSpecs}, _) when not is_list(SubSpecs) ->
    bad_spec({wrong_sub_specs, SubSpecs, Name});
check_spec(#spec{name = Name, attrs = Attrs}, _) when not is_list(Attrs) ->
    bad_spec({wrong_attrs, Attrs, Name});
check_spec(#spec{name = Name, label = Label}, _) when not is_atom(Label) ->
    bad_spec({wrong_label, Label, Name});
check_spec(#spec{name = Name, label = Label, attrs = Attrs, cdata = CData},
           KnownFunctions) ->
    case (is_label(Label) or (Label == undefined)) of
        true ->
            lists:foreach(
              fun(Attr) -> check_attr_spec(Name, Attr, KnownFunctions) end,
              Attrs),
            check_cdata_spec(Name, CData, KnownFunctions);
        false ->
            bad_spec({wrong_label_format, Label, Name})
    end;
check_spec(Junk, _) ->
    bad_spec({not_spec, Junk}).

check_attr_spec(Name, #attr{name = AName}, _)
  when not is_binary(AName) ->
    bad_spec({wrong_attr_name, AName, Name});
check_attr_spec(Name, #attr{name = AName, label = Label}, _)
  when not is_atom(Label) ->
    bad_spec({wrong_attr_label, Label, AName, Name});
check_attr_spec(Name, #attr{name = AName, required = Req}, _)
  when not (Req == false orelse Req == true) ->
    bad_spec({wrong_attr_required, Req, AName, Name});
check_attr_spec(Name, #attr{name = AName, label = Label,
                            dec = DecF, enc = EncF}, KnownFunctions) ->
    check_dec_fun(DecF, KnownFunctions),
    check_enc_fun(EncF, KnownFunctions),
    case (is_label(Label) or (Label == undefined)) of
        false ->
            bad_spec({wrong_attr_label_format, Label, AName, Name});
        true ->
            ok
    end;
check_attr_spec(Name, Junk, _) ->
    bad_spec({not_attr_spec, Junk, Name}).

check_cdata_spec(Name, #cdata{label = Label}, _)
  when not is_atom(Label) ->
    bad_spec({wrong_cdata_label, Label, Name});
check_cdata_spec(Name, #cdata{required = Req}, _)
  when not (Req == false orelse Req == true) ->
    bad_spec({wrong_cdata_required, Req, Name});
check_cdata_spec(Name, #cdata{label = Label, dec = DecF, enc = EncF},
                 KnownFunctions) ->
    check_dec_fun(DecF, KnownFunctions),
    check_enc_fun(EncF, KnownFunctions),
    case (is_label(Label) or (Label == undefined)) of
        false ->
            bad_spec({wrong_cdata_label_format, Label, Name});
        true ->
            ok
    end;
check_cdata_spec(Name, Junk, _) ->
    bad_spec({not_cdata_spec, Junk, Name}).

check_group(Label, Specs) ->
    case lists:all(
           fun(S) ->
                   is_record(S, spec)
           end, Specs) of
        true ->
            #spec{default = Default} = hd(Specs),
            case lists:all(
                   fun(#spec{default = D}) ->
                           D == Default
                   end, Specs) of
                true ->
                    case lists:all(
                           fun(#spec{min = 0, max = 1}) ->
                                   true;
                              (_) ->
                                   false
                           end, Specs) of
                        true ->
                            ok;
                        false ->
                            bad_spec({wrong_min_max_in_group, Label})
                    end;
                false ->
                    bad_spec({different_defaults_in_group, Label})
            end;
        false ->
            bad_spec({only_specs_allowed_in_group, Label})
    end.

check_dec_fun({Mod, Fun, Args}, _)
  when is_atom(Mod) andalso is_atom(Fun) andalso is_list(Args) ->
    ok;
check_dec_fun({Fun, Args}, KnownFunctions)
  when is_atom(Fun) andalso is_list(Args) ->
    Arity = length(Args) + 1,
    case erlang:function_exported(?MODULE, Fun, Arity) of
        true ->
            ok;
        false ->
            case lists:member({Fun, Arity}, KnownFunctions) of
                true ->
                    ok;
                false ->
                    bad_spec({unknown_dec_fun, {Fun, Args}})
            end
    end;
check_dec_fun(undefined, _) ->
    ok;
check_dec_fun(Junk, _) ->
    bad_spec({invalid_dec_fun, Junk}).

check_enc_fun({Mod, Fun, Args}, _)
  when is_atom(Mod) andalso is_atom(Fun) andalso is_list(Args) ->
    ok;
check_enc_fun({Fun, Args}, KnownFunctions)
  when is_atom(Fun) andalso is_list(Args) ->
    Arity = length(Args) + 1,
    case erlang:function_exported(?MODULE, Fun, Arity) of
        true ->
            ok;
        false ->
            case lists:member({Fun, Arity}, KnownFunctions) of
                true ->
                    ok;
                false ->
                    bad_spec({unknown_enc_fun, {Fun, Args}})
            end
    end;
check_enc_fun(undefined, _) ->
    ok;
check_enc_fun(Junk, _) ->
    bad_spec({invalid_enc_fun, Junk}).

is_label(Label) when not is_atom(Label) ->
    false;
is_label(Label) ->
    case atom_to_list(Label) of
        "$_els" ->
            true;
        [$$,$_|_] ->
            false;
        [$$,_|_] ->
            true;
        _ ->
            false
    end.

bad_spec(Err) ->
    erlang:error({bad_spec, Err}).

%% @hidden
%% @doc Same as file:consult, but expands known records.
consult(Path) ->
    case file:read_file(Path) of
        {ok, Data} ->
            case get_forms(binary_to_list(Data)) of
                {ok, Forms} ->
                    {Terms, OtherForms} =
                        lists:foldl(
                          fun([Form], {Trms, Other}) ->
                                  Trm = 
                                      erl_syntax_lib:map(
                                        fun(T) ->
                                                case erl_syntax:type(T) of
                                                    record_expr ->
                                                        record_to_tuple(T);
                                                    _ ->
                                                        T
                                                end
                                        end, erl_syntax:form_list([Form])),
                                  {[Trm|Trms], Other};
                             (Form, {Trms, Other}) ->
                                          {Trms, [Form|Other]}
                          end, {[], []}, Forms),
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
                            end, Terms),
                    Res = case file:write_file(TmpFile, Str) of
                              ok ->
                                  case file:consult(TmpFile) of
                                      {ok, R} ->
                                          {ok, R, OtherForms};
                                      Err ->
                                          Err
                                  end;
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
                             erl_syntax:abstract(Default)
                     end
             end, TaggedRecord),
    erl_syntax:tuple([Name|Vals]).

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
                          _ ->
                              case erl_parse:parse_form(Term) of
                                  {ok, AbsForm} ->
                                      {ok, [AbsForm|Acc]};
                                  Err ->
                                      Err
                              end
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
