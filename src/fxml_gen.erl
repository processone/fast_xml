%%%----------------------------------------------------------------------
%%% File    : xml_gen.erl
%%% Author  : Evgeniy Khramtsov <ekhramtsov@process-one.net>
%%% Purpose : XML code generator
%%% Created : 22 Jun 2009 by Evgeniy Khramtsov <ekhramtsov@process-one.net>
%%%
%%%
%%% Copyright (C) 2002-2020 ProcessOne, SARL. All Rights Reserved.
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

-module(fxml_gen).

-compile(debug_info).
-compile({parse_transform, fxml_gen_pt}).
%% Generator API
-export([compile/1, compile/2]).
%% Runtime API
-export([format_error/1, io_format_error/1, get_attr/3]).
%% Runtime built-in decoders/encoders
-export([dec_int/1, dec_int/3, dec_enum/2, enc_int/1, enc_enum/1, not_empty/1,
	enc_xmlns_attrs/2, choose_top_xmlns/3,
	 register_module/2, unregister_module/2, recompile_resolver/2]).

-include("fxml_gen.hrl").
-include("fxml.hrl").

-define(err(F, Args),
	begin
	    io:format("** fxml_gen error: " ++ F ++ "~n", Args),
	    erlang:error(badarg)
	end).

-define(info(F, Args), io:format("fxml_gen: " ++ F ++ "~n", Args)).
-define(warn(F, Args), io:format("* fxml_gen warning: " ++ F ++ "~n", Args)).

%%====================================================================
%% Compiler API
%%====================================================================
compile(Path) ->
    compile(Path, []).

compile(Path, Opts) ->
    case catch do_compile(Path, Opts) of
        ok ->
            ok;
        Err ->
            io:format("failed to compile ~p: ~p~n", [Path, Err]),
            Err
    end.

do_compile(Path, Opts) ->
    case get_files_from_predefined_dir() of
	{ok, Files} ->
	    case lists:foldr(
		   fun(_, {error, _} = Err) ->
			   Err;
		      (File, {AccTs, AccFs}) ->
			   case consult(File) of
			       {ok, Ts, Fs} ->
				   {Ts ++ AccTs, Fs ++ AccFs};
			       Err ->
				   Err
			   end
		   end, {[], []}, [Path|Files]) of
		{error, Why} ->
		    {error, Why};
		{Terms, Forms} ->
		    Elems = lists:flatmap(
			      fun({Tag, Elem}) when is_atom(Tag),
						    is_record(Elem, elem) ->
				      [{Tag, Elem}];
				 (_) ->
				      []
			      end, Terms),
		    compile(Elems, Forms, Path, Opts)
	    end;
        Err ->
            Err
    end.

get_files_from_predefined_dir() ->
    case os:getenv("FXML_GEN_DIR") of
	false ->
	    {ok, []};
	Dir ->
	    case file:list_dir(Dir) of
		{ok, Files} ->
		    {ok, lists:flatmap(
			   fun(File) ->
				   case filename:extension(File) of
				       ".spec" ->
					   [filename:join(Dir, File)];
				       _ ->
					   []
				   end
			   end, Files)};
		{error, _} = Err ->
		    Err
	    end
    end.

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
%% Runtime builtins
%%====================================================================
format_error({bad_attr_value, Attr, Tag, XMLNS}) ->
    <<"Bad value of attribute '", Attr/binary,
      "' in tag <", Tag/binary, "/> qualified by namespace '",
      XMLNS/binary, "'">>;
format_error({bad_cdata_value, <<>>, Tag, XMLNS}) ->
    <<"Bad value of cdata in tag <", Tag/binary,
      "/> qualified by namespace '", XMLNS/binary, "'">>;
format_error({missing_tag, Tag, XMLNS}) ->
    <<"Missing tag <", Tag/binary,
      "/> qualified by namespace '", XMLNS/binary, "'">>;
format_error({missing_attr, Attr, Tag, XMLNS}) ->
    <<"Missing attribute '", Attr/binary,
      "' in tag <", Tag/binary,
      "/> qualified by namespace '", XMLNS/binary, "'">>;
format_error({missing_cdata, <<>>, Tag, XMLNS}) ->
    <<"Missing cdata in tag <", Tag/binary,
      "/> qualified by namespace '", XMLNS/binary, "'">>;
format_error({unknown_tag, Tag, XMLNS}) ->
    <<"Unknown tag <", Tag/binary,
      "/> qualified by namespace '", XMLNS/binary, "'">>;
format_error({missing_tag_xmlns, Tag}) ->
    <<"Missing namespace for tag <", Tag/binary, "/>">>.

io_format_error({bad_attr_value, Attr, Tag, XMLNS}) ->
    {<<"Bad value of attribute '~s' in tag <~s/> qualified by namespace '~s'">>,
     [Attr, Tag, XMLNS]};
io_format_error({bad_cdata_value, <<>>, Tag, XMLNS}) ->
    {<<"Bad value of cdata in tag <~s/> qualified by namespace '~s'">>,
     [Tag, XMLNS]};
io_format_error({missing_tag, Tag, XMLNS}) ->
    {<<"Missing tag <~s/> qualified by namespace '~s'">>, [Tag, XMLNS]};
io_format_error({missing_attr, Attr, Tag, XMLNS}) ->
    {<<"Missing attribute '~s' in tag <~s/> qualified by namespace '~s'">>,
     [Attr, Tag, XMLNS]};
io_format_error({missing_cdata, <<>>, Tag, XMLNS}) ->
    {<<"Missing cdata in tag <~s/> qualified by namespace '~s'">>,
     [Tag, XMLNS]};
io_format_error({unknown_tag, Tag, XMLNS}) ->
    {<<"Unknown tag <~s/> qualified by namespace '~s'">>, [Tag, XMLNS]};
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
choose_top_xmlns(XMLNS, _, _) ->
    XMLNS.

register_module(Mod, ResolverMod) ->
    MD5Sum = try Mod:module_info(md5) of
		 Val -> Val
	     catch error:badarg ->
		     %% 'md5' attribute is relatively new,
		     %% calculate md5sum using beam_lib for
		     %% older OTP
		     {ok, {Mod, Val}} = beam_lib:md5(code:which(Mod)),
		     Val
	     end,
    case orddict:find(Mod, ResolverMod:modules()) of
	{ok, MD5Sum} ->
	    ok;
	_ ->
	    Mods = orddict:store(Mod, MD5Sum, ResolverMod:modules()),
	    recompile_resolver(Mods, ResolverMod)
    end.

unregister_module(Mod, ResolverMod) ->
    case orddict:find(Mod, ResolverMod:modules()) of
	{ok, _} ->
	    Mods = orddict:erase(Mod, ResolverMod:modules()),
	    recompile_resolver(Mods, ResolverMod);
	error ->
	    ok
    end.

%% We don't use erl_syntax here in order not to depend
%% on syntax_tools in runtime
recompile_resolver(Mods, ResolverMod) ->
    Tags = lists:flatmap(
	     fun(M) ->
		     [{Name, XMLNS, M} || {Name, XMLNS} <- M:tags()]
	     end, orddict:fetch_keys(Mods)),
    Records = lists:flatmap(
		fun(M) ->
			[{RecName, RecSize, M}
			 || {RecName, RecSize} <- M:records()]
		end, orddict:fetch_keys(Mods)),
    Lookup1 = string:join(
		lists:map(
		  fun({RecName, RecSize, M}) ->
			  io_lib:format(
			    "lookup({~s}) -> '~s'",
			    [string:join(
			       [io_lib:format("'~s'", [RecName]) |
				["_" || _ <- lists:seq(1, RecSize)]],
			       ","), M])
		  end, Records) ++ ["lookup(Term) -> erlang:error(badarg, [Term])."],
		";" ++ io_lib:nl()),
    Lookup2 = string:join(
		lists:map(
		  fun({Name, XMLNS, M}) ->
			  io_lib:format("lookup(~w, ~w) -> '~s'",
					[Name, XMLNS, M])
		  end, Tags) ++ ["lookup(_, _) -> undefined."],
		";" ++ io_lib:nl()),
    Modules = io_lib:format(
		"modules() -> [~s].",
		[string:join(
		   [io_lib:format(
		      "{'~s', ~w}", [M, S])
		    || {M, S} <- Mods], ",")]),
    Module = io_lib:format("-module(~s).", [ResolverMod]),
    Compile = "-compile(export_all).",
    Forms = lists:map(
	      fun(Expr) ->
		      {ok, Tokens, _} = erl_scan:string(lists:flatten(Expr)),
		      {ok, Form} = erl_parse:parse_form(Tokens),
		      Form
	      end, [Module, Compile, Modules, Lookup1, Lookup2]),
    {ok, Code} = case compile:forms(Forms, []) of
		     {ok, ResolverMod, Bin} ->
			 {ok, Bin};
		     {ok, ResolverMod, Bin, _Warnings} ->
			 {ok, Bin};
		     Error ->
			 Error
		 end,
    {module, ResolverMod} = code:load_binary(ResolverMod, "nofile", Code),
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
compile(TaggedElems0, Forms, Path, Opts) ->
    FileName = filename:basename(Path),
    ModName = list_to_atom(filename:rootname(FileName)),
    ModNameHrl = filename:rootname(FileName) ++ ".hrl",
    ModNameATD = filename:rootname(FileName) ++ ".atd",
    DirName = filename:dirname(Path),
    ErlDirName = proplists:get_value(erl_dir, Opts, DirName),
    HrlDirName = proplists:get_value(hrl_dir, Opts, DirName),
    {AttrForms, RestForms} = lists:partition(
			       fun(Form) ->
				       erl_syntax:type(Form) == attribute
			       end, Forms),
    FunForms = lists:foldl(
		 fun(F, Acc) ->
			 case erl_syntax:type(F) of
			     function ->
				 [F|Acc];
			     _ ->
				 Acc
			 end
		 end, make_builtin_codec_funs(), RestForms),
    KnownFuns = [erl_syntax_lib:analyze_function(F) || F <- FunForms],
    FunSpecs = lists:foldl(
		 fun(Form, D) ->
			 case erl_syntax_lib:analyze_attribute(Form) of
			     {spec, _} ->
				 case get_fun_spec(erl_syntax:revert(Form)) of
				     {Key, Value} ->
					 dict:store(Key, Value, D);
				     _ ->
					 D
				 end;
			     _ ->
				 D
			 end
		 end, dict:new(), AttrForms),
    TaggedElems = lists:map(
		    fun({Tag, Elem}) ->
			    {Tag, prepare_elem(Elem, KnownFuns, FunSpecs,
					       TaggedElems0, ModName, Opts)}
		    end, TaggedElems0),
    Dups = get_dups([Tag || {Tag, _} <- TaggedElems]),
    if Dups /= [] ->
            bad_spec({duplicated_elem_specs, Dups});
       true ->
            ok
    end,
    Dups1 = get_dups([{Elem#elem.name, Elem#elem.xmlns}
                      || {_, Elem} <- TaggedElems]),
    if Dups1 /= [] ->
            bad_spec({duplicated_elem_names, Dups1});
       true ->
            ok
    end,
    Types = get_types(TaggedElems, FunSpecs, Opts),
    RawAttributes = lists:flatmap(
		      fun(Form) ->
			      case erl_syntax_lib:analyze_attribute(Form) of
				  {spec, _} ->
				      [];
				  _ ->
				      erl_syntax:get_ann(Form)
			      end
		      end, lists:reverse(AttrForms)),
    PredefRecords = get_predefined_records(AttrForms),
    ASTs = lists:foldl(
	     fun({Tag, Elem}, Acc) ->
		     dict:append_list(
		       Elem#elem.module,
		       elem_to_AST(Elem, Tag, TaggedElems, Types,
				   ModName, PredefRecords, FunSpecs, Opts),
		       Acc)
	     end, dict:from_list([{ModName, []}]), TaggedElems),
    Records = make_records(Types, TaggedElems, PredefRecords, FunSpecs, Opts),
    ATDRecords = make_atd_records(Types, TaggedElems, PredefRecords,
                                  FunSpecs, Opts),
    TypeSpecs = make_typespecs(ModName, Types, Opts),
    Hdr = header(FileName),
    FunDeps = build_fun_deps(FunForms),
    case write_modules(ASTs, ModName, FunDeps, ErlDirName,
		       FileName, TaggedElems, PredefRecords) of
        ok ->
	    case write_resolver(TaggedElems, ModName, ErlDirName, FileName) of
		ok ->
		    io:format("Generating ~s~n", [ModNameHrl]),
		    file:write_file(
		      filename:join([HrlDirName, ModNameHrl]),
		      [erl_prettypr:format(Hdr),
		       RawAttributes,
		       io_lib:nl(),
		       string:join(Records, io_lib:nl() ++ io_lib:nl()),
		       io_lib:nl(),
		       io_lib:nl(),
		       TypeSpecs,
		       io_lib:nl()]),
		    io:format("Generating ~s~n", [ModNameATD]),
		    file:write_file(
		      filename:join([HrlDirName, ModNameATD]),
		      [atd_header(FileName),
		       ATDRecords,
                       io_lib:nl()]);
		Err ->
		    Err
	    end;
        Err ->
            Err
    end.

write_modules(ASTs, ModName, FunDeps, ErlDirName,
	      FileName, TaggedElems, PredefRecords) ->
    dict:fold(
      fun(_, _, {error, _} = Err) ->
	      Err;
	 (Mod, AST, ok) ->
	      write_module(ModName, Mod, AST, FunDeps, ErlDirName,
			   FileName, TaggedElems, PredefRecords)
      end, ok, ASTs).

write_module(ModName, ModName, AST, FunDeps, ErlDirName,
	     SpecFile, TaggedElems, PredefRecords) ->
    ModNameErl = atom_to_list(ModName) ++ ".erl",
    io:format("Generating ~s~n", [ModNameErl]),
    Module = erl_syntax:attribute(
               ?AST(module),
               [erl_syntax:atom(ModName)]),
    TopDecoders = make_top_decoders(TaggedElems, ModName),
    TopEncoders = make_top_encoders(TaggedElems, ModName),
    Registrar = make_registrar(ModName),
    Decoders = make_decoders(TaggedElems, PredefRecords, ModName, ModName),
    Encoders = make_encoders(TaggedElems, ModName),
    Printer = make_printer(TaggedElems, PredefRecords, ModName, ModName),
    GettersSetters = make_getters_setters(TaggedElems, PredefRecords, ModName),
    Resolver = make_resolver(TaggedElems, ModName),
    AuxFuns = make_aux_funs(),
    LocalFunForms = make_local_funs(FunDeps, TaggedElems, ModName),
    NewAST = TopDecoders ++ TopEncoders ++ Decoders ++ Encoders ++
	GettersSetters ++ Registrar ++ AuxFuns ++ LocalFunForms ++ Printer ++ Resolver ++ AST,
    Compile = erl_syntax:attribute(?AST(compile), [?AST(export_all)]),
    Hdr = header(SpecFile),
    ResultAST = erl_syntax:form_list([Hdr, Module, Compile|NewAST]),
    file:write_file(
      filename:join([ErlDirName, ModNameErl]),
      [erl_prettypr:format(ResultAST), io_lib:nl()]);
write_module(ParentMod, ModName, AST, FunDeps, ErlDirName,
	     SpecFile, TaggedElems, PredefRecords) ->
    ModNameErl = atom_to_list(ModName) ++ ".erl",
    io:format("Generating ~s~n", [ModNameErl]),
    Hdr = header(SpecFile),
    Module = erl_syntax:attribute(
               ?AST(module),
               [erl_syntax:atom(ModName)]),
    Decoders = make_decoders(TaggedElems, PredefRecords, ParentMod, ModName),
    Encoders = make_encoders(TaggedElems, ModName),
    Printer = make_printer(TaggedElems, PredefRecords, ModName, ParentMod),
    GettersSetters = make_getters_setters(TaggedElems, PredefRecords, ModName),
    Compile = erl_syntax:attribute(?AST(compile), [?AST(export_all)]),
    LocalFunForms = make_local_funs(FunDeps, TaggedElems, ModName),
    NewAST = Decoders ++ Encoders ++ GettersSetters ++ Printer ++ LocalFunForms ++ AST,
    ResultAST = erl_syntax:form_list([Hdr, Module, Compile|NewAST]),
    file:write_file(
      filename:join([ErlDirName, ModNameErl]),
      [erl_prettypr:format(ResultAST), io_lib:nl()]).

write_resolver(_TaggedElems, ParentMod, ErlDirName, SpecFile) ->
    ModName = resolver_mod(ParentMod),
    ModNameErl = atom_to_list(ModName) ++ ".erl",
    io:format("Generating ~s~n", [ModNameErl]),
    Hdr = header(SpecFile),
    Module = erl_syntax:attribute(
               ?AST(module),
               [erl_syntax:atom(ModName)]),
    Compile = erl_syntax:attribute(?AST(compile), [?AST(export_all)]),
    AST = [make_function(modules, [], [?AST([])]),
	   make_function(lookup, [?AST(_), ?AST(_)], [?AST(undefined)]),
	   make_function(lookup, [?AST(Term)], [?AST(erlang:error(badarg, [Term]))])],
    ResultAST = erl_syntax:form_list([Hdr, Module, Compile|AST]),
    file:write_file(
      filename:join([ErlDirName, ModNameErl]),
      [erl_prettypr:format(ResultAST), io_lib:nl()]).

get_fun_spec({attribute, _, spec, {MFA, Args}}) ->
    Spec = case [Range || {type, _, 'fun', [_, Range]} <- Args] of
	       [] -> {type, 0, any, []};
	       [T] -> T;
	       Ts -> {type, 0, union, Ts}
	   end,
    {MFA, Spec}.

get_predefined_records(AttrForms) ->
    lists:foldl(
      fun(F, Acc) ->
	      case erl_syntax_lib:analyze_attribute(F) of
		  {record, {RecName, RecAttrs}} ->
		      RecAttrs1 = lists:map(
				    fun({Attr, {Default, Type}}) ->
                                            {Attr, Default, Type};
				       ({Attr, Default}) ->
                                            {Attr, Default, undefined}
				    end, RecAttrs),
		      dict:store(RecName, RecAttrs1, Acc);
		  _ ->
		      Acc
	      end
      end, dict:new(), AttrForms).

make_aux_funs() ->
    case get_abstract_code_from_myself() of
        {ok, AbsCode} ->
	    lists:filter(
	      fun(T) ->
                      case catch erl_syntax_lib:analyze_function(T) of
                          {format_error, 1} -> true;
			  {io_format_error, 1} -> true;
			  {get_attr, 3} -> true;
			  {choose_top_xmlns, 3} -> true;
			  {enc_xmlns_attrs, 2} -> true;
			  {register_module, 2} -> true;
			  {unregister_module, 2} -> true;
			  {recompile_resolver, 2} -> true;
                          _ -> false
                      end
              end, AbsCode);
        error ->
            erlang:error({no_abstract_code_found, ?MODULE})
    end.

make_builtin_codec_funs() ->
    case get_abstract_code_from_myself() of
        {ok, AbsCode} ->
	    lists:filter(
	      fun(T) ->
                      case catch erl_syntax_lib:analyze_function(T) of
                          {dec_int, 3} -> true;
                          {dec_int, 1} -> true;
                          {dec_enum, 2} -> true;
                          {enc_int, 1} -> true;
                          {enc_enum, 1} -> true;
                          _ -> false
                      end
              end, AbsCode);
        error ->
            erlang:error({no_abstract_code_found, ?MODULE})
    end.

build_fun_deps(FunForms) ->
    G = digraph:new([acyclic]),
    lists:foreach(
      fun(FunForm) ->
	      Fun = erl_syntax_lib:analyze_function(FunForm),
	      digraph:add_vertex(G, Fun, FunForm),
	      erl_syntax_lib:map(
		fun(Form) ->
			try erl_syntax_lib:analyze_application(Form) of
			    {_, Arity} = SubFun when is_integer(Arity) ->
				case digraph:vertex(G, SubFun) of
				    false -> digraph:add_vertex(G, SubFun);
				    _ -> ok
				end,
				digraph:add_edge(G, Fun, SubFun);
			    _ ->
				ok
			catch _:_ ->
				ok
			end,
			Form
		end, FunForm)
      end, FunForms),
    G.

make_local_funs(FunDeps, TaggedElems, ModName) ->
    CodecFuns = lists:flatmap(
		  fun({_, #elem{attrs = Attrs, cdata = CData, module = M}})
			when M == ModName ->
			  [CData#cdata.enc, CData#cdata.dec|
			   lists:flatmap(
			     fun(#attr{enc = Enc, dec = Dec}) ->
				     [Enc,Dec]
			     end, Attrs)];
		     ({_, _}) ->
			  []
		  end, TaggedElems),
    LocalFuns = lists:usort([{Name, length(Arity)+1} || {Name, Arity} <- CodecFuns]),
    AllLocalFuns = lists:flatmap(
		     fun(LocalFun) ->
			     digraph_utils:reachable([LocalFun], FunDeps)
		     end, LocalFuns),
    lists:flatmap(
      fun(V) ->
	      case digraph:vertex(FunDeps, V) of
		  false ->
		      [];
		  {V, Form} when is_list(Form) ->
		      Form;
		  {V, Form} ->
		      case erl_syntax:is_form(Form) of
			  true -> [Form];
			  false -> []
		      end
	      end
      end, lists:usort(AllLocalFuns)).

make_records({Tags, TypesDict, RecDict}, TaggedElems, PredefRecords, FunDict, Opts) ->
    {Strings, _} =
        lists:foldl(
          fun(Tag, {Res, Seen}) ->
                  RefElem = get_elem_by_ref(Tag, TaggedElems),
                  Result = RefElem#elem.result,
                  case term_is_record(Result) of
                      true ->
                          RecName = element(1, Result),
			  case dict:is_key(RecName, PredefRecords) of
			      true ->
				  {Res, Seen};
			      false ->
				  case lists:member(RecName, Seen) of
				      false ->
					  {[record_to_string(
					      RefElem, RecDict,
					      TypesDict, FunDict, Opts)|Res],
					   [RecName|Seen]};
				      true ->
					  {Res, Seen}
				  end
			  end;
                      false ->
                          {Res, Seen}
                  end
          end, {[], []}, Tags),
    lists:reverse(Strings).

make_typespecs(_ModName, {_Tags, _TypesDict, RecDict}, Opts) ->
    case proplists:get_value(add_type_specs, Opts) of
	TypeName when is_atom(TypeName), TypeName /= undefined ->
	    case [[atom_to_string(R), "()"]
		  || {record, R} <- lists:sort(dict_keys(RecDict))] of
		[] ->
		    [];
		Records ->
		    Prefix = "-type " ++ atom_to_string(TypeName) ++ "() :: ",
		    Sep = " |" ++ io_lib:nl()
			++ lists:duplicate(length(Prefix), $ ),
		    [Prefix, string:join(Records, Sep), $.]
	    end;
	_ ->
	    []
    end.

atom_to_string(Atom) ->
    erl_syntax:atom_literal(abstract(Atom)).

-define(is_raw_type(T),
	element(1, T) == type orelse
	element(1, T) == remote_type).

record_to_string(#elem{result = Result} = Elem, RecDict, RecTypes, FunTypes, Opts) ->
    [RecName|RecLabels] = tuple_to_list(Result),
    Prefix = "-record(" ++ atom_to_string(RecName) ++ ", {",
    Sep = "," ++ io_lib:nl() ++ lists:duplicate(length(Prefix), $ ),
    Fs = lists:map(
           fun(Label) ->
                   FName = label_to_record_field(Label),
                   case get_label_type(Label, Elem, RecTypes, FunTypes, Opts) of
		       {FType, undefined, true} when ?is_raw_type(FType) ->
			   [atom_to_string(FName), " :: ",
			    erl_types:t_form_to_string(FType)];
		       {FType, undefined, false} when ?is_raw_type(FType) ->
			   [atom_to_string(FName), " :: ",
			    "undefined | ", erl_types:t_form_to_string(FType)];
		       {FType, Default, _} when ?is_raw_type(FType) ->
			   [atom_to_string(FName), " = ",
                            io_lib:fwrite("~w", [Default]),
                            " :: ", erl_types:t_form_to_string(FType)];
                       {FType, undefined, true} ->
                           FType1 = erl_types:t_subtract(
				      FType, erl_types:t_atom(undefined)),
                           [atom_to_string(FName), " :: ",
                            erl_types:t_to_string(FType1, RecDict)];
		       {FType, undefined, false} ->
			   FType1 = erl_types:t_sup(
				      FType, erl_types:t_atom(undefined)),
			   [atom_to_string(FName), " :: ",
                            erl_types:t_to_string(FType1, RecDict)];
                       {FType, Default, _} ->
                           Type = erl_types:t_sup(
                                    [erl_types:t_from_term(Default),
                                     FType]),
                           [atom_to_string(FName), " = ",
                            io_lib:fwrite("~w", [Default]),
                            " :: ",
                            erl_types:t_to_string(Type, RecDict)]
                   end
           end, RecLabels),
    RecordStr = [Prefix, string:join(Fs, Sep), "})."],
    case proplists:get_value(add_type_specs, Opts) of
	TypeName when is_atom(TypeName), TypeName /= undefined ->
	    [RecordStr, io_lib:nl(),
	     "-type ", atom_to_string(RecName), "() :: ",
	     "#", atom_to_string(RecName), "{}."];
	_ ->
	    RecordStr
    end.

header(FileName) ->
    erl_syntax:comment(
      0,
      ["% Created automatically by XML generator (fxml_gen.erl)",
       "% Source: " ++ FileName]).

make_atd_records({Tags, TypesDict, RecDict}, TaggedElems, PredefRecords, FunDict, Opts) ->
    RecTypes =
        lists:foldl(
          fun(Tag, Types) ->
                  RefElem = get_elem_by_ref(Tag, TaggedElems),
                  Result = RefElem#elem.result,
                  case term_is_record(Result) of
                      true ->
                          {RecName, Ts} =
                              record_to_atd_types(
                                RefElem, RecDict, TaggedElems,
                                TypesDict, FunDict, Opts),
                          case maps:find(RecName, Types) of
                              {ok, Ts2} ->
                                  %% TODO
                                  Ts3 = lists:zipwith(fun merge_json_types/2,
                                                      Ts, Ts2),
                                  maps:put(RecName, Ts3, Types);
                              error ->
                                  maps:put(RecName, Ts, Types)
                          end;
                      false ->
                          Types
                  end
          end, maps:new(), Tags),
    io:format("atd2 ~p~n", [RecTypes]),
    {Strings, _} =
        lists:foldl(
          fun(Tag, {Res, Seen}) ->
                  RefElem = get_elem_by_ref(Tag, TaggedElems),
                  Result = RefElem#elem.result,
                  case term_is_record(Result) of
                      true ->
                          RecName = element(1, Result),
                          case lists:member(RecName, Seen) of
                              false ->
                                  Types = maps:get(RecName, RecTypes),
                                  {[record_to_atd_string(
                                      RefElem, RecDict, TaggedElems,
                                      TypesDict, FunDict, Opts, Types,
                                      PredefRecords) | Res],
                                   [RecName|Seen]};
                              true ->
                                  {Res, Seen}
			  end;
                      false when is_atom(Result) ->
                          Label = Result,
                          Str =
                              case is_label(Label) of
                                  true ->
                                      JSONType = get_label_json_type(
                                                   Label, RefElem, TypesDict,
                                                   FunDict, TaggedElems, Opts),
                                      T =
                                          case JSONType of
                                              {option, _Type, Default}
                                              when is_atom(Default) ->
                                                  JSONType;
                                              {option, Type, _Default} ->
                                                  Type;
                                              _ ->
                                                  JSONType
                                          end,
                                      io_lib:format(
                                        "type ~s = ~s",
                                        [sanitize_for_atd(Tag),
                                         json_type_to_atd(T)]);
                                  false ->
                                      io_lib:format(
                                        "type ~s = ~s",
                                        [sanitize_for_atd(Tag),
                                         json_type_to_atd({atom, [Label]})])
                              end,
                          {[Str | Res], Seen};
                      false ->
                          {[tuple_to_atd_string(
                              RefElem, RecDict, TaggedElems,
                              TypesDict, FunDict, Opts,
                              PredefRecords, Tag) | Res],
                           Seen}
                          %{Res, Seen}
                  end
          end, {[], []}, Tags),
    lists:reverse(Strings),
    ATDRecords = [io_lib:format(
                    "  | ~s of ~s~n",
                    [to_atd_variant(R),
                     to_atd_var(R)])
		  || {record, R} <- lists:sort(dict_keys(RecDict))],
    [string:join(Strings, io_lib:nl() ++ io_lib:nl()), io_lib:nl(),
     "type xmpp_element = [", io_lib:nl(),
     "  | XmlString of string", io_lib:nl(),
     ATDRecords,
     "]"
    ].

record_to_atd_string(#elem{result = Result} = Elem, RecDict, AllElems,
                     RecTypes, FunTypes, Opts, Types, PredefRecords) ->
    [RecName | RecLabels] = tuple_to_list(Result),
    Prefix = ["type ", to_atd_var(RecName), " = {", io_lib:nl(),
              "\s\s"],
    Sep = [";", io_lib:nl(), "\s\s"],
    {_, RevRecFields} =
        lists:foldl(
          fun(Label, {K, Fs}) ->
                  case is_label(Label) of
                      true when Label /= '$_' ->
                          {K + 1, [label_to_record_field(Label) | Fs]};
                      _ ->
                          {ok, RecFields} = dict:find(RecName, PredefRecords),
                          {FName, _, _} = lists:nth(K + 1, RecFields),
                          {K + 1, [FName | Fs]}
                  end
          end, {0, []}, RecLabels),
    RecFields = lists:reverse(RevRecFields),
    case lists:zip(RecFields, Types) of
        [{_FName, JSONType}] ->
            ["type ", sanitize_for_atd(RecName), " = ",
             json_type_to_atd(JSONType)];
        RecFieldsTypes ->
            Fs = lists:flatmap(
                   fun({FName, JSONType}) ->
                           case JSONType of
                               ignore -> [];
                               {option, Type, Default} ->
                                   if
                                                %is_atom(Default) ->
                                       Default == undefined ->
                                           [io_lib:format(
                                              "?~s : ~s",
                                              [to_atd_var(FName),
                                               json_type_to_atd(JSONType)])];
                                       true ->
                                           [io_lib:format(
                                              "~~~s <ocaml default=\"~s\"> : ~s",
                                              [to_atd_var(FName),
                                               atd_value_to_string(Default),
                                               json_type_to_atd(Type)])]
                                   end;
                               Type ->
                                   UseDefault =
                                       case Type of
                                           {list, _} -> "~";
                                           binary -> "~";
                                           {binary, _, _} -> "~";
                                           _ -> ""
                                       end,
                                   [io_lib:format("~s~s : ~s",
                                                  [UseDefault,
                                                   to_atd_var(FName),
                                                   json_type_to_atd(Type)])]
                           end
                   end, RecFieldsTypes),
            case Fs of
                [] ->
                    ["type ", sanitize_for_atd(RecName), " = unit"];
                _ ->
                    RecordStr = [Prefix, string:join(Fs, Sep), io_lib:nl() ++ "}"],
                    [RecordStr, io_lib:nl()]
            end
    end.

tuple_to_atd_string(#elem{result = Result} = Elem, RecDict, AllElems,
                    RecTypes, FunTypes, Opts, PredefRecords, RecName) ->
    RecLabels = tuple_to_list(Result),
    Prefix = "type " ++ sanitize_for_atd(RecName) ++ " = (",
    Sep = " * ",
    Fs = lists:flatmap(
           fun(Label) ->
                   JSONType = get_label_json_type(
                                Label, Elem, RecTypes,
                                FunTypes, AllElems, Opts),
                   FName = label_to_record_field(Label),
                   case JSONType of
                       ignore -> [];
                       Type ->
                           [json_type_to_atd(Type)]
                   end
           end, RecLabels),
    RecordStr = [Prefix, string:join(Fs, Sep), ")"],
    RecordStr.

record_to_atd_types(#elem{result = Result} = Elem, RecDict, AllElems,
                    RecTypes, FunTypes, Opts) ->
    [RecName | RecLabels] = tuple_to_list(Result),
    {RecName,
     lists:map(
       fun(Label) ->
               case is_label(Label) of
                   true ->
                       get_label_json_type(
                         Label, Elem, RecTypes,
                         FunTypes, AllElems, Opts);
                   false ->
                       {atom, [Label]}
               end
       end, RecLabels)}.

merge_json_types(T, T) ->
    T;
merge_json_types(ignore, T) ->
    merge_json_types(T, ignore);
merge_json_types(binary, ignore) ->
    {option, binary, <<>>};
merge_json_types(jid, ignore) ->
    {option, jid, undefined};
merge_json_types({atom, _} = T, ignore) ->
    {option, T, undefined};
merge_json_types({option, _, _} = T, ignore) ->
    T;
merge_json_types({atom, A1}, {atom, A2}) ->
    {atom, A1 ++ A2};
merge_json_types({option, T1, Default}, {option, T2, Default}) ->
    {option, merge_json_types(T1, T2), Default};
merge_json_types({external, T1, _, _} = T, {external, T1, _, _}) ->
    T;
merge_json_types([T1], T2) ->
    merge_json_types(T1, T2);
merge_json_types(T1, [T2]) ->
    merge_json_types(T1, T2);
merge_json_types(T1, T2) ->
    io:format("merge ~p~n", [{T1, T2}]),
    T1.

sanitize_for_atd(Atom) when is_atom(Atom) ->
    sanitize_for_atd(atom_to_binary(Atom, utf8));
sanitize_for_atd(Str) ->
    string:replace(Str, <<"-">>, <<"_">>, all).

to_atd_var(Atom) when is_atom(Atom) ->
    to_atd_var(atom_to_binary(Atom, utf8));
to_atd_var(<<"type">>) -> <<"type_ <json name=\"type\">">>;
to_atd_var(<<"to">>) -> <<"to_ <json name=\"to\">">>;
to_atd_var(<<"end">>) -> <<"end_ <json name=\"end\">">>;
to_atd_var(<<"class">>) -> <<"class_ <json name=\"class\">">>;
to_atd_var(<<"private">>) -> <<"private_ <json name=\"private\">">>;
to_atd_var(<<"with">>) -> <<"with_ <json name=\"with\">">>;
to_atd_var(Str) ->
    sanitize_for_atd(Str).

to_atd_variant(Atom) when is_atom(Atom) ->
    to_atd_variant(atom_to_binary(Atom, utf8));
to_atd_variant(S) ->
    [string:titlecase(sanitize_for_atd(S)),
     " <json name=\"", S, "\">"].

atd_header(FileName) ->
    ["(* Created automatically by XML generator (fxml_gen.erl) *)", io_lib:nl(),
     "(* Source: ", FileName, " *)", io_lib:nl(),
     io_lib:nl(),
     "type jid = {user : string; server : string; resource : string; ",
     "~luser : string; ~lserver : string; ~lresource : string}", io_lib:nl(),
     io_lib:nl(),
     "type els = xmpp_element list", io_lib:nl(),
     io_lib:nl()].

atd_value_to_string(<<>>) -> "\\\"\\\"";
atd_value_to_string(true) -> "true";
atd_value_to_string(false) -> "false".


make_registrar(ModName) ->
    ResolverMod = resolver_mod(ModName),
    [make_function(register_module, [?AST(Mod)],
		   [?AST(register_module(Mod, '?a(ResolverMod)'))]),
     make_function(unregister_module, [?AST(Mod)],
		   [?AST(unregister_module(Mod, '?a(ResolverMod)'))])].

make_resolver(TaggedSpecs, ModName) ->
    ResolverMod = resolver_mod(ModName),
    TagNSMods = lists:foldl(
		  fun({_, #elem{xmlns = XMLNS, name = Name, module = Mod}}, Acc)
			when is_list(XMLNS) ->
			  lists:foldl(
			    fun(NS, D) ->
				    dict:store({Name, NS}, Mod, D)
			    end, Acc, XMLNS);
		     ({_, #elem{xmlns = XMLNS, name = Name, module = Mod}}, Acc) ->
			  dict:store({Name, XMLNS}, Mod, Acc)
		  end, dict:new(), TaggedSpecs),
    RecordMods = lists:foldl(
		   fun({_, #elem{result = Result, module = Mod}}, Acc) ->
			   case term_is_record(Result) of
			       true ->
				   Key = {element(1, Result), tuple_size(Result)},
				   dict:append(Key, Mod, Acc);
			       false ->
				   Acc
			   end
		   end, dict:new(), TaggedSpecs),
    NilClause1 = erl_syntax:clause([?AST(Name), ?AST(XMLNS)], none,
				   [?AST('?a(ResolverMod)':lookup(Name, XMLNS))]),
    Clauses1 = dict:fold(
		 fun({Name, NS}, Mod, Acc) ->
			 [erl_syntax:clause(
			    [?AST('?a(Name)'), ?AST('?a(NS)')],
			    none,
			    [?AST('?a(Mod)')])|Acc]
		 end, [NilClause1], TagNSMods),
    NilClause2 = erl_syntax:clause([?AST(Record)], none,
				   [?AST('?a(ResolverMod)':lookup(Record))]),
    Clauses2 = dict:fold(
		 fun({RecName, RecSize}, [Mod|_], Acc) ->
			 [erl_syntax:clause(
			    [erl_syntax:tuple(
			       [erl_syntax:atom(RecName)|
				[?AST(_) || _ <- lists:seq(1, RecSize-1)]])],
			    none,
			    [erl_syntax:atom(Mod)])|Acc]
		 end, [NilClause2], RecordMods),
    [erl_syntax:function(?AST(get_mod), Clauses1),
     erl_syntax:function(?AST(get_mod), Clauses2)].

make_top_decoders(TaggedSpecs, ModName) ->
    C0 = ?AST(XMLNS = get_attr(<<"xmlns">>, Attrs, TopXMLNS)),
    C1 = erl_syntax:case_expr(
	   ?AST(get_mod(Name, XMLNS)),
	   [erl_syntax:clause(
	      [?AST(undefined)],
	      ?AST(XMLNS == <<>>),
	      [?AST(erlang:error(
		      {'?a(ModName)', {missing_tag_xmlns, Name}}))]),
	    erl_syntax:clause(
	      [?AST(undefined)],
	      none,
	      [?AST(erlang:error(
			    {'?a(ModName)', {unknown_tag, Name, XMLNS}}))]),
	    erl_syntax:clause(
	      [?AST(Mod)],
	      none,
	      [?AST(Mod:do_decode(Name, XMLNS, El, Opts))])]),
    [make_function("decode", [?AST(El)], [?AST(decode(El, <<>>, []))]),
     make_function("decode", [?AST(El), ?AST(Opts)], [?AST(decode(El, <<>>, Opts))]),
     make_function(
       "decode",
       [?AST({xmlel, Name, Attrs, _} = El), ?AST(TopXMLNS), ?AST(Opts)],
       [C0, C1])] ++
        make_top_decoders_json(TaggedSpecs, ModName).

make_top_decoders_json(TaggedSpecs, ModName) ->
    ResolverMod = resolver_mod(ModName),
    TagNSMods = lists:foldl(
		  fun({_, #elem{xmlns = XMLNS, name = Name, module = Mod}}, Acc)
			when is_list(XMLNS) ->
			  lists:foldl(
			    fun(NS, D) ->
				    dict:store({Name, NS}, Mod, D)
			    end, Acc, XMLNS);
		     ({_, #elem{xmlns = XMLNS, name = Name, module = Mod}}, Acc) ->
			  dict:store({Name, XMLNS}, Mod, Acc)
		  end, dict:new(), TaggedSpecs),
    RecordMods = lists:foldl(
		   fun({_, #elem{result = Result, module = Mod}}, Acc) ->
			   case term_is_record(Result) of
			       true ->
				   Key = {element(1, Result), tuple_size(Result)},
				   dict:append(Key, Mod, Acc);
			       false ->
				   Acc
			   end
		   end, dict:new(), TaggedSpecs),
    Clauses = dict:fold(
                fun({RecName, _RecSize}, [Mod | _], Acc) ->
                        BRecName = atom_to_binary(RecName, utf8),
                        [erl_syntax:clause(
                           [?AST(['?a(BRecName)', _] = El)],
                           none,
                           [?AST('?a(Mod)':do_decode_json(El))]) | Acc]
                end, [], RecordMods),
    [erl_syntax:function(?AST(decode_json), Clauses)].

make_decoders(TaggedSpecs1, PredefRecords, ParentMod, ModName) ->
    TaggedSpecs = lists:flatmap(
		    fun({Tag, #elem{xmlns = XMLNSs, module = Mod} = E})
			  when Mod == ModName ->
			    if is_list(XMLNSs) ->
				    [{Tag, E#elem{xmlns = XMLNS}} || XMLNS <- XMLNSs];
			       true ->
				    [{Tag, E}]
			    end;
		       (_) ->
			    []
		    end, TaggedSpecs1),
    NilClause = [erl_syntax:clause(
		   [?AST(Name), ?AST(<<>>), ?AST(_), ?AST(_)],
		   none,
		   [?AST(erlang:error(
			   {'?a(ParentMod)', {missing_tag_xmlns, Name}}))]),
		 erl_syntax:clause(
		   [?AST(Name), ?AST(XMLNS), ?AST(_), ?AST(_)],
		   none,
		   [?AST(erlang:error(
			    {'?a(ParentMod)', {unknown_tag, Name, XMLNS}}))])],
    Clauses = lists:map(
		fun({Tag, #elem{xmlns = XMLNS, name = Name}}) ->
			erl_syntax:clause(
			  [?AST('?a(Name)'), ?AST('?a(XMLNS)'), ?AST(El), ?AST(Opts)],
			  none,
			  [make_function_call(
			     make_dec_fun_name([Tag]),
			     [abstract(XMLNS),
			      ?AST(Opts),
			      ?AST(El)])])
		end, TaggedSpecs),
    [erl_syntax:function(erl_syntax:atom(do_decode), Clauses ++ NilClause),
     make_function(tags, [],
		   [erl_syntax:list(
		      lists:map(
			fun({_, #elem{name = Name, xmlns = NS}}) ->
				?AST({'?a(Name)', '?a(NS)'})
			end, TaggedSpecs))])] ++
        make_decoders_json(TaggedSpecs1, PredefRecords, ParentMod, ModName).

make_decoders_json(TaggedSpecs1, PredefRecords, ParentMod, ModName) ->
    TaggedSpecs =
        dict:to_list(
          lists:foldl(
            fun({Tag, #elem{module = Mod} = E}, Acc)
               when Mod == ModName ->
                    Result = E#elem.result,
                    case term_is_record(Result) of
                        true ->
                            RecName = element(1, Result),
                            Append =
                                case dict:find(RecName, Acc) of
                                    {ok, [{_, #elem{result = Result}}]} ->
                                        case lists:keyfind(
                                               <<"xmlns">>, #attr.name,
                                               E#elem.attrs) of
                                            #attr{} ->
                                                true;
                                            _ ->
                                                false
                                        end;
                                    _ ->
                                        true
                                end,
                            case Append of
                                false ->
                                    Acc;
                                true ->
                                    dict:append(RecName, {Tag, E}, Acc)
                            end;
                        false ->
                            Acc
                    end;
               (_, Acc) ->
                    Acc
            end, dict:new(), TaggedSpecs1)),
    NilClause = [erl_syntax:clause(
		   [?AST([Name, _])],
		   none,
		   [?AST(erlang:error(
			    {'?a(ParentMod)', {unknown_tag, Name}}))])],
    Clauses =
        lists:map(
          fun({RecName, [{Tag, #elem{}}]}) ->
                  BRecName = atom_to_binary(RecName, utf8),
                  erl_syntax:clause(
                    [?AST(['?a(BRecName)', El])],
                    none,
                    [make_function_call(
                       make_dec_fun_name([Tag, json]),
                       [?AST(El)])]);
             ({RecName, [{_, El1} | _] = TEs}) ->
                  BRecName = atom_to_binary(RecName, utf8),
		  XMLNSLabel = case lists:keyfind(<<"xmlns">>, #attr.name,
                                                  El1#elem.attrs) of
				   #attr{label = L, name = N} ->
				       prepare_label(L, N);
				   _ ->
				       undefined
			       end,
                  TagField =
                      case size(El1#elem.result) of
                          2 ->
                              ?AST(El);
                          _ ->
                              case XMLNSLabel of
                                  undefined ->
                                      {ok, RecFields} =
                                          dict:find(RecName, PredefRecords),
                                      {FName, _, _} = lists:nth(1, RecFields),
                                      FBName = atom_to_binary(FName, utf8),
                                      ?AST(proplists:get_value(
                                             '?a(FBName)',
                                             element(1, El)));
                                  _ ->
                                      BXMLNSLabel = atom_to_binary(
                                                      label_to_record_field(
                                                        XMLNSLabel), utf8),
                                      ?AST(proplists:get_value(
                                             '?a(BXMLNSLabel)',
                                             element(1, El)))
                              end
                      end,
                  Cs = lists:map(
                         fun({Tag, El}) when XMLNSLabel == undefined ->
                                 Atom = element(2, El#elem.result),
                                 false = is_label(Atom),
                                 BAtom = atom_to_binary(Atom, utf8),
                                 erl_syntax:clause(
                                   [?AST('?a(BAtom)')],
                                   none,
                                   [make_function_call(
                                      make_dec_fun_name([Tag, json]),
                                      [?AST(El)])]);
                            ({Tag, El}) ->
                                 XMLNS = El#elem.xmlns,
                                 erl_syntax:clause(
                                   [?AST('?a(XMLNS)')],
                                   none,
                                   [make_function_call(
                                      make_dec_fun_name([Tag, json]),
                                      [?AST(El)])])
                         end, TEs),
                  erl_syntax:clause(
                    [?AST(['?a(BRecName)', El])],
                    none,
                    [erl_syntax:case_expr(TagField, Cs)])
          end, TaggedSpecs),
    [erl_syntax:function(erl_syntax:atom(do_decode_json),
                         Clauses ++ NilClause)].

make_top_encoders(_TaggedSpecs, _ModName) ->
    Clause1 = erl_syntax:clause(
		[?AST({xmlel, _, _, _} = El), ?AST(_)],
		none,
		[?AST(El)]),
    Clause2 = erl_syntax:clause(
		[?AST({xmlcdata, _} = CData), ?AST(_)],
		none,
		[?AST(CData)]),
    GetNameCase = [?AST(Mod = get_mod(El)),
		   ?AST(Mod:do_get_name(El))],
    GetNSCase = [?AST(Mod = get_mod(El)),
		 ?AST(Mod:do_get_ns(El))],
    KnownTagCase = [?AST(XMLNS = get_attr(<<"xmlns">>, Attrs, TopXMLNS)),
		    ?AST(get_mod(Name, XMLNS) /= undefined)],
    Clause3 = erl_syntax:clause(
		[?AST(El), ?AST(TopXMLNS)],
		none,
		[?AST(Mod = get_mod(El)),
		 ?AST(Mod:do_encode(El, TopXMLNS))]),
    JClause1 = erl_syntax:clause(
                 [?AST({xmlel, _, _, _} = El)],
                 none,
                 [?AST(try decode(El) of
                           JSON -> encode_json(JSON)
                       catch
                           _:_ ->
                               [<<"XmlString">>, fxml:element_to_binary(El)]
                       end)]),
    JClause3 = erl_syntax:clause(
                 [?AST(El)],
                 none,
                 [?AST(Mod = get_mod(El)),
                  ?AST(Mod:do_encode_json(El))]),
    [make_function(encode, [?AST(El)], [?AST(encode(El, <<>>))]),
     erl_syntax:function(?AST(encode), [Clause1, Clause2, Clause3]),
     erl_syntax:function(?AST(encode_json), [JClause1, JClause3]),
     make_function(get_name, [?AST(El)], GetNameCase),
     make_function(get_ns, [?AST(El)], GetNSCase),
     make_function(is_known_tag,
		   [?AST({xmlel, Name, Attrs, _}), ?AST(TopXMLNS)],
		   KnownTagCase),
     make_function(get_els, [?AST(Term)],
		   [?AST(Mod = get_mod(Term)),
		    ?AST(Mod:get_els(Term))]),
     make_function(set_els, [?AST(Term), ?AST(Els)],
		   [?AST(Mod = get_mod(Term)),
		    ?AST(Mod:set_els(Term, Els))])].

make_encoders(TaggedSpecs, ModName) ->
    {RecNames, ResNames} =
	lists:foldl(
	  fun({Tag, #elem{result = Result, module = Mod}}, {RecAcc, ResAcc})
		when Mod == ModName ->
		  try
		      [H|_]= tuple_to_list(Result),
		      true = is_atom(H),
		      false = is_label(H),
		      {dict:append(H, Tag, RecAcc),
		       dict:append(H, Result, ResAcc)}
		  catch _:_ ->
			  {RecAcc, ResAcc}
		  end;
	     (_, Acc) ->
		  Acc
	  end, {dict:new(), dict:new()}, TaggedSpecs),
    {EncClauses, EncJSONClauses, NSClauses, TagClauses, _} =
        lists:foldl(
          fun({Tag, #elem{name = Name, xmlns = XMLNS, module = Mod,
			  result = Result, attrs = Attrs}},
	      {EncAcc, EncJSONAcc, NSAcc, TagAcc, Seen}) ->
		  XMLNSLabel = case lists:keyfind(<<"xmlns">>, #attr.name, Attrs) of
				   #attr{label = L, name = N} ->
				       prepare_label(L, N);
				   _ ->
				       undefined
			       end,
                  Var = label_to_var(prepare_label(undefined, Name)),
		  HasXMLNSAttr = XMLNSLabel /= undefined,
		  EncodeResult = if HasXMLNSAttr and not is_list(XMLNS) ->
					 labels_to_underscores(
					   Result, [], [{XMLNSLabel, XMLNS}]);
				    true ->
					 labels_to_underscores(Result)
				 end,
		  EncodeResultAux = labels_to_underscores(
				      Result, [], [{XMLNSLabel, <<>>}]),
		  NSResult = labels_to_underscores(Result, [XMLNSLabel]),
		  TagResult = labels_to_underscores(Result),
                  try
                      [H|_]= tuple_to_list(Result),
                      true = is_atom(H),
                      false = is_label(H),
                      Tags = dict:fetch(H, RecNames),
		      OtherResults = dict:fetch(H, ResNames) -- [Result],
		      true = lists:member(Tag, Tags),
		      IsDuplicated = lists:member(Result, OtherResults),
		      AlreadySeen = lists:member(Result, Seen),
                      {if AlreadySeen ->
			       EncAcc;
			  true ->
			       Call = [make_function_call(
					 make_enc_fun_name(ModName, Mod, [Tag]),
					 [Var, ?AST(TopXMLNS)])],
			       [erl_syntax:clause(
				  [?AST('?EncodeResult' = '?Var'), ?AST(TopXMLNS)],
				  none, Call)] ++
				   if HasXMLNSAttr and not is_list(XMLNS) ->
					   [erl_syntax:clause(
					      [?AST('?EncodeResultAux' = '?Var'),
					       ?AST(TopXMLNS = '?a(XMLNS)')],
					      none, Call)];
				      true ->
					   []
				   end ++ EncAcc
		       end,
		       if AlreadySeen ->
			       EncJSONAcc;
			  true ->
			       Call = make_function_call(
                                        make_enc_fun_name(ModName, Mod,
                                                          [Tag, json]),
                                        [Var]),
                               BRecName = atom_to_binary(H, utf8),
                               ResJSON = ?AST(['?a(BRecName)', '?Call']),
			       [erl_syntax:clause(
				  [?AST('?EncodeResult' = '?Var')],
				  none,
                                  [ResJSON])] ++ EncJSONAcc
		       end,
		       if IsDuplicated ->
			       NSAcc;
			  HasXMLNSAttr ->
			       [erl_syntax:clause(
				  [NSResult], none,
				  [label_to_var(XMLNSLabel)])|NSAcc];
			  is_list(XMLNS) ->
			       [erl_syntax:clause(
				  [NSResult], none,
				  [abstract(hd(XMLNS))])|NSAcc];
			  true ->
			       [erl_syntax:clause(
				  [NSResult], none,
				  [abstract(XMLNS)])|NSAcc]
		       end,
		       if AlreadySeen ->
			       TagAcc;
			  true ->
			       [erl_syntax:clause(
				  [TagResult], none,
				  [abstract(Name)])|TagAcc]
		       end,
		       [Result|Seen]}
                  catch _:_ ->
                          {EncAcc, EncJSONAcc, NSAcc, TagAcc, Seen}
                  end
          end, {[], [], [], [], []}, TaggedSpecs),
    if EncClauses /= [] ->
	    [erl_syntax:function(?AST(do_encode), EncClauses),
             erl_syntax:function(?AST(do_encode_json), EncJSONClauses)];
       true -> []
    end ++
    if TagClauses /= [] ->
	    [erl_syntax:function(?AST(do_get_name), lists:usort(TagClauses))];
       true -> []
    end ++
    if NSClauses /= [] ->
	    [erl_syntax:function(?AST(do_get_ns), lists:usort(NSClauses))];
       true -> []
    end.

make_printer(TaggedSpecs, PredefRecords, ModName, ParentMod) ->
    PassClause1 =
	if ModName == ParentMod ->
		[erl_syntax:clause(
		   [?AST(xmlel), ?AST(3)],
		   none,
		   [?AST([name, attrs, children])]),
		 erl_syntax:clause(
		   [?AST(Name), ?AST(Arity)],
		   none,
		   [erl_syntax:try_expr(
		      [?AST(get_mod(
			      erlang:make_tuple(Arity+1, undefined, [{1, Name}])))],
		      [erl_syntax:clause(
			 [?AST(Mod)],
			 none,
			 [?AST(Mod:pp(Name, Arity))])],
		      [erl_syntax:clause(
			 [?AST(error:badarg)], none, [?AST(no)])])])];
	   true ->
		[erl_syntax:clause([?AST(_), ?AST(_)], none, [?AST(no)])]
	end,
    %% Exclude tags with duplicated results
    RecNames = lists:foldl(
                 fun({Tag, #elem{result = Result, module = Mod}}, Acc)
		       when Mod == ModName ->
                         try
                             [H|_]= tuple_to_list(Result),
                             true = is_atom(H),
                             false = is_label(H),
                             dict:append(H, Tag, Acc)
                         catch _:_ ->
                                 Acc
                         end;
		    (_, Acc) ->
			 Acc
                 end, dict:new(), TaggedSpecs),
    {Clauses1, Records} =
        lists:foldl(
          fun({Tag, #elem{result = Result}}, {Acc1, Acc2}) ->
                  try
                      [H|T]= tuple_to_list(Result),
                      true = is_atom(H),
                      false = is_label(H),
                      [Tag|_] = dict:fetch(H, RecNames),
		      Fields = case dict:find(H, PredefRecords) of
				   {ok, Fs} ->
				       [FName || {FName, _, _} <- Fs];
				   error ->
				       [label_to_record_field(F) || F <- T]
			       end,
                      {[erl_syntax:clause(
			  [erl_syntax:atom(H), abstract(length(T))],
			  none,
			  [erl_syntax:list(
			     [erl_syntax:atom(F) || F <- Fields])])
			|Acc1],
		       [{H, length(T)}|Acc2]}
		  catch _:_ ->
                          {Acc1, Acc2}
                  end
          end, {[], []}, TaggedSpecs),
    [erl_syntax:function(?AST(pp), Clauses1 ++ PassClause1),
     make_function(records, [],
		   [erl_syntax:list(
		      lists:map(
			fun({RecName, RecSize}) ->
				?AST({'?a(RecName)', '?a(RecSize)'})
			end, Records))])].

make_getters_setters(TaggedSpecs, PredefRecords, ModName) ->
    RecNames = lists:foldl(
                 fun({Tag, #elem{result = Result, module = Mod}}, Acc)
		       when Mod == ModName ->
                         try
                             [H|_]= tuple_to_list(Result),
                             true = is_atom(H),
                             false = is_label(H),
                             dict:append(H, Tag, Acc)
                         catch _:_ ->
                                 Acc
                         end;
		    (_, Acc) ->
			 Acc
                 end, dict:new(), TaggedSpecs),
    {Getters, Setters} =
        lists:foldl(
          fun({Tag, #elem{result = Result}}, {Acc1, Acc2}) ->
                  try
                      [H|T]= tuple_to_list(Result),
                      true = is_atom(H),
                      false = is_label(H),
                      [Tag|_] = dict:fetch(H, RecNames),
		      Fields = case dict:find(H, PredefRecords) of
				   {ok, Fs} ->
				       [FName || {FName, _, _} <- Fs];
				   error ->
				       [label_to_record_field(F) || F <- T]
			       end,
		      case lists:member(sub_els, Fields) of
			  false ->
			      {Acc1, Acc2};
			  true ->
			      {[erl_syntax:clause(
				  [record_fields_to_vars(H, Fields)],
				  none,
				  [?AST(_sub_els)])|Acc1],
			       [erl_syntax:clause(
				  [record_fields_to_vars(H, Fields, [{sub_els, ?AST(_)}]),
				   ?AST(_sub_els)],
				  none,
				  [record_fields_to_vars(
				     H, Fields, [{sub_els, ?AST(_sub_els)}])])|Acc2]}
		      end
		  catch _:_ ->
			 {Acc1, Acc2}
                  end
          end, {[], []}, TaggedSpecs),
    case {Getters, Setters} of
	{[], []} ->
	    [];
	_ ->
	    [erl_syntax:function(?AST(get_els), Getters),
	     erl_syntax:function(?AST(set_els), Setters)]
    end.

elem_to_AST(#elem{name = Name, xmlns = XMLNS, cdata = CData,
                  result = Result, attrs = Attrs, refs = _Refs} = Elem,
            Tag, AllElems, Types, ModName, PredefRecords, FunSpecs, Opts) ->
    AttrAST = lists:flatmap(
                fun(#attr{name = AttrName,
                          required = Required,
                          dec = AttrDecF,
                          enc = AttrEncF,
                          default = AttrDefault}) ->
                        make_decoding_MFA([AttrName,attr,Tag],
                                          Name, XMLNS, AttrName,
                                          Required, AttrDefault,
                                          AttrDecF, Types, ModName) ++
                            make_encoding_MFA([AttrName,attr,Tag],
                                              AttrName, Required, AttrDefault,
                                              AttrEncF)
                end, Attrs),
    #cdata{label = CDataLabl,
           required = CDataRequired,
           dec = CDataDecF,
           enc = CDataEncF,
           default = CDataDefault} = CData,
    CDataAST =
        case have_label(Result, CDataLabl) of
            true ->
                make_decoding_MFA([cdata,Tag], Name, XMLNS, <<>>,
                                  CDataRequired, CDataDefault,
                                  CDataDecF, Types, ModName) ++
                    make_encoding_MFA([cdata,Tag], <<>>,
                                      CDataRequired, CDataDefault,
                                      CDataEncF);
            false ->
                []
        end,
    DecAST = make_elem_dec_fun(Elem, Tag, AllElems, Types,
			       ModName, PredefRecords, Opts),
    DecJsonAST = make_elem_dec_fun_json(Elem, Tag, AllElems, Types,
                                        ModName, FunSpecs, PredefRecords, Opts),
    EncAST = make_elem_enc_fun(Elem, Tag, AllElems, ModName),
    EncJsonAST = make_elem_enc_fun_json(Elem, Tag, AllElems, ModName,
                                        Types, FunSpecs, PredefRecords, Opts),
    DecAST ++ DecJsonAST ++ EncAST ++ EncJsonAST ++ AttrAST ++ CDataAST.

%% Replace in `Term' every label found in `Labels'
%% with the corresponding value.
subst_labels(Term) ->
    subst_labels(Term, undefined).

subst_labels(Term, PredefRecords) ->
    case have_label(Term, '$_') of
	true when PredefRecords /= undefined ->
	    try
		true = term_is_record(Term),
		[H|Elems] = erl_syntax:tuple_elements(abstract(Term)),
		RecName = erl_syntax:atom_value(H),
		RecFields = dict:fetch(RecName, PredefRecords),
		Vars = lists:map(
			 fun({T, {_, Default, _}}) ->
				 AbsDefault = if Default == none ->
						      ?AST(undefined);
						 true ->
						      Default
					      end,
				 try
				     Label = erl_syntax:atom_value(T),
				     true = is_label(Label),
				     case Label of
					 '$_' -> AbsDefault;
					 _ -> label_to_var(Label)
				     end
				 catch _:_ ->
					 T
				 end
			 end, lists:zip(Elems, RecFields)),
		erl_syntax:tuple([H|Vars])
	    catch error:{badmatch, false} ->
		    bad_spec({underscore_label_outside_record_tuple, Term});
		  _:_ ->
		    bad_spec({no_predefined_record_found, Term})
	    end;
	_ ->
	    erl_syntax_lib:map(
	      fun(T) ->
		      try
			  Label = erl_syntax:atom_value(T),
			  true = is_label(Label),
			  label_to_var(Label)
		      catch _:_ ->
			      T
		      end
	      end, abstract(Term))
    end.

get_elem_by_ref(RefName, TaggedElems) ->
    {_, Elem} = lists:keyfind(RefName, 1, TaggedElems),
    Elem.

get_spec_by_label('$_els', _Elem) ->
    sub_els;
get_spec_by_label('$_', _Elem) ->
    '_';
get_spec_by_label(Label, Elem) ->
    [Spec|T] = lists:flatmap(
                 fun(#cdata{label = L} = CData) when Label == L ->
                         [CData];
                    (#attr{label = L, name = N} = Attr) ->
                         case prepare_label(L, N) of
                             Label ->
                                 [Attr];
                             _ ->
                                 []
                         end;
                    (#ref{label = L, name = N} = Ref) ->
                         case prepare_label(L, N) of
                             Label ->
                                 [Ref];
                             _ ->
                                 []
                         end;
                    (_) ->
                         []
                 end, [Elem#elem.cdata|Elem#elem.attrs ++ Elem#elem.refs]),
    if is_record(Spec, ref) ->
            [Spec|T];
       true ->
            Spec
    end.

group_refs(Refs) ->
    dict:to_list(
      lists:foldl(
        fun(#ref{name = Name, label = Label} = Ref, G) ->
                L = prepare_label(Label, Name),
                dict:append_list(L, [Ref], G)
        end, dict:new(), Refs)).

make_elem_dec_fun(#elem{name = Name, result = Result, refs = Refs, module = Mod,
                        cdata = CData, attrs = Attrs, xmlns = XMLNS,
			ignore_els = IgnoreEls},
                  Tag, AllElems, Types, ModName, PredefRecords, Opts) ->
    FunName = make_dec_fun_name([Tag]),
    ResultWithVars = subst_labels(Result, PredefRecords),
    AttrVars = lists:map(
                 fun(#attr{name = AttrName, label = AttrLabel}) ->
                         label_to_var(prepare_label(AttrLabel, AttrName))
                 end, Attrs),
    HaveCData = have_label(Result, CData#cdata.label),
    CDataVars = if HaveCData ->
                        [label_to_var(CData#cdata.label)];
                   true ->
                        []
                end,
    SubElVars = case have_label(Result, '$_els') of
                    true ->
                        [label_to_var('$_els')];
                    false ->
                        []
                end,
    ElemVars = lists:map(
                 fun({Label, _}) ->
                         label_to_var(Label)
                 end, group_refs(Refs)),
    AttrMatch =
        if AttrVars /= [] ->
                AttrPattern = tuple_or_single_var(AttrVars),
                AttrCall = make_function_call(
                             FunName ++ "_attrs",
                             [?AST(__TopXMLNS),
			      ?AST(_attrs)|
                              lists:map(
                                fun(_) ->
                                        ?AST(undefined)
                                end, Attrs)]),
                [erl_syntax:match_expr(AttrPattern, AttrCall)];
           true ->
                []
        end,
    ElCDataMatch =
        case CDataVars ++ ElemVars ++ SubElVars of
            [] ->
                [];
            _ ->
                [erl_syntax:match_expr(
                   tuple_or_single_var(CDataVars ++ ElemVars ++ SubElVars),
                   make_function_call(
                     FunName ++ "_els",
                     [?AST(__TopXMLNS),
		      ?AST(__Opts),
		      ?AST(_els)|
                      lists:flatmap(
                        fun({_, [#ref{min = 0, max = 1, default = Def}|_]}) ->
                                [abstract(Def)];
                           ({_, [#ref{min = 1, max = 1}|_]}) ->
                                [?AST(error)];
                           (#cdata{}) when HaveCData ->
                                [?AST(<<>>)];
                           (#cdata{}) ->
                                [];
                           (_) ->
                                [?AST([])]
                        end, [CData|group_refs(Refs)] ++ SubElVars)]))]
        end,
    [make_function(
       FunName,
       [?AST(__TopXMLNS),
	?AST(__Opts),
	?AST({xmlel, '?a(Name)', _attrs, _els})],
       ElCDataMatch ++ AttrMatch ++ [ResultWithVars])]
        ++ make_els_dec_fun(Mod, FunName ++ "_els", CData, HaveCData, SubElVars,
                            Refs, Tag, XMLNS, AllElems,
                            Result, Types, ModName, IgnoreEls, Opts)
        ++ make_attrs_dec_fun(FunName ++ "_attrs", Attrs, Tag).

make_els_dec_clause(ParentMod, ModName, FunName, CDataVars, Refs, _TopXMLNS,
		    AllElems, Result, {_SortedTags, Types, _RecDict}, _Opts) ->
    SubElVars = case have_label(Result, '$_els') of
                    true ->
                        [label_to_var('$_els')];
                    false ->
                        []
                end,
    lists:map(
      fun(#ref{name = RefName, label = RefLabel}) ->
              Label = prepare_label(RefLabel, RefName),
              Var = label_to_var(Label),
              RefElem = get_elem_by_ref(RefName, AllElems),
	      RefMod = RefElem#elem.module,
              XMLNSs = lists:flatten([RefElem#elem.xmlns]),
	      ElemVars = lists:map(
			   fun({Labl, _}) ->
				   label_to_var(Labl)
			   end, group_refs(Refs)),
              NewElemVars =
		  fun(NS) ->
			  lists:map(
			    fun({L, [#ref{min = Min, max = 1}|_]})
				  when L == Label ->
				    Call = make_function_call(
					     make_dec_fun_name(ModName, RefMod, [RefName]),
					     [NS,
					      ?AST(__Opts),
					      ?AST(_el)]),
				    if Min == 0 ->
					    Call;
				       Min == 1 ->
					    ?AST({value, '?Call'})
				    end;
			       ({L, [#ref{default = Def}]}) when L == Label ->
				    RefType = dict:fetch(RefName, Types),
				    case is_subtype(Def, RefType) of
					true ->
					    erl_syntax:case_expr(
					      make_function_call(
						make_dec_fun_name(ModName, RefMod, [RefName]),
						[NS,
						 ?AST(__Opts),
						 ?AST(_el)]),
					      [erl_syntax:clause(
						 [abstract(Def)], none, [Var]),
					       erl_syntax:clause(
						 [?AST(_new_el)],
						 none,
						 [?AST([_new_el | '?Var'])])]);
					false ->
					    erl_syntax:list(
					      [make_function_call(
						 make_dec_fun_name(ModName, RefMod, [RefName]),
						 [NS,
						  ?AST(__Opts),
						  ?AST(_el)])],
					      Var)
				    end;
			       ({L, _}) ->
				    label_to_var(L)
			    end, group_refs(Refs))
		  end,
              erl_syntax:clause(
                [?AST(__TopXMLNS),
		 ?AST(__Opts),
		 ?AST([{xmlel, '?a(RefElem#elem.name)', _attrs, _} = _el | _els])|
                 CDataVars ++ ElemVars ++ SubElVars],
                none,
		[erl_syntax:case_expr(
		   ?AST('?a(ParentMod)':get_attr(<<"xmlns">>, _attrs, __TopXMLNS)),
		   lists:map(
		     fun(NS) ->
			     erl_syntax:clause(
			       [abstract(NS)],
			       none,
			       [make_function_call(
				  FunName,
				  [?AST(__TopXMLNS),
				   ?AST(__Opts),
				   ?AST(_els)|CDataVars ++ NewElemVars(abstract(NS))
				   ++ SubElVars])])
		     end, XMLNSs)
		   ++
		       [erl_syntax:clause(
			  [?AST(_)], none,
			  [make_function_call(
			     FunName,
			     [?AST(__TopXMLNS),
			      ?AST(__Opts),
			      ?AST(_els)|CDataVars ++ ElemVars
			      ++ case SubElVars of
				     [] -> [];
				     _ -> [erl_syntax:list(
					     [?AST(_el)],
					     label_to_var('$_els'))]
				 end])])])])
      end, Refs).

make_els_dec_fun(_, _FunName, _CData, false, [], [], _Tag,
                 _TopXMLNS, _AllElems, _Result, _Types, _ModName,
		 _IgnoreEls, _Opts) ->
    [];
make_els_dec_fun(Mod, FunName, CData, HaveCData, SubElVars, Refs, Tag,
                 TopXMLNS, AllElems, Result, Types, ModName, IgnoreEls, Opts) ->
    CDataVars = if HaveCData ->
                        [label_to_var(CData#cdata.label)];
                   true ->
                        []
                end,
    ElemVars = lists:map(
                 fun({Label, _}) ->
                         label_to_var(Label)
                 end, group_refs(Refs)),
    ResultCData = if HaveCData ->
                          [erl_syntax:binary(
                             [erl_syntax:binary_field(
                                hd(CDataVars),
                                [?AST(binary)]),
                              erl_syntax:binary_field(
                                ?AST(_data),
                                [?AST(binary)])])];
                     true ->
                          []
                  end,
    CDataClause = if HaveCData ->
                          [erl_syntax:clause(
                             [?AST(__TopXMLNS),
			      ?AST(__Opts),
			      erl_syntax:list(
                                [?AST({xmlcdata, _data})],
                                ?AST(_els))
                              |CDataVars ++ ElemVars ++ SubElVars],
                             none,
                             [make_function_call(
                                FunName,
                                [?AST(__TopXMLNS),
				 ?AST(__Opts),
				 ?AST(_els)|
                                 ResultCData ++ ElemVars ++ SubElVars])])];
                     true ->
                          []
                  end,
    ElemClauses = make_els_dec_clause(ModName, Mod, FunName, CDataVars,
                                      Refs, TopXMLNS, AllElems, Result,
                                      Types, Opts),
    ResultElems = lists:map(
                    fun({L, [#ref{min = 0, max = 1}|_]}) ->
                            label_to_var(L);
                       ({L, [#ref{min = 1, max = 1, name = RefName}]}) ->
                            RefElem = get_elem_by_ref(RefName, AllElems),
                            L1 = list_to_atom(atom_to_list(L) ++ "1"),
                            erl_syntax:case_expr(
                              label_to_var(L),
                              [erl_syntax:clause(
                                 [?AST(error)],
                                 none,
                                 [?AST(erlang:error(
					 {'?a(ModName)',
					  {missing_tag,
					   '?a(RefElem#elem.name)',
					   __TopXMLNS}}))]),
                               erl_syntax:clause(
                                 [erl_syntax:tuple(
                                    [?AST(value),
                                     label_to_var(L1)])],
                                 none,
                                 [label_to_var(L1)])]);
                       ({L, [#ref{min = 0, max = infinity}|_]}) ->
                            make_function_call(
                              {lists, reverse}, [label_to_var(L)])
                    end, group_refs(Refs)),
    CDataCall = if HaveCData ->
                        [make_function_call(
                           make_dec_fun_name([cdata,Tag]),
			   [?AST(__TopXMLNS)|CDataVars])];
                   true ->
                        []
                end,
    SubElResult = case have_label(Result, '$_els') of
                      true ->
                          [make_function_call(
                             {lists, reverse},
                             [label_to_var('$_els')])];
                      false ->
                          []
                  end,
    NilClause = erl_syntax:clause(
                  [?AST(__TopXMLNS),
		   ?AST(__Opts),
		   ?AST([])|
                   CDataVars ++
                       lists:map(
                         fun({L, _}) ->
                                 label_to_var(L)
                         end, group_refs(Refs)) ++ SubElVars],
                  none,
                  [tuple_or_single_var(
                     CDataCall ++ ResultElems ++ SubElResult)]),
    SubElPattern = [?AST(__TopXMLNS),
		    ?AST(__Opts),
		    erl_syntax:list(
                      [?AST({xmlel, _name, _attrs, _} = _el)],
                      ?AST(_els))
                    |CDataVars ++ ElemVars ++ SubElVars],
    SubElClause =
        case have_label(Result, '$_els') of
	    true when IgnoreEls ->
		SubElBody = make_function_call(
			      FunName,
			      [?AST(__TopXMLNS),
			       ?AST(__Opts),
			       ?AST(_els)|CDataVars ++ ElemVars] ++
				  [erl_syntax:list(
				     [?AST(_el)],
				     label_to_var('$_els'))]),
		[erl_syntax:clause(SubElPattern, none, [SubElBody])];
            true ->
                SubElBody = erl_syntax:case_expr(
			      ?AST(proplists:get_bool(ignore_els, __Opts)),
			      [erl_syntax:clause(
				 [?AST(true)],
				 none,
				 [make_function_call(
                                    FunName,
                                    [?AST(__TopXMLNS),
				     ?AST(__Opts),
				     ?AST(_els)|CDataVars ++ ElemVars] ++
                                        [erl_syntax:list(
					   [?AST(_el)],
                                           label_to_var('$_els'))])]),
			       erl_syntax:clause(
				 [?AST(false)],
				 none,
				 [?AST(__XMLNS = '?a(ModName)':get_attr(
						   <<"xmlns">>, _attrs, __TopXMLNS)),
				  erl_syntax:case_expr(
				    ?AST('?a(ModName)':get_mod(_name, __XMLNS)),
				   [erl_syntax:clause(
				      [?AST(undefined)],
				      none,
				      [make_function_call(
					 FunName,
					 [?AST(__TopXMLNS),
					  ?AST(__Opts),
					  ?AST(_els)|CDataVars ++ ElemVars] ++
					     [erl_syntax:list(
						[?AST(_el)],
						label_to_var('$_els'))])]),
				    erl_syntax:clause(
				      [?AST(Mod)],
				      none,
				      [make_function_call(
					 FunName,
					 [?AST(__TopXMLNS),
					  ?AST(__Opts),
					  ?AST(_els)|CDataVars ++ ElemVars] ++
					     [erl_syntax:list(
						[?AST(Mod:do_decode(_name, __XMLNS, _el, __Opts))],
						label_to_var('$_els'))])])])])]),
                [erl_syntax:clause(SubElPattern, none, [SubElBody])];
            false ->
                []
        end,
    PassClause = if SubElVars == []; CDataVars == [] ->
                         [erl_syntax:clause(
                            [?AST(__TopXMLNS),
			     ?AST(__Opts),
			     ?AST([_ | _els])|
			     CDataVars ++ ElemVars ++ SubElVars],
                            none,
                            [make_function_call(
                               FunName,
                               [?AST(__TopXMLNS),
				?AST(__Opts), ?AST(_els)
				|CDataVars ++ ElemVars ++ SubElVars])])];
                    true ->
                         []
                 end,
    [erl_syntax:function(
       erl_syntax:atom(FunName),
       [NilClause|CDataClause ++ ElemClauses ++ SubElClause ++ PassClause])].

make_attrs_dec_fun(FunName, Attrs, Tag) ->
    AttrVars = lists:map(
                 fun(#attr{name = AttrName, label = AttrLabel}) ->
                         label_to_var(prepare_label(AttrLabel, AttrName))
                 end, Attrs),
    Clauses =
        lists:map(
          fun(#attr{name = Name, label = Label}) ->
                  Var = label_to_var(prepare_label(Label, Name)),
                  Pattern = [?AST(__TopXMLNS),
			     ?AST([{'?a(Name)', _val} | _attrs]) |
                             lists:map(
                               fun(V) when V == Var ->
                                       VName = erl_syntax:variable_literal(V),
                                       erl_syntax:variable("_" ++ VName);
                                  (V) ->
                                       V
                               end, AttrVars)],
                  Body = [make_function_call(
                            FunName,
                            [?AST(__TopXMLNS), ?AST(_attrs) |
                             lists:map(
                               fun(V) when V == Var ->
                                       ?AST(_val);
                                  (V) ->
                                       V
                               end, AttrVars)])],
                  erl_syntax:clause(Pattern, none, Body)
          end, Attrs),
    if Clauses /= [] ->
            PassClause = erl_syntax:clause(
                           [?AST(__TopXMLNS), ?AST([_|_attrs])|AttrVars],
                           none,
                           [make_function_call(
                              FunName,
                              [?AST(__TopXMLNS), ?AST(_attrs)|AttrVars])]),
            Result = lists:map(
                       fun(#attr{name = Name, label = Label}) ->
                               Var = label_to_var(prepare_label(Label, Name)),
                               make_function_call(
                                 make_dec_fun_name([Name,attr,Tag]),
				 [?AST(__TopXMLNS), Var])
                       end, Attrs),
            NilClause = erl_syntax:clause(
                          [?AST(__TopXMLNS), ?AST([])|AttrVars],
                          none,
                          [tuple_or_single_var(Result)]),
            [erl_syntax:function(
               erl_syntax:atom(FunName),
               Clauses ++ [PassClause, NilClause])];
       true ->
            []
    end.

make_ref_enc_funs(Elem, Tag, AllElems) ->
    ModName = Elem#elem.module,
    lists:map(
      fun({L, [#ref{min = Min, max = Max, default = Default}|_] = Refs}) ->
              DefaultClause = if Min == 0, Max == 1 ->
                                      [erl_syntax:clause(
                                         [abstract(Default), ?AST(__TopXMLNS),
					  ?AST(_acc)],
                                         none, [?AST(_acc)])];
                                 Min == 1, Max == 1 ->
                                      [];
                                 true ->
                                      [erl_syntax:clause(
                                         [?AST([]), ?AST(__TopXMLNS), ?AST(_acc)],
                                         none, [?AST(_acc)])]
                              end,
              Var = label_to_var(L),
              Clauses =
                  lists:map(
                    fun(#ref{name = RefName, max = 1}) ->
                            RefElem = get_elem_by_ref(RefName, AllElems),
			    RefMod = RefElem#elem.module,
                            Pattern =
                                if length(Refs) > 1 ->
                                        MatchVar = erl_syntax:match_expr(
                                                     labels_to_underscores(
                                                       RefElem#elem.result),
                                                     Var),
                                        [MatchVar, ?AST(__TopXMLNS), ?AST(_acc)];
                                   true ->
                                        [Var, ?AST(__TopXMLNS), ?AST(_acc)]
                                end,
                            erl_syntax:clause(
                              Pattern,
                              none,
                              [erl_syntax:list(
                                 [make_function_call(
                                    make_enc_fun_name(ModName, RefMod, [RefName]),
                                    [Var, ?AST(__TopXMLNS)])],
                                 ?AST(_acc))]);
                       (#ref{name = RefName}) ->
                            RefElem = get_elem_by_ref(RefName, AllElems),
			    RefMod = RefElem#elem.module,
                            Pattern =
                                if length(Refs) > 1 ->
                                        erl_syntax:match_expr(
                                          labels_to_underscores(
                                            RefElem#elem.result),
                                          Var);
                                   true ->
                                        Var
                                end,
                            erl_syntax:clause(
                              [?AST(['?Pattern' | _els]), ?AST(__TopXMLNS), ?AST(_acc)],
                              none,
                              [make_function_call(
                                 make_enc_fun_name([L,Tag]),
                                 [?AST(_els),
				  ?AST(__TopXMLNS),
                                  erl_syntax:list(
                                    [make_function_call(
                                       make_enc_fun_name(ModName, RefMod, [RefName]),
                                       [Var, ?AST(__TopXMLNS)])],
                                    ?AST(_acc))])])
                    end, Refs),
              erl_syntax:function(
                erl_syntax:atom(make_enc_fun_name([L,Tag])),
                DefaultClause ++ Clauses)
      end, group_refs(Elem#elem.refs)).

make_elem_dec_fun_json(
  #elem{name = Name, result = Result, refs = Refs, module = Mod,
        cdata = CData, attrs = Attrs, xmlns = XMLNS,
        ignore_els = IgnoreEls} = Elem,
  Tag, AllElems, Types, ModName, FunTypes, PredefRecords, Opts) ->
    {_Tags, RecTypes, _RecDict} = Types,
    case term_is_record(Result) of
        true ->
            ResultWithVars = subst_labels(Result, PredefRecords),
            [RecName | RecLabels] = tuple_to_list(Result),
            BRecName = atom_to_binary(RecName, utf8),
            RecLabels2 =
                lists:filter(
                  fun(Label) ->
                          case is_atom(Label) andalso is_label(Label) of
                              true ->
                                  JSONType = get_label_json_type(
                                               Label, Elem, RecTypes,
                                               FunTypes, AllElems, Opts),
                                  case JSONType of
                                      ignore ->
                                          false;
                                      _ ->
                                          true
                                  end;
                              false ->
                                  false
                          end
                  end, RecLabels),
            case RecLabels of
                [Label] ->
                    Res =
                        case is_label(Label) of
                            true ->
                                JSONType = get_label_json_type(
                                             Label, Elem, RecTypes,
                                             FunTypes, AllElems, Opts),
                                io:format("ref ~p~n", [{Label, JSONType}]),
                                decode_json_field2(
                                  JSONType, ?AST(_value),
                                  Elem#elem.module, ModName);
                            false ->
                                BLabel = atom_to_binary(Label, utf8),
                                ?AST('?a(BLabel)')
                        end,
                    [erl_syntax:function(
                       erl_syntax:atom(make_dec_fun_name([Tag, json])),
                       [erl_syntax:clause(
                          [?AST(_value)],
                          none,
                          [?AST({'?a(RecName)', '?Res'})]
                         )])];
                 _ ->
                    case RecLabels2 of
                        [] ->
                            [erl_syntax:function(
                               erl_syntax:atom(make_dec_fun_name([Tag, json])),
                               [erl_syntax:clause(
                                  [?AST(_)],
                                  none,
                                  [ResultWithVars]
                                 )])];
                        _ ->
                            DecCall = make_function_call(
                                        make_dec_fun_name([fields, Tag, json]),
                                        [?AST(_fields)|
                                         lists:map(
                                           fun(_) ->
                                                   ?AST(undefined)
                                           end, RecLabels2)]),
                            DecTuple =
                                tuple_or_single_var(
                                  lists:map(fun label_to_var/1, RecLabels2)),
                            [erl_syntax:function(
                               erl_syntax:atom(make_dec_fun_name([Tag, json])),
                               [erl_syntax:clause(
                                  [?AST({_fields})],
                                  none,
                                  [?AST('?DecTuple' = '?DecCall')] ++
                                  [ResultWithVars]
                                 )])]
                                ++ make_json_fields_dec_fun(
                                     make_dec_fun_name([fields, Tag, json]),
                                     RecLabels2, Tag,
                                     Elem, RecTypes, FunTypes, AllElems,
                                     Opts, ModName)
                    end
            end;
        false when is_tuple(Result) ->
            RecLabels = tuple_to_list(Result),
            Res = lists:map(
                    fun(Label) ->
                            Var = label_to_var(Label),
                            JSONType = get_label_json_type(
                                         Label, Elem, RecTypes,
                                         FunTypes, AllElems, Opts),
                            decode_json_field(
                              JSONType, Label, Var,
                              Elem#elem.module, ModName)
                    end, RecLabels),
            [erl_syntax:function(
               erl_syntax:atom(make_dec_fun_name([Tag, json])),
               [erl_syntax:clause(
                  [erl_syntax:list(lists:map(fun label_to_var/1, RecLabels))],
                  none,
                  [erl_syntax:tuple(Res)]
                 )])];
        false when is_atom(Result) ->
io:format("refs3 ~p~n", [{Tag, Refs, Result}]),
            Label = Result,
            Res =
                case is_label(Label) of
                    true ->
                        JSONType = get_label_json_type(
                                     Label, Elem, RecTypes,
                                     FunTypes, AllElems, Opts),
                        io:format("ref ~p~n", [{Label, JSONType}]),
                        VLabel = label_to_var(Label),
                        decode_json_field2(
                          JSONType, ?AST(_val),
                          Elem#elem.module, ModName);
                    false ->
                        BLabel = atom_to_binary(Label, utf8),
                        ?AST('?a(BLabel)')
                end,
            [erl_syntax:function(
               erl_syntax:atom(make_dec_fun_name([Tag, json])),
               [erl_syntax:clause(
                  [?AST(_val)],
                  none,
                  [Res]
                 )])];
        false ->
io:format("refs2 ~p~n", [{Tag, Refs, Result}]),
            []
    end.

make_json_fields_dec_fun(FunName, Attrs, Tag,
                         Elem, RecTypes, FunTypes, AllElems, Opts, ModName) ->
    AttrVars = lists:map(fun label_to_var/1, Attrs),
    Clauses =
        lists:map(
          fun(Label) ->
                  FName = label_to_record_field(Label),
                  FBName = atom_to_binary(FName, utf8),
                  Var = label_to_var(Label),
                  Pattern = [?AST([{'?a(FBName)', _val} | _fields]) |
                             lists:map(
                               fun(V) when V == Var ->
                                       VName = erl_syntax:variable_literal(V),
                                       erl_syntax:variable("_" ++ VName);
                                  (V) ->
                                       V
                               end, AttrVars)],
                  Body = [make_function_call(
                            FunName,
                            [?AST(_fields) |
                             lists:map(
                               fun(V) when V == Var ->
                                       ?AST(_val);
                                  (V) ->
                                       V
                               end, AttrVars)])],
                  erl_syntax:clause(Pattern, none, Body)
          end, Attrs),
    if Clauses /= [] ->
            PassClause = erl_syntax:clause(
                           [?AST([_ | _fields]) | AttrVars],
                           none,
                           [make_function_call(
                              FunName,
                              [?AST(_fields) | AttrVars])]),
            Result = lists:map(
                       fun(Label) ->
                               Var = label_to_var(Label),
                               JSONType = get_label_json_type(
                                            Label, Elem, RecTypes,
                                            FunTypes, AllElems, Opts),
                               decode_json_field(
                                 JSONType, Label, Var,
                                 Elem#elem.module, ModName)
                       end, Attrs),
            NilClause = erl_syntax:clause(
                          [?AST([]) | AttrVars],
                          none,
                          [tuple_or_single_var(Result)]),
            [erl_syntax:function(
               erl_syntax:atom(FunName),
               Clauses ++ [PassClause, NilClause])];
       true ->
            []
    end.

make_elem_enc_fun(#elem{result = Result, attrs = Attrs,
                        name = ElemName, xmlns = XMLNS,
                        cdata = CData, refs = Refs} = Elem,
                  Tag, AllElems, ModName) ->
    CDataLabel = CData#cdata.label,
    HaveCData = have_label(Result, CDataLabel),
    HaveRefs = Refs /= [],
    HaveEls = have_label(Result, '$_els'),
    NewTopXMLNSCall =
	make_function_call(
	  {ModName, choose_top_xmlns},
	  case lists:keyfind(<<"xmlns">>, #attr.name, Attrs) of
	      #attr{label = L, name = N} ->
		  if is_list(XMLNS) ->
			  [label_to_var(prepare_label(L, N)),
			   erl_syntax:list([abstract(NS) || NS <- XMLNS])];
		     true ->
			  [label_to_var(prepare_label(L, N)),
			   erl_syntax:list([abstract(XMLNS)])]
		  end;
	      false when is_list(XMLNS) ->
		  [?AST(<<>>), erl_syntax:list([abstract(NS) || NS <- XMLNS])];
	      false ->
		  [abstract(XMLNS), ?AST([])]
	  end ++ [?AST(__TopXMLNS)]),
    SubElGenerator = case have_label(Result, '$_els') of
                         true ->
                             erl_syntax:list_comp(
			       ?AST('?a(ModName)':encode(_el, __NewTopXMLNS)),
                               [erl_syntax:generator(
                                  ?AST(_el),
                                  label_to_var('$_els'))]);
                         false ->
                             ?AST([])
                     end,
    RefsFun = lists:foldr(
		fun({Label, _}, Acc) ->
			Var = label_to_var(Label),
			make_function_call(
			  make_enc_fun_name([Label,Tag]),
			  [Var, ?AST(__NewTopXMLNS), Acc])
		end, ?AST([]), group_refs(Refs)),
    CDataFun = if HaveRefs and HaveCData ->
		       make_function_call(
			 {lists, reverse},
			 [make_function_call(make_enc_fun_name([cdata,Tag]),
					     [label_to_var(CDataLabel),
					      RefsFun])]);
		  HaveRefs and not HaveCData ->
		       ?AST(lists:reverse('?RefsFun'));
		  HaveCData and not HaveRefs ->
		       make_function_call(make_enc_fun_name([cdata,Tag]),
					     [label_to_var(CDataLabel),
					      ?AST([])]);
                  true ->
		       ?AST([])
               end,
    ResFun = if (HaveCData or HaveRefs) and HaveEls ->
		     ?AST('?SubElGenerator' ++ '?CDataFun');
		HaveCData or HaveRefs ->
		     CDataFun;
		HaveEls ->
		     SubElGenerator;
		true ->
		     ?AST([])
	     end,
    AttrFun = lists:foldl(
                fun(#attr{name = AttrName, label = AttrLabel}, Acc)
		      when AttrName /= <<"xmlns">> ->
                        Var = label_to_var(prepare_label(AttrLabel, AttrName)),
                        make_function_call(
                          make_enc_fun_name([AttrName,attr,Tag]),
                          [Var, Acc]);
		   (_, Acc) ->
			Acc
                end, ?AST('?a(ModName)':enc_xmlns_attrs(__NewTopXMLNS, __TopXMLNS)), Attrs),
    [erl_syntax:function(
       erl_syntax:atom(make_enc_fun_name([Tag])),
       [erl_syntax:clause(
          [subst_labels(Result), ?AST(__TopXMLNS)],
          none,
          [?AST(__NewTopXMLNS = '?NewTopXMLNSCall'),
	   ?AST(_els = '?ResFun'),
	   ?AST(_attrs = '?AttrFun'),
	   ?AST({xmlel, '?a(ElemName)', _attrs, _els})
          ])])] ++ make_ref_enc_funs(Elem, Tag, AllElems).

make_elem_enc_fun_json(
  #elem{result = Result, attrs = Attrs,
        name = ElemName,
        cdata = CData, refs = Refs} = Elem,
  Tag, AllElems, ModName, Types, FunTypes, PredefRecords, Opts) ->
io:format("refs ~p~n", [{Tag, Refs, Result}]),
    case term_is_record(Result) of
        true ->
            {_Tags, RecTypes, _RecDict} = Types,
            [RecName | RecLabels] = tuple_to_list(Result),
            BRecName = atom_to_binary(RecName, utf8),
            ConvertLabelFun =
                fun(Label, {K, ASTs}) ->
                        InVar = erl_syntax:variable(
                                  "_fields" ++ integer_to_list(K)),
                        OutVar = erl_syntax:variable(
                                   "_fields" ++ integer_to_list(K + 1)),
                        case is_label(Label) of
                            true ->
                                FName = label_to_record_field(Label),
                                FBName = atom_to_binary(FName, utf8),
                                JSONType = get_label_json_type(
                                             Label, Elem, RecTypes,
                                             FunTypes, AllElems, Opts),
                                io:format("ref ~p~n", [{Label, JSONType}]),
                                Value =
                                    encode_json_field(
                                      JSONType, Label, InVar, FBName,
                                      Elem#elem.module, ModName),
                                Assign = ?AST('?OutVar' = '?Value'),
                                                %erl_syntax:tuple([?AST('?a(FBName)'), Value]),
                                {K + 1, [Assign | ASTs]};
                            false ->
                                {ok, RecFields} = dict:find(RecName, PredefRecords),
                                {FName, _, _} = lists:nth(K + 1, RecFields),
                                FBName = atom_to_binary(FName, utf8),
                                BLabel = atom_to_binary(Label, utf8),
                                Value = ?AST([{'?a(FBName)', '?a(BLabel)'} |
                                              '?InVar']),
                                Assign = ?AST('?OutVar' = '?Value'),
                                {K + 1, [Assign | ASTs]}
                        end
                end,
            case RecLabels of
                [] ->
                    [erl_syntax:function(
                       erl_syntax:atom(make_enc_fun_name([Tag, json])),
                       [erl_syntax:clause(
                          [?AST({'?a(RecName)'})],
                          none,
                          [?AST(null)]
                         )])];
                [Label] ->
                    Res =
                        case is_label(Label) of
                            true ->
                                JSONType = get_label_json_type(
                                             Label, Elem, RecTypes,
                                             FunTypes, AllElems, Opts),
                                io:format("ref ~p~n", [{Label, JSONType}]),
                                encode_json_field2(
                                  JSONType, ?AST(_val),
                                  Elem#elem.module, ModName);
                            false ->
                                BLabel = atom_to_binary(Label, utf8),
                                ?AST('?a(BLabel)')
                        end,
                    [erl_syntax:function(
                       erl_syntax:atom(make_enc_fun_name([Tag, json])),
                       [erl_syntax:clause(
                          [?AST({'?a(RecName)', _val})],
                          none,
                          [Res]
                         )])];
                _ ->
                    {K, MakeJSON} =
                        lists:foldl(ConvertLabelFun, {0, []}, RecLabels),
                    RecFieldsJSON = erl_syntax:variable("_fields" ++ integer_to_list(K)),
                                                %RecFieldsJSON =
                                                %    erl_syntax:list(
                                                %      lists:map(ConvertLabelFun, RecLabels)),
                    RecJSON =
                        erl_syntax:tuple(
                          [RecFieldsJSON]),
                    [erl_syntax:function(
                       erl_syntax:atom(make_enc_fun_name([Tag, json])),
                       [erl_syntax:clause(
                          [subst_labels(Result)],
                          none,
                          [?AST(_fields0 = [])] ++
                          lists:reverse(MakeJSON) ++
                          [RecJSON]
                         )])]
            end;
        false when is_tuple(Result) ->
            {_Tags, RecTypes, _RecDict} = Types,
            RecLabels = tuple_to_list(Result),
            ConvertLabelFun =
                fun(Label, {K, ASTs}) ->
                        InVar = erl_syntax:variable(
                                  "_fields" ++ integer_to_list(K)),
                        OutVar = erl_syntax:variable(
                                   "_fields" ++ integer_to_list(K + 1)),
                        case is_label(Label) of
                            true ->
                                JSONType = get_label_json_type(
                                             Label, Elem, RecTypes,
                                             FunTypes, AllElems, Opts),
                                io:format("ref ~p~n", [{Label, JSONType}]),
                                VLabel = label_to_var(Label),
                                Res = encode_json_field2(
                                        JSONType, VLabel,
                                        Elem#elem.module, ModName),
                                Value = ?AST(['?Res' | '?InVar']),
                                Assign = ?AST('?OutVar' = '?Value'),
                                {K + 1, [Assign | ASTs]};
                            false ->
                                BLabel = atom_to_binary(Label, utf8),
                                Value = ?AST(['?a(BLabel)' | '?InVar']),
                                Assign = ?AST('?OutVar' = '?Value'),
                                {K + 1, [Assign | ASTs]}
                        end
                end,
            {K, MakeJSON} =
                lists:foldl(ConvertLabelFun, {0, []}, lists:reverse(RecLabels)),
            RecFieldsJSON = erl_syntax:variable("_fields" ++ integer_to_list(K)),
                                                %RecFieldsJSON =
                                                %    erl_syntax:list(
                                                %      lists:map(ConvertLabelFun, RecLabels)),
            JSON = RecFieldsJSON,
            [erl_syntax:function(
               erl_syntax:atom(make_enc_fun_name([Tag, json])),
               [erl_syntax:clause(
                  [subst_labels(Result)],
                  none,
                  [?AST(_fields0 = [])] ++
                  lists:reverse(MakeJSON) ++
                  [JSON]
                 )])];
        false when is_atom(Result) ->
io:format("refs3 ~p~n", [{Tag, Refs, Result}]),
            Label = Result,
            {_Tags, RecTypes, _RecDict} = Types,
            Res =
                case is_label(Label) of
                    true ->
                        JSONType = get_label_json_type(
                                     Label, Elem, RecTypes,
                                     FunTypes, AllElems, Opts),
                        io:format("ref ~p~n", [{Label, JSONType}]),
                        encode_json_field2(
                          JSONType, ?AST(_val),
                          Elem#elem.module, ModName);
                    false ->
                        BLabel = atom_to_binary(Label, utf8),
                        ?AST('?a(BLabel)')
                end,
            [erl_syntax:function(
               erl_syntax:atom(make_enc_fun_name([Tag, json])),
               [erl_syntax:clause(
                  [?AST(_val)],
                  none,
                  [Res]
                 )])];
        false ->
io:format("refs2 ~p~n", [{Tag, Refs, Result}]),
            []
    end.

encode_json_field(Type, Label, InVar, FBName, ModName, TopModName) ->
    VLabel = label_to_var(Label),
    case Type of
        ignore ->
            InVar;
        {option, SubType, Default} ->
            Res = encode_json_field2(SubType, VLabel, ModName, TopModName),
            ?AST(case '?VLabel' of
                     '?a(Default)' -> '?InVar';
                     _ -> [{'?a(FBName)', '?Res'} | '?InVar']
                 end
                );
        _ ->
            Res = encode_json_field2(Type, VLabel, ModName, TopModName),
            ?AST([{'?a(FBName)', '?Res'} | '?InVar'])
    end.

encode_json_field2(Type, InVar, ModName, TopModName) ->
    case Type of
        [T] ->
            encode_json_field2(T, InVar, ModName, TopModName);
        binary -> InVar;
        integer -> InVar;
        boolean -> InVar;
        {binary, _DecFun, EncFun} ->
            case EncFun of
                {M, F, Args} ->
                    make_function_call(
                      {M, F},
                      [InVar |
                       [abstract(Arg) || Arg <- Args]]);
                {F, Args} ->
                    make_function_call(
                      F,
                      [InVar |
                       [abstract(Arg) || Arg <- Args]]);
                undefined ->
                    InVar
            end;
        {external, _, RefMod, RefTag} ->
            io:format("ext ~p~n", [Type]),
            make_function_call(
              make_enc_fun_name(ModName, RefMod, [RefTag, json]),
              [InVar]);
        {list, T} ->
            Res = encode_json_field2(T, ?AST(_X), ModName, TopModName),
            ?AST(lists:map(fun(_X) -> '?Res' end, '?InVar'));
        {tuple, Args} ->
            Pattern =
                erl_syntax:tuple(
                  lists:map(
                    fun(I) ->
                            erl_syntax:variable("_X" ++ integer_to_list(I))
                    end, lists:seq(1, length(Args)))),
            Tuple =
                erl_syntax:list(
                  lists:map(
                    fun(I) ->
                            Var = erl_syntax:variable(
                                    "_X" ++ integer_to_list(I)),
                            encode_json_field2(lists:nth(I, Args), Var,
                                               ModName, TopModName)
                    end, lists:seq(1, length(Args)))),
            ?AST(fun('?Pattern') ->
                         '?Tuple'
                 end('?InVar'));
        %timestamp ->
        %    ?AST(tuple_to_list('?InVar'));
        ip_address ->
            ?AST(enc_ip('?InVar'));
        jid ->
            ?AST(fun({jid, _U, _S, _R, _LU, _LS, _LR}) ->
                         {[{<<"user">>, _U},
                           {<<"server">>, _S},
                           {<<"resource">>, _R},
                           {<<"luser">>, _LU},
                           {<<"lserver">>, _LS},
                           {<<"lresource">>, _LR}
                          ]}
                 end('?InVar'));
        {atom, unknown} ->
            io:format("asd ~p~n", [Type]),
            InVar;
        {atom, _} ->
            encode_json_case([Type], InVar, ModName, TopModName);
        Types when is_list(Types) ->
            encode_json_case(Types, InVar, ModName, TopModName);
        {option, SubType, Default} when is_atom(Default) ->
            Res = encode_json_field2(SubType, InVar, ModName, TopModName),
            ?AST(case '?InVar' of
                     '?a(Default)' -> <<"None">>;
                     _ -> [<<"Some">>, '?Res']
                 end
                );
        {option, SubType, _Default} ->
            encode_json_field2(SubType, InVar, ModName, TopModName);
        els ->
            erl_syntax:list_comp(
              ?AST('?a(TopModName)':encode_json(_el)),
              [erl_syntax:generator(?AST(_el), InVar)]);
        _ ->
            io:format("asd ~p~n", [Type]),
            InVar
    end.

encode_json_case(Types, InVar, ModName, TopModName) ->
    io:format("cases' ~p~n", [Types]),
    TMap =
        lists:foldl(
          fun({atom, Atoms}, Map) ->
                  lists:foldl(
                    fun(Atom, Acc) ->
                            case maps:find(Atom, Acc) of
                                {ok, atom} ->
                                    Acc;
                                {ok, T} ->
                                    erlang:error({conflicting_types,
                                                  {atom, Acc}, T});
                                error ->
                                    maps:put(Atom, atom, Acc)
                            end
                    end, Map, Atoms);
             ({external, {record, Tag, _}, _, _} = Rec, Map) ->
                  case maps:find(Tag, Map) of
                      {ok, Rec} ->
                          Map;
                      {ok, T} ->
                          erlang:error({conflicting_types, Rec, T});
                      error ->
                          maps:put(Tag, Rec, Map)
                  end
          end, maps:new(), Types),
    io:format("cases ~p~n", [TMap]),
    Clauses =
        lists:map(
          fun({Atom, atom}) ->
                  BAtom = atom_to_binary(Atom, utf8),
                  erl_syntax:clause(
                    [?AST('?a(Atom)')],
                    none,
                    [?AST('?a(BAtom)')]);
             ({Tag, {external, {record, Tag, Arity}, _, _} = Rec}) ->
                  Pattern =
                      erl_syntax:tuple(
                        [erl_syntax:atom(Tag) |
                         lists:duplicate(Arity, erl_syntax:underscore())
                        ]
                       ),
                  Res = encode_json_field2(Rec, InVar, ModName, TopModName),
                  erl_syntax:clause(
                    [Pattern],
                    none,
                    [Res])
          end, maps:to_list(TMap)),
    erl_syntax:case_expr(InVar, Clauses).

decode_json_field(Type, Label, InVar, ModName, TopModName) ->
    case Type of
        ignore ->
            erlang:error({internal_error, ?MODULE});
        {option, SubType, Default} ->
            Res = decode_json_field2(SubType, InVar, ModName, TopModName),
            ?AST(case '?InVar' of
                     undefined -> '?a(Default)';
                     _ -> '?Res'
                 end
                );
        _ ->
            decode_json_field2(Type, InVar, ModName, TopModName)
    end.

decode_json_field2(Type, InVar, ModName, TopModName) ->
    case Type of
        [T] ->
            decode_json_field2(T, InVar, ModName, TopModName);
        binary -> InVar;
        integer -> InVar;
        boolean -> InVar;
        {binary, DecFun, _EncFun} ->
            case DecFun of
                {M, F, Args} ->
                    make_function_call(
                      {M, F},
                      [InVar |
                       [abstract(Arg) || Arg <- Args]]);
                {F, Args} ->
                    make_function_call(
                      F,
                      [InVar |
                       [abstract(Arg) || Arg <- Args]]);
                undefined ->
                    InVar
            end;
        {external, _, RefMod, RefTag} ->
            io:format("ext ~p~n", [Type]),
            make_function_call(
              make_dec_fun_name(ModName, RefMod, [RefTag, json]),
              [InVar]);
        {list, T} ->
            Res = decode_json_field2(T, ?AST(_X), ModName, TopModName),
            ?AST(lists:map(fun(_X) -> '?Res' end, '?InVar'));
        {tuple, Args} ->
            Pattern =
                erl_syntax:list(
                  lists:map(
                    fun(I) ->
                            erl_syntax:variable("_X" ++ integer_to_list(I))
                    end, lists:seq(1, length(Args)))),
            Tuple =
                erl_syntax:tuple(
                  lists:map(
                    fun(I) ->
                            Var = erl_syntax:variable(
                                    "_X" ++ integer_to_list(I)),
                            decode_json_field2(lists:nth(I, Args), Var,
                                               ModName, TopModName)
                    end, lists:seq(1, length(Args)))),
            ?AST(fun('?Pattern') ->
                         '?Tuple'
                 end('?InVar'));
        ip_address ->
            ?AST(dec_ip('?InVar'));
        jid ->
            ?AST(fun({_jid}) ->
                         case jid:make(proplists:get_value(<<"user">>, _jid, <<>>),
                                       proplists:get_value(<<"server">>, _jid),
                                       proplists:get_value(<<"resource">>, _jid, <<>>)) of
                             error -> erlang:error({bad_jid, _jid});
                             _JID -> _JID
                         end
                 end('?InVar'));
        {atom, unknown} ->
            io:format("asd ~p~n", [Type]),
            InVar;
        {atom, _} ->
            decode_json_case([Type], InVar, ModName, TopModName);
        Types when is_list(Types) ->
            decode_json_case(Types, InVar, ModName, TopModName);
        {option, SubType, Default} when is_atom(Default) ->
            Res = decode_json_field2(SubType, ?AST(_val1), ModName, TopModName),
            ?AST(fun(<<"None">>) -> '?a(Default)';
                    ([<<"Some">>, _val1]) -> '?Res'
                 end('?InVar')
                );
        {option, SubType, _Default} ->
            decode_json_field2(SubType, InVar, ModName, TopModName);
        els ->
            erl_syntax:list_comp(
              ?AST('?a(TopModName)':decode_json(_el)),
              [erl_syntax:generator(?AST(_el), InVar)]);
        _ ->
            io:format("asd ~p~n", [Type]),
            InVar
    end.

decode_json_case(Types, InVar, ModName, TopModName) ->
    io:format("cases' ~p~n", [Types]),
    TMap =
        lists:foldl(
          fun({atom, Atoms}, Map) ->
                  lists:foldl(
                    fun(Atom, Acc) ->
                            case maps:find(Atom, Acc) of
                                {ok, atom} ->
                                    Acc;
                                {ok, T} ->
                                    erlang:error({conflicting_types,
                                                  {atom, Acc}, T});
                                error ->
                                    maps:put(Atom, atom, Acc)
                            end
                    end, Map, Atoms);
             ({external, {record, Tag, _}, _, _} = Rec, Map) ->
                  case maps:find(Tag, Map) of
                      {ok, Rec} ->
                          Map;
                      {ok, T} ->
                          erlang:error({conflicting_types, Rec, T});
                      error ->
                          maps:put(Tag, Rec, Map)
                  end
          end, maps:new(), Types),
    io:format("cases ~p~n", [TMap]),
    Clauses =
        lists:map(
          fun({Atom, atom}) ->
                  BAtom = atom_to_binary(Atom, utf8),
                  erl_syntax:clause(
                    [?AST('?a(BAtom)')],
                    none,
                    [?AST('?a(Atom)')]);
             ({Tag, {external, {record, Tag, Arity}, _, _} = Rec}) ->
                  BTag = atom_to_binary(Tag, utf8),
                  Pattern = ?AST(['?a(BTag)', _]),
                  Res = decode_json_field2(Rec, InVar, ModName, TopModName),
                  erl_syntax:clause(
                    [Pattern],
                    none,
                    [Res])
          end, maps:to_list(TMap)),
    erl_syntax:case_expr(InVar, Clauses).

get_label_json_type(Label, Elem, Dict, FunSpecs, AllElems, Opts) ->
    case get_spec_by_label(Label, Elem) of
        sub_els ->
            %XMLType = t_remote(fxml, xmlel),
	    %T = case proplists:get_value(add_type_specs, Opts) of
	    %        SpecName when is_atom(SpecName), SpecName /= undefined ->
            %            io:format("sub_els1 ~p~n", [{SpecName, Opts}]),
	    %    	erl_types:t_sup([XMLType, t_identifier(SpecName)]);
	    %        _ ->
            %            io:format("sub_els2 ~p~n", [Label]),
	    %    	erl_types:t_any()
	    %    end,
            %{todo, erl_types:t_list(T), [], false};
            els;
	'_' ->
	    ignore;
	#attr{dec = undefined, default = Default,
              required = false} ->
            {option, binary, Default};
	#attr{dec = undefined, default = _Default, required = _IsRequired} ->
            binary;
        #attr{dec = DecFun, enc = EncFun, default = Default, required = IsRequired} ->
            case json_use_enc_dec(DecFun) of
                true ->
                    case {Default, IsRequired} of
                        {undefined, false} ->
                            {option, {binary, DecFun, EncFun}, Default};
                        _ ->
                            {binary, DecFun, EncFun}
                    end;
                false ->
                    FType = get_fun_return_type(DecFun, FunSpecs),
                    get_json_type(FType, Default, IsRequired)
            end;
	#cdata{dec = undefined, default = Default,
               required = false} ->
            {option, binary, Default};
	#cdata{dec = undefined, default = _Default, required = _IsRequired} ->
            binary;
        #cdata{dec = DecFun, enc = EncFun, default = Default, required = IsRequired} ->
            case json_use_enc_dec(DecFun) of
                true ->
                    case {Default, IsRequired} of
                        {undefined, false} ->
                            {option, {binary, DecFun, EncFun}, Default};
                        _ ->
                            {binary, DecFun, EncFun}
                    end;
                false ->
                    FType = get_fun_return_type(DecFun, FunSpecs),
                    get_json_type(FType, Default, IsRequired)
            end;
        [#ref{min = Min, max = Max, default = Default}|_] = Refs ->
            Types = lists:map(
                      fun(#ref{name = RefTag}) ->
                              case dict:find(RefTag, Dict) of
				  {ok, T} ->
                                      case get_json_type2(T) of
                                          %{record, _RecName, _RecSize} = Rec ->
                                          %    RefElem = get_elem_by_ref(RefTag, AllElems),
                                          %    RefMod = RefElem#elem.module,
                                          %    {external, Rec, RefMod, RefTag};
                                          %{tuple, _} = Tuple ->
                                          %    RefElem = get_elem_by_ref(RefTag, AllElems),
                                          %    RefMod = RefElem#elem.module,
                                          %    {external, Tuple, RefMod, RefTag};
                                          %RT -> RT
                                          {atom, _} = RT -> RT;
                                          RT ->
                                              RefElem = get_elem_by_ref(RefTag, AllElems),
                                              RefMod = RefElem#elem.module,
                                              {external, RT, RefMod, RefTag}
                                      end;
				  error ->
                                      erlang:error({internal_error, ?MODULE})
			      end
                      end, Refs),
            if true orelse length(Refs) > 1 ->
                    io:format("qaz ~p~n", [{Refs, Types}]);
               true -> ok
            end,
            IsRequired = (Min == 1) and (Max == 1),
            case {Default, IsRequired, Max} of
                {undefined, false, 1} ->
                    {option, Types, Default};
                {false, false, 1} when Types == [{atom,[true]}] ->
                    {option, boolean, Default};
                {_, _, 1} ->
                    Types;
                _ ->
                    {list, Types}
                    %{todo,
                    % erl_types:t_list(
                    %   erl_types:t_subtract(
                    %     Type, erl_types:t_from_term(Default))),
                    % [], false}
            end
    end.

get_json_type(FType, Default, IsRequired) ->
    IsRaw = is_json_raw_type(FType),
    case {Default, IsRequired, IsRaw} of
        {undefined, false, {true, T}} ->
            {option, T, Default};
        {_Default, _, {true, T}} ->
            T;
        {undefined, true, _} ->
            get_json_type2(FType);
        {undefined, false, _} ->
            {option, get_json_type2(FType), Default};
        {_Default, _, _} ->
            get_json_type2(FType)
    end.

get_json_type2(FType) ->
    io:format("ttt ~p~n", [FType]),
    case erl_types:t_is_tuple(FType) of
        true ->
            case erl_types:t_tuple_args(FType) of
                [ATag] ->
                    case erl_types:t_is_atom(ATag) of
                        true ->
                            case erl_types:t_atom_vals(ATag) of
                                [Tag] ->
                                    {record, Tag, erl_types:t_tuple_size(FType)};
                                _ ->
                                    erlang:error({internal_error, ?MODULE})
                            end;
                        false ->
                            erlang:error({internal_error, ?MODULE})
                    end;
                Args ->
                    io:format("tuple ~p~n", [{FType, Args}]),
                    {tuple, lists:map(fun get_json_type2/1, Args)}
            end;
        false ->
            case erl_types:t_is_atom(FType) of
                true ->
                    {atom, erl_types:t_atom_vals(FType)};
                false ->
                    case is_json_raw_type(FType) of
                        {true, T} -> T;
                        false ->
                            case erl_types:t_is_list(FType) of
                                true ->
                                    {list, get_json_type2(erl_types:t_list_elements(FType))};
                                false ->
                                    Undefined = erl_types:t_atom(undefined),
                                    case catch erl_types:t_elements(FType) of
                                        [Undefined, T] ->
                                            {option, get_json_type2(T), undefined};
                                        [_, _ | _] ->
                                            {todo, FType};
                                        _ ->
                                            case erl_types:t_is_any(FType) of
                                                true ->
                                                    {todo, FType};
                                                false ->
                                                    case FType of
                                                        {type, _, list, [T]} ->
                                                            {list, get_json_type2(T)};
                                                        {remote_type, _,
                                                         [{atom, _, erlang},
                                                          {atom, _, timestamp}, []]} ->
                                                            {tuple,
                                                             [integer,
                                                              integer,
                                                              integer]};
                                                        {remote_type, _,
                                                         [{atom, _, inet},
                                                          {atom, _, ip_address}, []]} ->
                                                            ip_address;
                                                        {remote_type, _,
                                                         [{atom, _, jid},
                                                          {atom, _, jid}, []]} ->
                                                            jid;
                                                        {type, _, union, _} ->
                                                            {todo, FType};
                                                        _ ->
                                                            erlang:error({internal_error, ?MODULE})
                                                    end
                                            end
                                    end
                            end
                    end
            end
    end.

is_json_raw_type(Type) ->
    case {erl_types:t_is_binary(Type),
          erl_types:t_is_integer(Type),
          erl_types:t_is_boolean(Type)} of
        {true, _, _} -> {true, binary};
        {_, true, _} -> {true, integer};
        {_, _, true} -> {true, boolean};
        _ -> false
    end.

json_use_enc_dec({dec_ip, []}) -> true;
json_use_enc_dec({dec_host, []}) -> true;
json_use_enc_dec({dec_host_port, []}) -> true;
json_use_enc_dec({xmpp_lang, check, []}) -> true;
json_use_enc_dec({base64, mime_decode, []}) -> true;
json_use_enc_dec({base64, decode, []}) -> true;
json_use_enc_dec(_) -> false.

json_type_to_atd(Type) ->
    case Type of
        [T] ->
            json_type_to_atd(T);
        binary -> "string";
        integer -> "int";
        boolean -> "bool";
        {binary, DecFun, _EncFun} -> "string";
        %{external, _, _RefMod, RefTag} -> atom_to_list(RefTag);
        {external, {record, Tag, _}, _RefMod, _RefTag} ->
            sanitize_for_atd(Tag);
        {external, {list, {record, Tag, _}}, _RefMod, _RefTag} ->
            [sanitize_for_atd(Tag), " list"];
        {external, _, _RefMod, RefTag} ->
            sanitize_for_atd(RefTag);
        {list, T} ->
            [json_type_to_atd(T), " list"];
        {tuple, Args} ->
            ["(",
             string:join(lists:map(fun json_type_to_atd/1, Args), " * "),
             ")"];
        ip_address -> "string";
        jid -> "jid";
        {atom, unknown} ->
            erlang:error({internal_error, ?MODULE});
        {atom, _} ->
            json_case_to_atd([Type]);
        Types when is_list(Types) ->
            json_case_to_atd(Types);
        {option, SubType, _Default} ->
            [json_type_to_atd(SubType), " option"];
        els -> "els"
    end.

json_case_to_atd(Types) ->
    TMap =
        lists:foldl(
          fun({atom, Atoms}, Map) ->
                  lists:foldl(
                    fun(Atom, Acc) ->
                            case maps:find(Atom, Acc) of
                                {ok, atom} ->
                                    Acc;
                                {ok, T} ->
                                    erlang:error({conflicting_types,
                                                  {atom, Acc}, T});
                                error ->
                                    maps:put(Atom, atom, Acc)
                            end
                    end, Map, Atoms);
             ({external, {record, Tag, _}, _, _} = Rec, Map) ->
                  case maps:find(Tag, Map) of
                      {ok, Rec} ->
                          Map;
                      {ok, T} ->
                          erlang:error({conflicting_types, Rec, T});
                      error ->
                          maps:put(Tag, Rec, Map)
                  end
          end, maps:new(), Types),
    Variants =
        lists:map(
          fun({Atom, atom}) ->
                  to_atd_variant(Atom);
             ({Tag, {external, {record, Tag, Arity}, _, _} = Rec}) ->
                  RecName = sanitize_for_atd(Tag),
                  [to_atd_variant(Tag), " of ", RecName]
          end, maps:to_list(TMap)),
    ["[",
     string:join(Variants, " | "),
     "]"].


make_decoding_MFA(Parents, TagName, _TagNS, AttrName,
                  IsRequired, Default, DecMFA, _Types, ModName) ->
    FunName = make_dec_fun_name(Parents),
    Type = case AttrName of
               <<>> -> "cdata";
               _ -> "attr"
           end,
    Clause1 = erl_syntax:clause(
                [?AST(__TopXMLNS),
		 if Type == "attr" -> ?AST(undefined);
                    true -> ?AST(<<>>) end],
                none,
                [if IsRequired ->
			 MissingType = erl_syntax:atom("missing_" ++ Type),
			 ?AST(erlang:error(
				{'?a(ModName)', {'?MissingType', '?a(AttrName)',
						 '?a(TagName)', __TopXMLNS}}));
                    true ->
                         abstract(Default)
                 end]),
    Body = case DecMFA of
               {M, F, Args} ->
                   make_function_call(
                     {M, F},
                     [?AST(_val)|
                      [abstract(Arg) || Arg <- Args]]);
               {F, Args} ->
                   make_function_call(
                     F,
                     [?AST(_val)|
                      [abstract(Arg) || Arg <- Args]]);
               undefined ->
                   ?AST(_val)
           end,
    Catch = case DecMFA of
                undefined ->
                    Body;
                _ ->
		    BadVal = erl_syntax:atom("bad_" ++ Type ++ "_value"),
                    erl_syntax:case_expr(
                      erl_syntax:catch_expr(Body),
                      [erl_syntax:clause(
                         [?AST({'EXIT', _})],
                         none,
                         [?AST(erlang:error(
				 {'?a(ModName)', {'?BadVal', '?a(AttrName)',
						  '?a(TagName)', __TopXMLNS}}))]),
                       erl_syntax:clause([?AST(_res)], none, [?AST(_res)])])
            end,
    Clause2 = erl_syntax:clause(
		[?AST(__TopXMLNS),
		 ?AST(_val)], none, [Catch]),
    [erl_syntax:function(erl_syntax:atom(FunName), [Clause1, Clause2])].

make_encoding_MFA(_, <<"xmlns">>, _, _, _) ->
    [];
make_encoding_MFA(Parents, AttrName, Required, AttrDefault, EncMFA) ->
    Clause1 = if Required ->
                      [];
                 true ->
                      [erl_syntax:clause(
                         [abstract(AttrDefault),
                          ?AST(_acc)],
                         none,
                         [?AST(_acc)])]
              end,
    Body = case EncMFA of
               {M, F, Args} ->
                   make_function_call(
                     {M, F},
                     [?AST(_val)|
                      [abstract(Arg) || Arg <- Args]]);
               {F, Args} ->
                   make_function_call(
                     F,
                     [?AST(_val)|
                      [abstract(Arg) || Arg <- Args]]);
               undefined ->
                   ?AST(_val)
           end,
    Clause2 = [erl_syntax:clause(
                 [?AST(_val),
                  ?AST(_acc)],
                 none,
                 [erl_syntax:list(
                    [erl_syntax:tuple(
                       [if AttrName /= <<>> -> abstract(AttrName);
                           true -> ?AST(xmlcdata)
                        end, Body])],
                    ?AST(_acc))])],
    [erl_syntax:function(
       erl_syntax:atom(make_enc_fun_name(Parents)),
       Clause1 ++ Clause2)].

make_dec_fun_name(Vars) ->
    make_dec_fun_name(undefined, undefined, Vars).

make_dec_fun_name(ParentMod, Mod, Vars) ->
    NewVars = lists:foldl(
                fun(Var, Acc) when is_binary(Var) ->
                        [binary_to_list(Var)|Acc];
                   (Var, Acc) when is_atom(Var) ->
                        [atom_to_list(Var)|Acc];
                   (Var, Acc) ->
                        [Var|Acc]
                end, [], Vars),
    Fun = "decode_" ++ string:join(NewVars, "_"),
    if Mod == undefined -> Fun;
       ParentMod == Mod -> Fun;
       true -> {Mod, Fun}
    end.

make_enc_fun_name(Vars) ->
    make_enc_fun_name(undefined, undefined, Vars).

make_enc_fun_name(ParentMod, Mod, Vars) ->
    NewVars = lists:foldl(
                fun(Var, Acc) when is_binary(Var) ->
                        [binary_to_list(Var)|Acc];
                   (Var, Acc) when is_atom(Var) ->
                        [atom_to_list(Var)|Acc];
                   (Var, Acc) ->
                        [Var|Acc]
                end, [], Vars),
    Fun = "encode_" ++ string:join(NewVars, "_"),
    if Mod == undefined -> Fun;
       ParentMod == Mod -> Fun;
       true -> {Mod, Fun}
    end.

%% Fun(Args) -> Body.
make_function(Fun, Args, Body) ->
    erl_syntax:function(
      erl_syntax:atom(Fun),
      [erl_syntax:clause(Args, none, Body)]).

make_function_call({Mod, Fun}, Args) ->
    erl_syntax:application(
      erl_syntax:atom(Mod),
      erl_syntax:atom(Fun),
      Args);
make_function_call(Fun, Args) ->
    erl_syntax:application(
      none,
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

a(Term) ->
    abstract(Term).

label_to_var(Label) ->
    case atom_to_list(Label) of
        [$$,$_|[H|T]] ->
            erl_syntax:variable(
              replace_invalid_chars(
                [$_, $_, string:to_upper(H)|T]));
        [$$|[H|T]] when H /= $- ->
            erl_syntax:variable(
              replace_invalid_chars(
                [string:to_upper(H)|T]))
    end.

replace_invalid_chars([$-|T]) ->
    [$_|replace_invalid_chars(T)];
replace_invalid_chars([$:|T]) ->
    [$_|replace_invalid_chars(T)];
replace_invalid_chars([$.|T]) ->
    [$_|replace_invalid_chars(T)];
replace_invalid_chars([H|T]) ->
    [H|replace_invalid_chars(T)];
replace_invalid_chars([]) ->
    [].

label_to_record_field(Label) ->
    case atom_to_list(Label) of
        "$_els" ->
            sub_els;
        [$$|T] ->
            list_to_atom(T)
    end.

record_fields_to_vars(Name, Fields) ->
    record_fields_to_vars(Name, Fields, []).

record_fields_to_vars(Name, Fields, Replace) ->
    FList = lists:map(
	      fun(Field) ->
		      case lists:keyfind(Field, 1, Replace) of
			  {_, AbstractVal} ->
			      AbstractVal;
			  false ->
			      erl_syntax:variable(
				[$_|atom_to_list(Field)])
		      end
	      end, Fields),
    erl_syntax:tuple([erl_syntax:atom(Name)|FList]).

prepare_label(Label, Name) when is_atom(Name) ->
    prepare_label(Label, erlang:atom_to_binary(Name, utf8));
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

tuple_or_single_var([Var]) ->
    Var;
tuple_or_single_var([_|_] = Vars) ->
    erl_syntax:tuple(Vars).

labels_to_underscores(Term) ->
    labels_to_underscores(Term, []).

labels_to_underscores(Term, Except) ->
    labels_to_underscores(Term, Except, []).

labels_to_underscores(Term, Except, Replace) ->
    erl_syntax_lib:map(
      fun(T) ->
              try
                  Label = erl_syntax:atom_value(T),
                  true = is_label(Label),
		  case lists:member(Label, Except) of
		      true ->
			  label_to_var(Label);
		      false ->
			  case lists:keyfind(Label, 1, Replace) of
			      {_, Value} ->
				  abstract(Value);
			      false ->
				  ?AST(_)
			  end
		  end
              catch _:_ ->
                      T
              end
      end, abstract(Term)).

have_label(Term, Label) ->
    erl_syntax_lib:fold(
      fun(_, true) ->
              true;
         (T, false) ->
              try
                  L = erl_syntax:atom_value(T),
                  true = is_label(L),
                  Label == L
              catch _:_ ->
                      false
              end
      end, false, abstract(Term)).

get_fun_return_type(Fun, FunSpecs) ->
    get_fun_return_type(Fun, FunSpecs, dict:new()).

get_fun_return_type({dec_enum, [Atoms]}, _, _) ->
    %% TODO: erl_types:t_atoms restricts the number of atoms to 13, this is a
    %% hack to allow any number of atoms
    %erl_types:t_atoms(Atoms);
    {c, atom, lists:usort(Atoms), unknown};
get_fun_return_type({dec_int, [Min, _]}, _, _) ->
    if Min > 0 ->
            erl_types:t_pos_integer();
       Min == 0 ->
            erl_types:t_non_neg_integer();
       Min < 0 ->
            erl_types:t_integer()
    end;
get_fun_return_type({dec_int, []}, _, _) ->
    erl_types:t_integer();
get_fun_return_type({F, Args}, FunSpecs, _) ->
    case dict:find({F, length(Args) + 1}, FunSpecs) of
	{ok, Spec} ->
	    Type = t_from_form(Spec),
	    case erl_types:t_is_any(Type) of
		true -> Spec;
		false -> Type
	    end;
	_ ->
	    erl_types:t_any()
    end;
get_fun_return_type({M, F, Args}, FunSpecs, _) ->
    case dict:find({M, F, length(Args) + 1}, FunSpecs) of
	{ok, Spec} ->
	    Type = t_from_form(Spec),
	    case erl_types:t_is_any(Type) of
		true -> Spec;
		false -> Type
	    end;
	_ ->
	    erl_types:t_any()
    end;
get_fun_return_type(undefined, _, _) ->
    erl_types:t_binary().

term_is_record(Term) ->
    try
        [H|_T]= tuple_to_list(Term),
        true = is_atom(H),
        false == is_label(H)
    catch _:_ ->
            false
    end.

%% This is a copy-paste from erl_types.erl and probably will
%% be broken from time to time in new OTP releases.
t_identifier(Elem) ->
    {c, identifier, ordsets:from_list([Elem]), unknown}.

term_to_t([H|T], LabelTypes) ->
    erl_types:t_cons(term_to_t(H, LabelTypes), term_to_t(T, LabelTypes));
term_to_t([], _LabelTypes) ->
    erl_types:t_nil();
term_to_t(T, LabelTypes) when is_atom(T) ->
    case is_label(T) of
        true ->
            {_, {Type, Default, IsRequired}} = lists:keyfind(T, 1, LabelTypes),
            if IsRequired ->
                    Type;
	       ?is_raw_type(Type) ->
		    Type;
               true ->
                    erl_types:t_sup(Type, erl_types:t_from_term(Default))
            end;
        false ->
            erl_types:t_atom(T)
    end;
term_to_t(T, _LabelTypes) when is_bitstring(T) ->
    erl_types:t_bitstr(0, erlang:bit_size(T));
term_to_t(T, _LabelTypes) when is_float(T) ->
    erl_types:t_float();
term_to_t(T, _LabelTypes) when is_function(T) ->
    {arity, Arity} = erlang:fun_info(T, arity),
    erl_types:t_fun(Arity, erl_types:t_any());
term_to_t(T, _LabelTypes) when is_integer(T) ->
    erl_types:t_integer(T);
term_to_t(T, _LabelTypes) when is_pid(T) ->
    erl_types:t_pid();
term_to_t(T, _LabelTypes) when is_port(T) ->
    erl_types:t_port();
term_to_t(T, _LabelTypes) when is_reference(T) ->
    erl_types:t_reference();
term_to_t(T, LabelTypes) when is_tuple(T) ->
    case term_is_record(T) of
        true ->
            RecName = element(1, T),
            erl_types:t_tuple([term_to_t(RecName, LabelTypes)]);
        false ->
            erl_types:t_tuple(
              [term_to_t(E, LabelTypes) || E <- tuple_to_list(T)])
    end.

is_subtype(_Term, Type) when ?is_raw_type(Type) ->
    false;
is_subtype(Term, Type) ->
    erl_types:t_is_subtype(erl_types:t_from_term(Term), Type).

get_types(TaggedElems, FunSpecs, Opts) ->
    G = build_ref_deps(TaggedElems),
    SortedTags = digraph_utils:topsort(G),
    TypesDict = lists:foldl(
                  fun(RefName, Dict) ->
                          RefElem = get_elem_by_ref(RefName, TaggedElems),
                          Result = RefElem#elem.result,
                          Labels = extract_labels_from_term(Result),
                          LabelTypes =
                              lists:map(
                                fun(Label) ->
                                        {Label, get_label_type(Label, RefElem, Dict, FunSpecs, Opts)}
                                end, Labels),
                          Type = term_to_t(Result, LabelTypes),
                          dict:store(RefName, Type, Dict)
                  end, dict:new(), SortedTags),
    RecDict = dict_from_list(
                lists:flatmap(
                  fun({Tag, _T}) ->
                          RefElem = get_elem_by_ref(Tag, TaggedElems),
                          case term_is_record(RefElem#elem.result) of
                              true ->
                                  RecName = element(1, RefElem#elem.result),
                                  [{{record, RecName}, {0, [{0, []}]}}];
                              false ->
                                  []
                          end
                  end, dict:to_list(TypesDict))),
    {digraph_utils:topsort(G), TypesDict, RecDict}.

extract_labels_from_term(Term) ->
    erl_syntax_lib:fold(
      fun(T, Acc) ->
              try
                  Label = erl_syntax:atom_value(T),
                  true = is_label(Label),
                  [Label|Acc]
              catch _:_ ->
                      Acc
              end
      end, [], abstract(Term)).

get_label_type(Label, Elem, Dict, FunSpecs, Opts) ->
    XMLType = t_remote(fxml, xmlel),
    case get_spec_by_label(Label, Elem) of
        sub_els ->
	    T = case proplists:get_value(add_type_specs, Opts) of
		    SpecName when is_atom(SpecName), SpecName /= undefined ->
			erl_types:t_sup([XMLType, t_identifier(SpecName)]);
		    _ ->
			erl_types:t_any()
		end,
            {erl_types:t_list(T), [], false};
	'_' ->
	    {erl_types:t_from_term(undefined), [], false};
	#attr{dec = undefined, default = Default, required = IsRequired} ->
	    {erl_types:t_binary(), Default, IsRequired};
        #attr{dec = DecFun, default = Default, required = IsRequired} ->
	    {get_fun_return_type(DecFun, FunSpecs), Default, IsRequired};
	#cdata{dec = undefined, default = Default, required = IsRequired} ->
	    {erl_types:t_binary(), Default, IsRequired};
        #cdata{dec = DecFun, default = Default, required = IsRequired} ->
	    {get_fun_return_type(DecFun, FunSpecs), Default, IsRequired};
        [#ref{min = Min, max = Max, default = Default}|_] = Refs ->
            Types = lists:flatmap(
                      fun(#ref{name = RefTag}) ->
                              case dict:find(RefTag, Dict) of
				  {ok, T} -> [T];
				  error -> []
			      end
                      end, Refs),
            Type = erl_types:t_sup(Types),
            IsRequired = (Min == 1) and (Max == 1),
            if Max == 1 ->
                    {Type, Default, IsRequired};
	       ?is_raw_type(Type) ->
		    {{type, element(2, Type), list, [Type]}, [], false};
               true ->
                    {erl_types:t_list(
                       erl_types:t_subtract(
                         Type, erl_types:t_from_term(Default))),
                     [], false}
            end
    end.

build_ref_deps(TaggedElems) ->
    G = digraph:new([acyclic]),
    lists:foreach(
      fun({Tag, Elem}) ->
              digraph:add_vertex(G, Tag),
              lists:foreach(
                fun(#ref{name = RefTag}) ->
                        digraph:add_vertex(G, RefTag),
                        digraph:add_edge(G, RefTag, Tag)
                end, Elem#elem.refs)
      end, TaggedElems),
    G.

get_abstract_code_from_myself() ->
    {file, File} = code:is_loaded(?MODULE),
    case beam_lib:chunks(File, [abstract_code]) of
        {ok, {_, List}} ->
            case lists:keyfind(abstract_code, 1, List) of
                {abstract_code, {raw_abstract_v1, Abstr}} ->
                    {ok, Abstr};
                _ ->
                    error
            end;
        _ ->
            error
    end.

%%====================================================================
%% Auxiliary functions
%%====================================================================
resolver_mod(Mod) ->
    list_to_atom(atom_to_list(Mod) ++ "_external").

%% Checks
prepare_elem(#elem{name = Name}, _, _, _, _, _)
  when not is_binary(Name) ->
    bad_spec({wrong_name, Name});
prepare_elem(#elem{module = Mod}, _, _, _, _, _) when not is_atom(Mod) ->
    bad_spec({wrong_module, Mod});
prepare_elem(#elem{name = Name, xmlns = XMLNS}, _, _, _, _, _)
  when not is_binary(XMLNS), not is_list(XMLNS) ->
    bad_spec({wrong_xmlns, XMLNS, Name});
prepare_elem(#elem{name = Name, refs = Refs}, _, _, _, _, _) when not is_list(Refs) ->
    bad_spec({wrong_refs, Refs, Name});
prepare_elem(#elem{name = Name, attrs = Attrs}, _, _, _, _, _) when not is_list(Attrs) ->
    bad_spec({wrong_attrs, Attrs, Name});
prepare_elem(#elem{name = Name, attrs = Attrs, xmlns = XMLNS,
                   cdata = CData, refs = Refs, module = Mod} = Elem,
             KnownFunctions, FunSpecs, AllElems, ModName, _Opts) ->
    if XMLNS == <<>> ->
            bad_spec({empty_xmlns, Name});
       true ->
            ok
    end,
    NewAttrs = lists:map(
                 fun(Attr) ->
			 prepare_attr(Name, Attr, KnownFunctions, FunSpecs, ModName)
		 end, Attrs),
    NewCData = prepare_cdata(Name, CData, KnownFunctions, FunSpecs, ModName),
    NewRefs = lists:map(
                fun(Ref) -> prepare_ref(Name, Ref, AllElems) end,
                Refs),
    check_labels(Elem),
    NewMod = if Mod == undefined -> ModName;
		true -> Mod
	     end,
    Elem#elem{attrs = NewAttrs, cdata = NewCData, refs = NewRefs, module = NewMod}.

prepare_ref(Name, #ref{name = RefName}, _)
  when not is_atom(RefName) ->
    bad_spec({wrong_ref_name, RefName, Name});
prepare_ref(Name, #ref{name = RefName, min = Min}, _)
  when not (Min == 0 orelse Min == 1) ->
    bad_spec({wrong_ref_min, Min, RefName, Name});
prepare_ref(Name, #ref{name = RefName, max = Max}, _)
  when not (Max == 1 orelse Max == infinity) ->
    bad_spec({wrong_ref_max, Max, RefName, Name});
prepare_ref(Name, #ref{name = RefName, label = Label}, _)
  when not is_atom(Label) ->
    bad_spec({wrong_ref_label, Label, RefName, Name});
prepare_ref(Name, #ref{name = RefName, label = Label} = Ref, AllElems) ->
    case (is_label(Label) or (Label == undefined)) of
        false ->
            bad_spec({wrong_ref_label_format, Label, RefName, Name});
        true ->
            case lists:keyfind(RefName, 1, AllElems) of
                false ->
                    bad_spec({unresolved_ref, RefName, Name});
                _ ->
                    Ref
            end
    end;
prepare_ref(Name, Junk, _) ->
    bad_spec({not_ref_spec, Junk, Name}).

prepare_default('$unset', DecFun, _IsRequired, FunSpecs) ->
    T = get_fun_return_type(DecFun, FunSpecs),
    case erl_types:t_is_binary(T) of
	true -> <<"">>;
	_ -> undefined
    end;
prepare_default(Default, _DecFun, false, _FunSpecs) ->
    Default;
prepare_default(Default, _DecFun, true, _FunSpecs) ->
    bad_spec({default_must_be_unset, Default}).

prepare_attr(Name, #attr{name = AName}, _, _, _)
  when not is_binary(AName) ->
    bad_spec({wrong_attr_name, AName, Name});
prepare_attr(Name, #attr{name = AName, label = Label}, _, _, _)
  when not is_atom(Label) ->
    bad_spec({wrong_attr_label, Label, AName, Name});
prepare_attr(Name, #attr{name = AName, required = Req}, _, _, _)
  when not (Req == false orelse Req == true) ->
    bad_spec({wrong_attr_required, Req, AName, Name});
prepare_attr(Name, #attr{name = AName, label = Label,
			 default = Default, required = IsRequired,
                         dec = DecF, enc = EncF} = Attr,
	     KnownFunctions, FunSpecs, ModName) ->
    NewDefault = prepare_default(Default, DecF, IsRequired, FunSpecs),
    NewDecFun = prep_dec_fun(DecF, KnownFunctions, ModName),
    NewEncFun = prep_enc_fun(EncF, KnownFunctions, ModName),
    case (is_label(Label) or (Label == undefined)) of
        false ->
            bad_spec({wrong_attr_label_format, Label, AName, Name});
        true ->
            Attr#attr{dec = NewDecFun, enc = NewEncFun, default = NewDefault}
    end;
prepare_attr(Name, Junk, _, _, _) ->
    bad_spec({not_attr_spec, Junk, Name}).

prepare_cdata(Name, #cdata{label = Label}, _, _, _)
  when not is_atom(Label) ->
    bad_spec({wrong_cdata_label, Label, Name});
prepare_cdata(Name, #cdata{required = Req}, _, _, _)
  when not (Req == false orelse Req == true) ->
    bad_spec({wrong_cdata_required, Req, Name});
prepare_cdata(Name, #cdata{label = Label, dec = DecF, enc = EncF,
			   default = Default, required = IsRequired} = CData,
                 KnownFunctions, FunSpecs, ModName) ->
    NewDefault = prepare_default(Default, DecF, IsRequired, FunSpecs),
    NewDecFun = prep_dec_fun(DecF, KnownFunctions, ModName),
    NewEncFun = prep_enc_fun(EncF, KnownFunctions, ModName),
    case (is_label(Label) or (Label == undefined)) of
        false ->
            bad_spec({wrong_cdata_label_format, Label, Name});
        true ->
            CData#cdata{enc = NewEncFun, dec = NewDecFun, default = NewDefault}
    end;
prepare_cdata(Name, Junk, _, _, _) ->
    bad_spec({not_cdata_spec, Junk, Name}).

get_dups(L) ->
    get_dups(L, []).

get_dups([H|T], Acc) ->
    case lists:member(H, T) of
        true ->
            get_dups(T, [H|Acc]);
        false ->
            get_dups(T, Acc)
    end;
get_dups([], Acc) ->
    lists:usort(Acc).

check_labels(#elem{result = Result, attrs = Attrs,
                   refs = Refs, cdata = CData}) ->
    ResultLabels = erl_syntax_lib:fold(
                     fun(T, Acc) ->
                             try
                                 Label = erl_syntax:atom_value(T),
                                 true = is_label(Label),
                                 [Label|Acc]
                             catch _:_ ->
                                     Acc
                             end
                     end, [], abstract(Result)),
    AttrLabels = lists:map(
                   fun(#attr{name = Name, label = Label}) ->
                           prepare_label(Label, Name)
                   end, Attrs),
    CDataLabel = CData#cdata.label,
    RefLabels = lists:map(
                  fun({L, Rs}) ->
                          check_group(L, Rs),
                          L
                  end, group_refs(Refs)),
    AllLabels = AttrLabels ++ [CDataLabel] ++ RefLabels,
    case get_dups(lists:filter(
		    fun('$_') -> false;
		       (_) -> true
		    end, ResultLabels)) of
        Dups1 when Dups1 /= [] ->
            bad_spec({duplicated_labels, Dups1});
        _ ->
            ok
    end,
    case get_dups(AllLabels) of
        Dups2 when Dups2 /= [] ->
            bad_spec({duplicated_labels, Dups2});
        _ ->
            ok
    end,
    ResultSet = sets:from_list(ResultLabels),
    AllSet = sets:from_list(AllLabels),
    UnresolvedLabels = sets:to_list(
                         sets:subtract(ResultSet, AllSet)) -- ['$_els', '$_'],
    UnusedLabels = sets:to_list(sets:subtract(AllSet, ResultSet)) -- ['$cdata'],
    if UnresolvedLabels /= [] ->
            bad_spec({unresolved_labels, UnresolvedLabels});
       UnusedLabels /= []->
            bad_spec({unused_labels, UnusedLabels});
       true ->
            ok
    end.

check_group(_Label, [_]) ->
    ok;
check_group(Label, Refs) ->
    #ref{default = Default} = hd(Refs),
    case lists:all(
           fun(#ref{default = D}) ->
                   D == Default
           end, Refs) of
        true ->
            case lists:all(
                   fun(#ref{min = 0, max = 1}) ->
                           true;
                      (_) ->
                           false
                   end, Refs) of
                true ->
                    ok;
                false ->
                    case lists:all(
                           fun(#ref{min = 0, max = infinity}) ->
                                   true;
                              (_) ->
                                   false
                           end, Refs) of
                        true ->
                            ok;
                        false ->
                            bad_spec({wrong_min_max_in_group, Label})
                    end
            end;
        false ->
            bad_spec({different_defaults_in_group, Label})
    end.

prep_dec_fun({Mod, Fun, Args}, _, _)
  when is_atom(Mod) andalso is_atom(Fun) andalso is_list(Args) ->
    {Mod, Fun, Args};
prep_dec_fun({Fun, Args}, KnownFunctions, _ModName)
  when is_atom(Fun) andalso is_list(Args) ->
    Arity = length(Args) + 1,
    case erlang:function_exported(?MODULE, Fun, Arity) of
        true ->
            {Fun, Args};
        false ->
            case lists:member({Fun, Arity}, KnownFunctions) of
                true ->
                    {Fun, Args};
                false ->
                    bad_spec({unknown_dec_fun, {Fun, Args}})
            end
    end;
prep_dec_fun(undefined, _, _) ->
    undefined;
prep_dec_fun(Junk, _, _) ->
    bad_spec({invalid_dec_fun, Junk}).

prep_enc_fun({Mod, Fun, Args}, _, _)
  when is_atom(Mod) andalso is_atom(Fun) andalso is_list(Args) ->
    {Mod, Fun, Args};
prep_enc_fun({Fun, Args}, KnownFunctions, _ModName)
  when is_atom(Fun) andalso is_list(Args) ->
    Arity = length(Args) + 1,
    case erlang:function_exported(?MODULE, Fun, Arity) of
        true ->
            {Fun, Args};
        false ->
            case lists:member({Fun, Arity}, KnownFunctions) of
                true ->
                    {Fun, Args};
                false ->
                    bad_spec({unknown_enc_fun, {Fun, Args}})
            end
    end;
prep_enc_fun(undefined, _, _) ->
    undefined;
prep_enc_fun(Junk, _, _) ->
    bad_spec({invalid_enc_fun, Junk}).

is_label(Label) when not is_atom(Label) ->
    false;
is_label(Label) ->
    case atom_to_list(Label) of
        "$_els" ->
            true;
	"$_" ->
	    true;
        [$$,$_|_] ->
            false;
        [$$,$-|_] ->
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
    case get_forms(Path) of
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
    end.

record_to_tuple(R) ->
    Name = erl_syntax:record_expr_type(R),
    AtomName = list_to_atom(erl_syntax:atom_name(Name)),
    case lists:member(AtomName, [attr, cdata, elem, ref]) of
        true ->
            record_to_tuple(R, tag_record(AtomName));
        false ->
            bad_spec({unknown_record, AtomName})
    end.

tag_record(attr) ->
    tag_record(#attr{}, record_info(fields, attr));
tag_record(cdata) ->
    tag_record(#cdata{}, record_info(fields, cdata));
tag_record(ref) ->
    tag_record(#ref{}, record_info(fields, ref));
tag_record(elem) ->
    tag_record(#elem{}, record_info(fields, elem)).

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
    case get_dups([K || {K, _} <- KeyVals]) of
        Dups when Dups /= [] ->
            bad_spec({duplicated_record_fields, Dups});
        _ ->
            ok
    end,
    case sets:to_list(sets:subtract(
                        sets:from_list([K || {K, _} <- KeyVals]),
                        sets:from_list([K || {K, _} <- TaggedRecord]))) of
        Undef when Undef /= [] ->
            bad_spec({undefined_record_fields, Undef});
        _ ->
            ok
    end,
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

get_forms(Path) ->
    case file:open(Path, [read]) of
        {ok, Fd} ->
            parse(Fd, 1, []);
        Err ->
            Err
    end.

parse(Fd, Line, Acc) ->
    {ok, Pos} = file:position(Fd, cur),
    case epp_dodger:parse_form(Fd, Line) of
        {ok, Form, NewLine} ->
	    case erl_syntax:type(Form) of
		attribute ->
		    {ok, NewPos} = file:position(Fd, cur),
		    {ok, RawForm} = file:pread(Fd, Pos, NewPos - Pos),
		    file:position(Fd, {bof, NewPos}),
		    AnnForm = erl_syntax:set_ann(Form, RawForm),
		    parse(Fd, NewLine, [AnnForm|Acc]);
		_ ->
		    parse(Fd, NewLine, [Form|Acc])
	    end;
        {eof, _} ->
            {ok, lists:reverse(Acc)};
        _Err ->
            file:position(Fd, {bof, Pos}),
            case io:scan_erl_exprs(Fd, "", Line) of
                {ok, Toks, NewLine} ->
                    case transform_spec_to_form(Toks) of
                        {ok, Form} ->
                            parse(Fd, NewLine, [Form|Acc]);
                        not_spec ->
                            parse(Fd, NewLine, Acc);
                        Err ->
                            Err
                    end;
                {eof, _} ->
                    {ok, lists:reverse(Acc)};
                Err ->
                    Err
            end
    end.

transform_spec_to_form([{'-', L0}, {atom, _, 'xml'}, {'(', _}|T]) ->
    case lists:reverse(T) of
        [{dot, L3}, {')', L2}|T1] ->
            T2 = lists:reverse([{dot, L3}, {'}', L2}|T1]),
            case erl_parse:parse_exprs([{'{', L0}|T2]) of
                {ok, Form} ->
                    {ok, Form};
                Err ->
                    Err
            end;
        _ ->
            not_spec
    end;
transform_spec_to_form(_) ->
    not_spec.

-ifdef(HAVE_REMOTE_TYPES).
-ifdef(HAVE_FROM_FORM0).

t_from_form(Spec) ->
    erl_types:t_from_form(Spec).
-else.
t_from_form(Spec) ->
    erl_types:t_from_form(Spec, sets:new(), {spec, foo}, dict:new()).
-endif.

t_remote(Mod, Type) ->
    erl_types:t_remote(Mod, Type, []).
-else.
t_from_form(Spec) ->
    {T, _} = erl_types:t_from_form(
	       Spec, sets:new(), {type, {mod, foo, 1}}, dict:new(),
	       erl_types:var_table__new(), erl_types:cache__new()),
    T.

t_remote(Mod, Type) ->
    D = dict_from_list([{{opaque, Type, []},
			 {{Mod, 1, 2, []}, type}}]),
    [T] = erl_types:t_opaque_from_records(D),
    T.
-endif.

-ifdef(USE_DICT).
dict_from_list(L) ->
    dict:from_list(L).
dict_keys(D) ->
    dict:fetch_keys(D).
-else.
dict_from_list(L) ->
    maps:from_list(L).
dict_keys(D) ->
    maps:keys(D).
-endif.
