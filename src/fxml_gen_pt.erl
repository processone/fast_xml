%%%-------------------------------------------------------------------
%%% @author Evgeny Khramtsov <ekhramtsov@process-one.net>
%%% @copyright (C) 2016-2022, Evgeny Khramtsov
%%% @doc
%%%
%%% @end
%%% Created : 27 May 2016 by Evgeny Khramtsov <ekhramtsov@process-one.net>
%%%-------------------------------------------------------------------
-module(fxml_gen_pt).

%% API
-export([parse_transform/2]).

-include("fxml_gen.hrl").

%%%===================================================================
%%% API
%%%===================================================================
parse_transform(Forms, _Options) ->
    Result = lists:map(
	       fun(Form) ->
		       try
			   Form2 = erl_syntax_lib:map(
				     fun(Node) ->
					     transform(Node)
				     end, Form),
			   Form3 = erl_syntax:revert(Form2),
			   %%io:format("~s~n", [erl_prettypr:format(Form3)]),
			   Form3
		       catch
			   throw:{error, Line, Error} ->
			       {error, {Line, erl_parse, Error}}
		       end
	       end, Forms),
    Result.

%%%===================================================================
%%% Internal functions
%%%===================================================================
transform(Form) ->
    case erl_syntax:type(Form) of
        application ->
            case erl_syntax_lib:analyze_application(Form) of
                {?AST_MARK, 1} ->
		    [Tree] = erl_syntax:application_arguments(Form),
		    NewTree = erl_syntax_lib:map(
				fun(Node) ->
					transform_variable(Node)
				end, erl_syntax:abstract(Tree)),
		    erl_syntax:revert(NewTree);
		_ ->
		    Form
	    end;
	_ ->
	    Form
    end.

transform_variable(Form) ->
    try
	Term = erl_syntax:concrete(Form),
	atom = erl_syntax:type(Term),
	"?" ++ Var = erl_syntax:atom_name(Term),
	{ok, Tokens, _} = erl_scan:string(Var ++ "."),
	{ok, [NewForm]} = erl_parse:parse_exprs(Tokens),
	NewForm
    catch _:_ ->
	    Form
    end.
