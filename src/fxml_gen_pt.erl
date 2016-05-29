%%%-------------------------------------------------------------------
%%% @author Evgeny Khramtsov <ekhramtsov@process-one.net>
%%% @copyright (C) 2016, Evgeny Khramtsov
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
    lists:map(
      fun(Form) ->
              try
                  Form2 = erl_syntax_lib:map(
                            fun(Node) ->
                                    transform(Node)
                            end, Form),
                  Form3 = erl_syntax:revert(Form2),
                  Form3
	      catch
		  throw:{error, Line, Error} ->
		      {error, {Line, erl_parse, Error}}
	      end
      end, Forms).

%%%===================================================================
%%% Internal functions
%%%===================================================================
transform(Form) ->
    case erl_syntax:type(Form) of
        application ->
            case erl_syntax_lib:analyze_application(Form) of
                {?AST_MARK, 1} ->
		    [Tree] = erl_syntax:application_arguments(Form),
		    erl_syntax:abstract(Tree);
		_ ->
		    Form
	    end;
	_ ->
	    Form
    end.
