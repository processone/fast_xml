This file is used to generate `src/fxmlrpc_codec.erl`.

From repository root, you can regenerate the file with:

    erl -noinput +B -pa ebin -pa deps/*/ebin -eval \
          'case fxml_gen:compile("spec/fxmlrpc_codec.spec") of ok -> halt(0); _ -> halt(1) end.'
