This file is used to generate `src/xmlrpc_codec.erl`.

From repository root, you can regenerate the file with:

    erl -noinput +B -pa ebin -pa deps/*/ebin -eval \
          'case xml_gen:compile("spec/xmlrpc_codec.spec") of ok -> halt(0); _ -> halt(1) end.'
