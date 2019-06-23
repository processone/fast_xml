-xml(methodCall,
     #elem{name = <<"methodCall">>,
	   xmlns = <<"xmlrpc">>,
	   result = {call, '$name', '$params'},
	   refs = [#ref{name = methodName,
			label = '$name',
			min = 1, max = 1},
		   #ref{name = params,
			label = '$params',
			default = [],
			min = 0, max = 1}]}).

-xml(methodResponse,
     #elem{name = <<"methodResponse">>,
	   xmlns = <<"xmlrpc">>,
	   result = {response, '$payload'},
	   refs = [#ref{name = fault, label = '$payload', default = [],
			min = 0, max = 1},
		   #ref{name = params, label = '$payload', default = [],
			min = 0, max = 1}]}).

-xml(fault,
     #elem{name = <<"fault">>,
	   xmlns = <<"xmlrpc">>,
	   result = {fault, '$value'},
	   refs = [#ref{name = value, label = '$value', min = 1, max = 1}]}).

-xml(methodName,
     #elem{name = <<"methodName">>,
	   xmlns = <<"xmlrpc">>,
	   result = '$cdata',
	   cdata = #cdata{required = true,
			  dec = {erlang, binary_to_atom, [utf8]},
			  enc = {erlang, atom_to_binary, [utf8]}}}).

-xml(params,
     #elem{name = <<"params">>,
	   xmlns = <<"xmlrpc">>,
	   result = '$params',
	   refs = [#ref{name = param, label = '$params'}]}).

-xml(param,
     #elem{name = <<"param">>,
	   xmlns = <<"xmlrpc">>,
	   result = '$value',
	   refs = [#ref{name = value, label = '$value',
			min = 1, max = 1}]}).

-xml(value,
     #elem{name = <<"value">>,
	   xmlns = <<"xmlrpc">>,
	   result = {'$val', '$cdata'},
	   refs = [#ref{name = i4, label = '$val', min = 0, max = 1},
		   #ref{name = int, label = '$val', min = 0, max = 1},
		   #ref{name = string, label = '$val', min = 0, max = 1},
		   #ref{name = double, label = '$val', min = 0, max = 1},
		   #ref{name = base64, label = '$val', min = 0, max = 1},
		   #ref{name = boolean, label = '$val', min = 0, max = 1},
		   #ref{name = array, label = '$val', min = 0, max = 1},
		   #ref{name = nil, label = '$val', min = 0, max = 1},
		   #ref{name = struct, label = '$val', min = 0, max = 1},
		   #ref{name = dateTime, label = '$val', min = 0, max = 1}],
	   cdata = #cdata{default = undefined}}).

-xml(i4,
     #elem{name = <<"i4">>,
	   xmlns = <<"xmlrpc">>,
	   result = {i4, '$cdata'},
	   cdata = #cdata{required = true,
			  dec = {erlang, binary_to_integer, []},
			  enc = {erlang, integer_to_binary, []}}}).

-xml(int,
     #elem{name = <<"int">>,
	   xmlns = <<"xmlrpc">>,
	   result = {int, '$cdata'},
	   cdata = #cdata{required = true,
			  dec = {erlang, binary_to_integer, []},
			  enc = {erlang, integer_to_binary, []}}}).

-xml(string,
     #elem{name = <<"string">>,
	   xmlns = <<"xmlrpc">>,
	   cdata = #cdata{default = <<"">>},
	   result = {string, '$cdata'}}).

-xml(double,
     #elem{name = <<"double">>,
	   xmlns = <<"xmlrpc">>,
	   result = {double, '$cdata'},
	   cdata = #cdata{required = true,
			  dec = {erlang, binary_to_float, []},
			  enc = {erlang, float_to_binary, []}}}).

-xml(base64,
     #elem{name = <<"base64">>,
	   xmlns = <<"xmlrpc">>,
	   result = {base64, '$cdata'},
	   cdata = #cdata{required = true}}).

-xml(dateTime,
     #elem{name = <<"dateTime.iso8601">>,
	   xmlns = <<"xmlrpc">>,
	   result = {date, '$cdata'},
	   cdata = #cdata{required = true}}).

-xml(boolean,
     #elem{name = <<"boolean">>,
	   xmlns = <<"xmlrpc">>,
	   result = {boolean, '$cdata'},
	   cdata = #cdata{required = true,
			  dec = {dec_bool, []},
			  enc = {enc_bool, []}}}).

-xml(array,
     #elem{name = <<"array">>,
	   xmlns = <<"xmlrpc">>,
	   result = {array, '$data'},
	   refs = [#ref{name = data, label = '$data', min = 1, max = 1}]}).

-xml(data,
     #elem{name = <<"data">>,
	   xmlns = <<"xmlrpc">>,
	   result = '$v',
	   refs = [#ref{name = value, label = '$v'}]}).

-xml(nil,
     #elem{name = <<"nil">>,
	   xmlns = <<"xmlrpc">>,
	   result = nil}).

-xml(struct,
     #elem{name = <<"struct">>,
	   xmlns = <<"xmlrpc">>,
	   result = {struct, '$members'},
	   refs = [#ref{name = member, label = '$members'}]}).

-xml(member,
     #elem{name = <<"member">>,
	   xmlns = <<"xmlrpc">>,
	   result = {'$name', '$value'},
	   refs = [#ref{name = name, label = '$name', min = 1, max = 1},
		   #ref{name = value, label = '$value', min = 1, max = 1}]}).

-xml(name,
     #elem{name = <<"name">>,
	   xmlns = <<"xmlrpc">>,
	   result = '$cdata',
	   cdata = #cdata{required = true,
			  dec = {erlang, binary_to_atom, [utf8]},
			  enc = {erlang, atom_to_binary, [utf8]}}}).

dec_bool(<<"false">>) -> false;
dec_bool(<<"0">>) -> false;
dec_bool(<<"true">>) -> true;
dec_bool(<<"1">>) -> true.

enc_bool(false) -> <<"0">>;
enc_bool(true) -> <<"1">>.

%% Local Variables:
%% mode: erlang
%% End:
%% vim: set filetype=erlang tabstop=8:
