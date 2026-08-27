-xml(ordered,
     #elem{name = <<"ordered">>,
	   xmlns = <<"urn:test:reference-order">>,
	   result = {ordered, '$mechanisms', '$inline'},
	   refs = [#ref{name = mechanism,
			label = '$mechanisms'},
		   #ref{name = inline,
			label = '$inline',
			min = 0, max = 1}]}).

-xml(mechanism,
     #elem{name = <<"mechanism">>,
	   xmlns = <<"urn:test:reference-order">>,
	   result = {mechanism, '$cdata'},
	   cdata = #cdata{required = true}}).

-xml(inline,
     #elem{name = <<"inline">>,
	   xmlns = <<"urn:test:reference-order">>,
	   result = {inline}}).
