-record(attr, {name,
	       label,
	       required = false,
	       default,
	       dec,
	       enc}).

-record(cdata, {required = false,
		label = '$cdata',
		default,
		dec,
		enc}).

-record(elem, {name,
               xmlns = <<"">>,
               cdata = #cdata{},
               result,
               attrs = [],
               refs = []}).

-record(ref, {name,
              label,
              min = 0,
              max = infinity,
              default}).
