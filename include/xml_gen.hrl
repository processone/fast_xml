-record(attr, {name,
	       label,
	       required = false,
	       default = <<"">>,
	       dec,
	       enc}).

-record(cdata, {required = false,
		label,
		default = <<"">>,
		dec,
		enc}).

-record(spec, {name,
	       label,
	       xmlns = <<"">>,
	       min = 0,
	       max = unlimited,
	       default = <<"">>,
	       cdata = #cdata{},
	       attrs = [],
	       result,
	       els = []}).
