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

-record(spec, {name,
	       label,
	       xmlns = <<"">>,
	       min = 0,
	       max = infinity,
	       default,
	       cdata = #cdata{},
	       attrs = [],
	       result,
               dec_f,
               enc_f,
	       els = []}).
