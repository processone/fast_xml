# xml

Fast Expat based Erlang XML parsing library, with a strong focus on XML stream parsing from network.

Note: This module obsolete and supercede exmpp.

XML streaming parser example:

```
$ erl -pa ebin 
Erlang/OTP 17 [erts-6.3] [source] [64-bit] [smp:4:4] [async-threads:10] [hipe] [kernel-poll:false] [dtrace]

Eshell V6.3  (abort with ^G)

% Start the application:
1> application:start(p1_xml). 
ok

% Create a new stream, using self PID to received XML parsing event:
2> S1 = xml_stream:new(self()).
{xml_stream_state,<0.32.0>,#Port<0.751>,[],0,infinity}

% Start feeding content to the XML parser.
3> S2 = xml_stream:parse(S1, <<"<test>">>).
{xml_stream_state,<0.32.0>,#Port<0.751>,
                  [xmlstreamstart],
                  0,infinity}

% Receive Erlang message send to shell process:
4> flush().
Shell got {'$gen_event',{xmlstreamstart,<<"test">>,[]}}
ok

% Feed more content:
5> S3 = xml_stream:parse(S2, <<"content cdata">>).
{xml_stream_state,<0.32.0>,#Port<0.751>,
                  [xmlstreamstart],
                  0,infinity}

% Receive more messages:
6> flush().                                       
Shell got {'$gen_all_state_event',{xmlstreamcdata,<<"content cdata">>}}
ok

% Feed more content:
7> S4 = xml_stream:parse(S3, <<"</test>">>).      
{xml_stream_state,<0.32.0>,#Port<0.751>,[],7,infinity}

% Receive messages:
8> flush().
Shell got {'$gen_event',{xmlstreamend,<<"test">>}}
ok
```

