# Erlang XML [![Build Status](https://travis-ci.org/processone/xml.svg?branch=master)](https://travis-ci.org/processone/xml)

Fast Expat based Erlang XML parsing library, with a strong focus on
XML stream parsing from network.

It supports:

- Full XML structure parsing: Suitable for small but complete XML chunks.
- XML stream parsing: Suitable for large XML document, or infinite
  network XML stream like XMPP.

## Building

Erlang XML parser can be build as follow:

    ./configure && make

Erlang XML parser is a rebar-compatible OTP application. Alternatively, you can build it with rebar:

    rebar compile

## Dependencies

Erlang XML parser depends on Expat XML parser. You need development
headers for Expat library to build it.

You can use `configure` options to pass custom path to Expat libraries and headers:

    --with-expat=[ARG]      use Expat XML Parser from given prefix (ARG=path);
                            check standard prefixes (ARG=yes); disable (ARG=no)
    --with-expat-inc=[DIR]  path to Expat XML Parser headers
    --with-expat-lib=[ARG]  link options for Expat XML Parser libraries

## xmlel record and types

XML elements are provided as Erlang xmlel records.

Format of the record allows defining a simple tree-like
structure. xmlel record has the following fields:

- name     :: binary()
- attrs    :: [attr()]
- children :: [xmlel() | cdata()]

cdata type is a tuple of the form:

    {xmlcdata, CData::binary()}

attr type if a tuple of the form:

    {Name::binary(), Value::binary()}

## XML full structure parsing

You can definitely parse a complete XML structure with `p1_xml`:

```
$ erl -pa ebin
Erlang/OTP 17 [erts-6.3] [source] [64-bit] [smp:4:4] [async-threads:10] [hipe] [kernel-poll:false] [dtrace]

Eshell V6.3  (abort with ^G)
1> application:start(p1_xml). 
ok
2> xml_stream:parse_element(<<"<test>content cdata</test>">>).
{xmlel,<<"test">>,[],[{xmlcdata,<<"content cdata">>}]}
```

## XML Stream parsing example

You can also parse continuous stream. Here is an example XML stream parsing:

```
$ erl -pa ebin 
Erlang/OTP 17 [erts-6.3] [source] [64-bit] [smp:4:4] [async-threads:10] [hipe] [kernel-poll:false] [dtrace]

Eshell V6.3  (abort with ^G)

% Start the application:
1> application:start(p1_xml). 
ok

% Create a new stream, using self PID to received XML parsing event:
2> S1 = xml_stream:new(self()).
<<>>

% Start feeding content to the XML parser.
3> S2 = xml_stream:parse(S1, <<"<root>">>).
<<>>

% Receive Erlang message send to shell process:
4> flush().
Shell got {'$gen_event',{xmlstreamstart,<<"root">>,[]}}
ok

% Feed more content:
5> S3 = xml_stream:parse(S2, <<"<xmlelement>content cdata</xmlelement">>).
<<>>

% Receive more messages:
6> flush().
Shell got {'$gen_event',
              {xmlstreamelement,
                  {xmlel,<<"xmlelement">>,[],
                      [{xmlcdata,<<"content cdata">>}]}}}
ok

% Feed more content:
7> S4 = xml_stream:parse(S3, <<"</root>">>).      
<<>>

% Receive messages:
8> flush().
Shell got {'$gen_event',{xmlstreamend,<<"root">>}}
ok

9> xml_stream:close(S4).
true
```

Note how the root element is important. We expect to have the root
element serve as boundary with stream start and stream end
event. Then, lower level tags are passed as sub stream elements.

## How does this module relate to exmpp ?

This module is a low level fast XML parser. It is not an XMPP client
library like [exmpp](https://processone.github.io/exmpp/).

## References

This module is use at large scale for parsing massive XML content in
[ejabberd](https://www.ejabberd.im) XMPP server project. It is used in
production in thousands of real life deployments.
