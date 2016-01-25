defmodule FastXML do
  require Record

  @doc """
xml element record, composed of fields: name, attrs, children
"""
  Record.defrecord :xmlel, Record.extract(:xmlel, from_lib: "fast_xml/include/fxml.hrl")

  @type xmlel :: record(:xmlel, name: String.t, attrs: [attr], children: [content])
  @type content :: xmlel | cdata
  @type cdata :: {:xmlcdata, String.t}
  @type attr  :: {attribute::String.t, value::String.t}

  Record.defrecord :xmlstreamstart, name: "", attrs: []
  @type xmlstreamstart :: record(:xmlstreamstart, name: String.t, attrs: [attr])

  Record.defrecord :xmlstreamend, name: ""
  @type xmlstreamend :: record(:xmlstreamend, name: String.t)  

  Record.defrecord :xmlstreamerror, error: {0, ""}
  @type xmlstreamerror ::record(:xmlstreamerror, error: error)
  @type error :: {code::Integer, description::String.t}
  
  def start, do: :application.start(:fast_xml)
  def stop,  do: :application.stop(:fast_xml)

end
