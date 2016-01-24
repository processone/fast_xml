defmodule FastXML do
  require Record

  @doc """
xml element record, composed of fields: name, attrs, children
"""
  Record.defrecord :xmlel, Record.extract(:xmlel, from_lib: "fast_xml/include/fxml.hrl")
  
  @type xmlel :: record(:xmlel, name: String.t, attrs: [attr], children: [content])
  @type content :: xmlel | cdata
  @type cdata :: {:xmlcdata, String.t}
  @type attr  :: {String.t, String.t}

  def start do
    :application.start(:fast_xml)
  end
end
