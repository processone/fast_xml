defmodule FastXMLTest do
  use ExUnit.Case

  # TODO: we should be able to pass :use_maps option to :fxml_stream.parse_element/2
  
  test "Stream parser can return Elixir structs" do
    s1 = :fxml_stream.new(self, :infinity, [:no_gen_server, :use_maps])
    s2 = :fxml_stream.parse(s1, "<root>")
    assert receive_result == %FastXML.StreamStart{name: "root"}
    s3 = :fxml_stream.parse(s2, "<xmlelement>content cdata</xmlelement>")
    assert receive_result == %FastXML.El{name: "xmlelement", children: ["content cdata"]}
    s4 = :fxml_stream.parse(s3, "</root>")
    assert receive_result == %FastXML.StreamEnd{name: "root"}
    :fxml_stream.close(s4)
  end

  # TODO This test is failing at the moment
#  test "Stream parser can support arbitrary root element attributes" do
#    s1 = :fxml_stream.new(self, :infinity, [:no_gen_server, :use_maps])
#    s2 = :fxml_stream.parse(s1, "<root xmlns='myns'>")
#    assert receive_result == %FastXML.StreamStart{name: "root", attrs: %{"xmlns": "myns"}}
#    :fxml_stream.close(s2)
#  end
  
# TODO to test, unbound prefix error:
#     code: receive_result == %FastXML.StreamStart{name: "stream:stream", attrs: %{xmlns: "jabber:client"}}
#     lhs:  %{__struct__: FastXML.StreamError, desc: {27, "unbound prefix"}}
#     rhs:  %FastXML.StreamStart{attrs: %{xmlns: "jabber:client"}, name: "stream:stream"}
  
  defp receive_result do
    receive do
      result ->
        result
    after 2000 ->
        nil
    end
  end
end
 
