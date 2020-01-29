# Copyright (C) 2002-2020 ProcessOne, SARL. All Rights Reserved.
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

defmodule FastXMLTest do
  use ExUnit.Case

  # TODO: we should be able to pass :use_maps option to :fxml_stream.parse_element/2
  
  test "Stream parser can return Elixir structs" do
    s1 = :fxml_stream.new(self, :infinity, [:no_gen_server, :use_maps])
    s2 = :fxml_stream.parse(s1, "<root>")
    assert receive_stanza == %FastXML.StreamStart{name: "root"}
    s3 = :fxml_stream.parse(s2, "<xmlelement>content cdata</xmlelement>")
    assert receive_stanza == %FastXML.El{name: "xmlelement", children: ["content cdata"]}
    s4 = :fxml_stream.parse(s3, "<xmlelement><empty/><subelement attribute='true'>content cdata</subelement></xmlelement>")
    assert receive_stanza == %FastXML.El{name: "xmlelement", children: ["content cdata"]}
    s5 = :fxml_stream.parse(s4, "</root>")
    assert receive_stanza == %FastXML.StreamEnd{name: "root"}
    :fxml_stream.close(s5)
  end

  test "Size of parsed stanza can be limited" do
    :todo
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

# TODO test mismatched tags
  
  defp receive_stanza do
    receive do
      result ->
        result
    after 2000 ->
        nil
    end
  end
end
