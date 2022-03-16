#
# Test XML module with Quviq Quickcheck
#
# Copyright (C) 2002-2022 ProcessOne, SARL. All Rights Reserved.
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

import :erlang, only: [list_to_binary: 1]

defmodule XmlTest do
  use ExUnit.Case
  use EQC.ExUnit
  require Record

  Record.defrecord :xmlel, Record.extract(:xmlel, from: "include/fxml.hrl")
  
  property "Can serialize arbitrary XML packets" do
    forall xml_chunk <- xml_el do
      is_binary(:fxml.element_to_binary(xml_chunk))
    end
  end
  
  property "Serialize and parse same XML packet" do
    forall xml_chunk <- xml_el do
      :fxml_stream.parse_element(:fxml.element_to_binary(xml_chunk)) == xml_chunk
    end
  end

  # Generators
  # ==========
  #
  # Random small XML packets generator
  # ----------------------------------

  # TODO: We need to weight lowercase char much more that the rest
  # An XML name cannot start with number of punctuation char
  def xml_name do
    [letter|list(letter_figure)]
  end

  def letter do
    oneof([choose(?A, ?Z), choose(?a, ?z)])
  end
  
  def letter_figure do
    oneof([choose(?A, ?Z),choose(?a, ?z), choose(?0, ?9)])
  end
  
  def xml_attr do
    let {key, val} <- {xml_name, xml_name} do
      {list_to_binary(key), list_to_binary(val)}
    end
  end

  def xml_children(0) do
    []
  end
  
  def xml_children(size) do
    let name <- xml_name do
      oneof([xml_children(0),
             oneof([[{:xmlcdata, list_to_binary(name)}],
                     list(xml_child(div size, 3))])])
    end
  end
  
  def xml_child(size) do
    let {name, attrs} <- {xml_name, list(xml_attr)} do
      xmlel(name: list_to_binary(name),
            attrs: :lists.ukeysort(1, attrs),
            children: xml_children(size))                   
    end
  end
            
  def xml_el do
    sized size do
      let {tagname, attrs} <- {xml_name, list(xml_attr)} do
        xmlel(name: list_to_binary(tagname),
              attrs: :lists.ukeysort(1, attrs),
              children: xml_children(size))
      end
    end
  end
  
end
