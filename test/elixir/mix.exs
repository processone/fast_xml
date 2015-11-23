# Copyright (C) 2002-2015 ProcessOne, SARL. All Rights Reserved.
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

# This mix module is used to configure project to run Elixir tests
# Elixir test can be run from project root using command:
#     make exunit

defmodule FastXml.Mixfile do
  use Mix.Project

  def project do
    [app: :fast_xml,
     version: "1.1.1",
     elixir: "~> 1.1",
     build_embedded: Mix.env == :prod,
     start_permanent: Mix.env == :prod,
     test_paths: ["test/elixir"],
     deps: deps]
  end

  def application do
    [applications: [:logger],
     mod: {:fxml_app, []}]
  end
  
  defp deps do
    [{:p1_utils,
      ~r//,    # project is not semantically versioned                          
      github:  "processone/p1_utils",
      compile: "rebar compile"
     },
     {:eqc_ex, "~> 1.2.3"}]
  end
end
