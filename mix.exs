# Copyright (C) 2002-2021 ProcessOne, SARL. All Rights Reserved.
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

defmodule FastXML.Mixfile do
  use Mix.Project

  def project do
    [ app: :fast_xml,
      description: "Fast Expat-based Erlang / Elixir XML parsing library",
      version: "1.1.28",
      elixir: "~> 1.4",
      compilers: [:elixir_make | Mix.compilers],
      make_makefile: "Makefile",
      deps: deps(),
      package: package()
    ]
  end

  def application do
    [ mod: {:fast_xml, []},
      applications: [:p1_utils]
    ]
  end

  defp package do
    [# These are the default files included in the package
     files: ["src", "lib", "c_src/*.c", "mix.exs", "rebar.config", "rebar.config.script", "Makefile", "Makefile.mix", "priv", "include", "README.md", "LICENSE.txt"],
     maintainers: ["ProcessOne"],
     licenses: ["Apache 2.0"],
     links: %{"GitHub" => "https://github.com/processone/fast_xml",
              "Docs" => "https://github.com/processone/fast_xml/blob/master/README.md"}]
  end

  def deps do
    [{:p1_utils, "~> 1.0"},
     {:elixir_make, "~> 0.4", runtime: false},
     {:earmark, "~> 0.1", only: :dev},
     {:ex_doc, "~> 0.11", only: :dev},
     {:eqc_ex, "~> 1.2", only: :test}]
  end
end
