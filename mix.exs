# Copyright (C) 2002-2017 ProcessOne, SARL. All Rights Reserved.
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
      version: "1.1.12",
      elixir: "~> 1.2",
      compilers: [:fastXML | Mix.compilers],
      aliases: aliases,
      deps: deps,
      package: package
    ]
  end

  def application do
    [ mod: {:fast_xml, []},
      applications: [:p1_utils]
    ]
  end

  defp package do
    [# These are the default files included in the package
     files: ["src", "lib", "c_src/*.c", "mix.exs", "rebar.config", "rebar.config.script", "Makefile.mix", "priv", "include", "README.md", "LICENSE.txt"],
     maintainers: ["ProcessOne"],
     licenses: ["Apache 2.0"],
     links: %{"GitHub" => "https://github.com/processone/fast_xml",
              "Docs" => "https://github.com/processone/fast_xml/blob/master/README.md"}]
  end

  def deps do
    [{:p1_utils, "~> 1.0"},
     {:earmark, "~> 0.1", only: :dev},
     {:ex_doc, "~> 0.11", only: :dev},
     {:eqc_ex, "~> 1.2", only: :test}]
  end

  defp aliases do
    # Execute the usual mix clean and our Makefile clean task
    [clean: ["clean", "clean.fastXML"]]
  end
end

# Define
defmodule Mix.Tasks.Compile.FastXML do
  @shortdoc "Compiles helper in c_src"

  # TODO refactor
  def run(_) do
    if match? {:win32, _}, :os.type do
      {result, _error_code} = System.cmd("nmake", ["/F", "Makefile.mix", "priv\\lib\\fxml_stream.dll"], stderr_to_stdout: true)
      Mix.shell.info result
      {result, _error_code} = System.cmd("nmake", ["/F", "Makefile.mix", "priv\\lib\\fxml.dll"], stderr_to_stdout: true)
      Mix.shell.info result
    else
      {result, _error_code} = System.cmd("make", ["-f", "Makefile.mix", "priv/lib/fxml_stream.so"], stderr_to_stdout: true)
      Mix.shell.info result
      {result, _error_code} = System.cmd("make", ["-f", "Makefile.mix", "priv/lib/fxml.so"], stderr_to_stdout: true)
      Mix.shell.info result
    end
    Mix.Project.build_structure
  end
end

defmodule Mix.Tasks.Clean.FastXML do
  @shortdoc "Cleans helper in c_src"

  def run(_) do
    {result, _error_code} = System.cmd("make", ["-f", "Makefile.mix", "clean"], stderr_to_stdout: true)
    Mix.shell.info result

    :ok
  end
end
