defmodule FastXML.Mixfile do
  use Mix.Project

  def project do
    [ app: :fast_xml,
      description: "Fast Expat-based Erlang / Elixir XML parsing library",
      version: "1.1.3",
      elixir: "~> 1.1",
      compilers: [:make, :elixir, :erlang, :app],
      aliases: aliases,
      deps: deps,
      package: package
    ]
  end

  def application do
    [mod: {:fast_xml, []}]
    end
     
  defp package do
    [# These are the default files included in the package
     files: ["src", "lib", "c_src/*.c", "mix.exs", "rebar.config", "rebar.config.script", "Makefile.mix", "include", "README.md", "LICENSE.txt"],
     maintainers: ["ProcessOne"],
     licenses: ["Apache 2.0"],
     links: %{"GitHub" => "https://github.com/processone/fast_xml",
              "Docs" => "https://github.com/processone/fast_xml/blob/master/README.md"}]
  end
     
  def deps do
    [{:p1_utils, "~> 1.0"}]
  end

  defp aliases do
    # Execute the usual mix clean and our Makefile clean task
    [clean: ["clean", "clean.make"]]
  end
end

# Define 
defmodule Mix.Tasks.Compile.Make do
  @shortdoc "Compiles helper in c_src"

  # TODO reformat 
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
    :ok
  end
end
 
defmodule Mix.Tasks.Clean.Make do
  @shortdoc "Cleans helper in c_src"
  
  def run(_) do
    {result, _error_code} = System.cmd("make", ["-f", "Makefile.mix", "clean"], stderr_to_stdout: true)
    Mix.shell.info result
    
    :ok
  end
end
