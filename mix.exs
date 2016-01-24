defmodule FastXML.Mixfile do
  use Mix.Project

  def project do
    [app: :fast_xml,
     version: "1.1.3",
     elixir: "~> 1.1",
     compilers: [:make, :elixir, :erlang, :app],
     aliases: aliases,
     deps: deps]
  end

  def deps do
    []
  end

  defp aliases do
    # Execute the usual mix clean and our Makefile clean task
    [clean: ["clean", "clean.make"]]
  end
end

# Define 
defmodule Mix.Tasks.Compile.Make do
  @shortdoc "Compiles helper in c_src"
 
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
