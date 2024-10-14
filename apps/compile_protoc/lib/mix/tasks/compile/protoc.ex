defmodule Mix.Tasks.Compile.Protoc do
  @moduledoc "I compile the protobuf files before compiling the project."
  @shortdoc "Compiles protobuf files"

  use Mix.Task.Compiler

  def run(_args) do
    # parse the options from mix.exs
    compiler_opts =
      Keyword.validate!(protoc_options(), [
        :elixir_out,
        :proto_files,
        :extra_opts
      ])

    # find the protoc executable
    protoc_bin = protoc_executable()

    # make the output directory
    File.mkdir_p!(compiler_opts[:elixir_out])

    # run the protoc command
    {_, return_code} =
      System.cmd(protoc_bin, build_arguments(compiler_opts),
        into: IO.stream(:stdio, :line),
        stderr_to_stdout: true
      )

    if return_code == 0 do
      Mix.shell().info("Compiled protobuf files")
      :ok
    else
      {:error, return_code}
    end
  end

  # @doc """
  # I find the protoc executable in the current system, or raise if its not found.
  # """
  defp protoc_executable() do
    System.find_executable("protoc") ||
      raise "`protoc` not found in system path"
  end

  # @doc """
  # I fetch the options for this task from the mix config.
  # """
  defp protoc_options() do
    config = Mix.Project.config()
    Keyword.get(config, :protoc_options, [])
  end

  defp build_arguments(compiler_opts) do
    extra_opts = Keyword.get(compiler_opts, :extra_opts, "")
    elixir_out = Keyword.fetch!(compiler_opts, :elixir_out)
    proto_files = Keyword.fetch!(compiler_opts, :proto_files)
    elixir_out = "--elixir_out=#{extra_opts}:#{elixir_out}"

    [elixir_out | proto_files]
  end
end
