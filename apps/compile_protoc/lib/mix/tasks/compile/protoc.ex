defmodule Mix.Tasks.Compile.Protoc do
  @moduledoc "I compile the protobuf files before compiling the project."
  @shortdoc "Compiles protobuf files"

  use Mix.Task.Compiler

  @impl true
  def run(args) do
    forced? = args == ["--force"]

    # parse the options from mix.exs
    compiler_opts =
      Keyword.validate!(protoc_options(), [
        :elixir_out,
        :proto_files,
        :extra_opts
      ])

    source_path = compiler_opts[:proto_files]
    target_path = compiler_opts[:elixir_out]

    if forced? or source_updated?(source_path, target_path) do
      Mix.shell().info("Compling protobuf files")

      cleanup_targets(target_path)

      run_compiler(compiler_opts)

      :ok
    else
      Mix.shell().info("No changes to protobuf files")
      :ok
    end
  end

  # @doc """
  # I run the protoc compiler to generate the output files.
  # """
  @spec run_compiler(Keyword.t()) :: term()
  defp run_compiler(compiler_opts) do
    protoc_bin = protoc_executable()

    System.cmd(protoc_bin, build_arguments(compiler_opts),
      into: IO.stream(:stdio, :line),
      stderr_to_stdout: true
    )
  end

  # @doc """
  # I check if any file in the source is newer than the oldest file in the targets.
  # If that is the case, the target files are outdated.
  # If all source files are older than the oldest target file, the source is not updated.
  # """
  @spec source_updated?(String.t(), String.t()) :: boolean
  defp source_updated?(source_path, target_path) do
    source_files = list_files(Path.join(source_path, "/**/*.proto"))
    target_files = list_files(Path.join(target_path, "/**/*.pb.ex"))

    case {newest_mtime(source_files), oldest_mtime(target_files)} do
      {nil, _} -> false
      {_, nil} -> true
      {source_mtime, target_mtime} -> source_mtime > target_mtime
    end
  end

  # @doc """
  # I remove all files in the target directory.
  # """
  @spec cleanup_targets(String.t()) :: :ok
  defp cleanup_targets(target_path) do
    # cleanup old files
    if File.exists?(target_path) do
      Path.join(target_path, "/*")
      |> Path.wildcard()
      |> Enum.each(&File.rm_rf!(&1))
    end

    :ok
  end

  # @doc """
  # I find the protoc executable in the current system, or raise if its not found.
  # """
  @spec protoc_executable() :: String.t()
  defp protoc_executable() do
    System.find_executable("protoc") ||
      raise "`protoc` not found in system path"
  end

  # @doc """
  # I fetch the options for this task from the mix config.
  # """
  @spec protoc_options() :: Keyword.t()
  defp protoc_options() do
    config = Mix.Project.config()
    Keyword.get(config, :protoc_options, [])
  end

  # @doc """
  # I build the arguments for the protoc compiler based on the compiler options.
  # """
  @spec build_arguments(Keyword.t()) :: [String.t()]
  defp build_arguments(opts) do
    extra_opts = Keyword.get(opts, :extra_opts, "")
    elixir_out = Keyword.fetch!(opts, :elixir_out)
    proto_files = list_files(Path.join(opts[:proto_files], "/**/*.proto"))
    elixir_out = "--elixir_out=#{extra_opts}:#{elixir_out}"
    import_path = "--proto_path=#{Keyword.fetch!(opts, :proto_files)}"

    [elixir_out, import_path | proto_files]
  end

  # @doc """
  # I return the oldest mtime for the given list of files.
  # I return nil if the list is empty.
  # """
  @spec oldest_mtime([String.t()]) ::
          [:calendar.datetime() | integer() | :undefined] | nil
  defp oldest_mtime([]), do: nil

  defp oldest_mtime(file_list) do
    Enum.min(mtimes(file_list))
  end

  # @doc """
  # I return the newest mtime for the given list of files.
  # I return nil if the list is empty.
  # """
  @spec newest_mtime([String.t()]) ::
          [:calendar.datetime() | integer() | :undefined] | nil
  defp newest_mtime([]), do: nil

  defp newest_mtime(file_list) do
    Enum.max(mtimes(file_list))
  end

  # @doc """
  # I return the mtime for the given list of files.
  # """
  @spec mtimes([String.t()]) :: [
          :calendar.datetime() | integer() | :undefined
        ]
  defp mtimes(file_list) do
    for file <- file_list do
      File.stat!(file, time: :posix).mtime
    end
  end

  # @doc """
  # I return the list of files that match the given list of patterns.
  # """
  @spec list_files(String.t() | [String.t()]) :: [String.t()]
  defp list_files(patterns) when is_list(patterns) do
    Enum.flat_map(patterns, &list_files/1)
  end

  defp list_files(pattern) do
    Path.wildcard(pattern)
  end
end
