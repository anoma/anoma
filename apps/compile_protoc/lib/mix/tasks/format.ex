defmodule Mix.Tasks.FormatProtoc do
  use Mix.Task

  @shortdoc "Formats the protobuf files using clang-format"

  @impl true
  def run(_args) do
    format_clang_installed? = clang_format_executable() != nil

    if format_clang_installed? do
      "**/*.proto"
      |> Path.wildcard()
      |> format_files()

      :ok
    else
      Mix.shell().info("clang-format is required to run this task")
      :ok
    end
  end

  # @doc """
  # I format the given list of files using the local clang-format executable.
  # """
  @spec format_files(list(String.t())) :: :ok
  def format_files(file_paths) do
    clang_format = clang_format_executable()

    Enum.each(file_paths, fn file_path ->
      System.cmd(clang_format, ["-i", file_path])
    end)
  end

  # @doc """
  # I find the clang-format executable in the current system, or raise if its not found.
  # """
  @spec clang_format_executable() :: String.t()
  defp clang_format_executable() do
    System.find_executable("clang-format")
  end
end
