defmodule Mix.Tasks.Compile.Protoc do
  @moduledoc """
  I compile the protobuf files before compiling the project.

  In order to avoid unnecessary rebuilds and prevent tools like Elixir LS
  from detecting non-substantial changes, I only update the generated files
  if there is a real change in their contents. This means I first compile the files
  into a temporary directory, compare them with the existing files, and update
  only the files that have actually changed. This prevents needless updates to file
  timestamps and avoids triggering unnecessary recompilation or reanalysis by tools
  that monitor file changes.
  """
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

    # Create a temporary directory for compiling the files
    tmp_out_dir = Path.join(System.tmp_dir!(), "protobuf_compilation")
    File.mkdir_p!(tmp_out_dir)

    # run the protoc command and set the output directory as temp directory
    {_, return_code} =
      System.cmd(protoc_bin, build_arguments(compiler_opts, tmp_out_dir),
        into: IO.stream(:stdio, :line),
        stderr_to_stdout: true
      )

    if return_code == 0 do
      # Compare and update the files if they are different
      elixir_out = compiler_opts[:elixir_out]
      updated_files = compare_and_copy_files(tmp_out_dir, elixir_out)

      # Clean up the temporary directory
      File.rm_rf!(tmp_out_dir)

      if updated_files == [] do
        Mix.shell().info("Protobuf files are up to date")
        :noop
      else
        Mix.shell().info(
          "Updated protobuf files:\n#{Enum.join(updated_files, "\n")}\n"
        )

        :ok
      end
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

  defp build_arguments(compiler_opts, out_dir) do
    extra_opts = Keyword.get(compiler_opts, :extra_opts, "")
    proto_files = Keyword.fetch!(compiler_opts, :proto_files)
    elixir_out = "--elixir_out=#{extra_opts}:#{out_dir}"

    [elixir_out | proto_files]
  end

  # Compare files in tmp_out_dir with files in the real output directory
  # If files differ or are missing, copy them from tmp_out_dir to elixir_out
  defp compare_and_copy_files(tmp_out_dir, elixir_out) do
    tmp_out_dir
    # List all files and directories
    |> File.ls!()
    |> Enum.reduce([], fn item, acc ->
      src_path = Path.join(tmp_out_dir, item)
      dest_path = Path.join(elixir_out, item)

      cond do
        File.dir?(src_path) ->
          # If it's a directory, create it in the destination and recurse
          File.mkdir_p!(dest_path)
          compare_and_copy_files(src_path, dest_path) ++ acc

        File.regular?(src_path) ->
          # If it's a file, compare it with the destination and copy if different
          if files_differ?(src_path, dest_path) do
            File.cp!(src_path, dest_path)
            [dest_path | acc]
          else
            acc
          end

        true ->
          acc
      end
    end)
  end

  defp files_differ?(src_path, dest_path) do
    if File.exists?(dest_path) do
      File.read!(src_path) != File.read!(dest_path)
    else
      true
    end
  end
end
