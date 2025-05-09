# I compile the juvix files in the current folder.
[file] = System.argv()

# ----------------------------------------------------------------------------
# Verify compiler version

# version of the compiler used
expected_juvix = "0.6.10-9692fe8"

# ensure the juvix compiler is the right one
{compiler_version, _} = System.cmd("juvix", ["--version"])

if not String.contains?(compiler_version, expected_juvix) do
  raise "expected Juvix compiler version #{expected_juvix}, got #{compiler_version}"
end

# ----------------------------------------------------------------------------
# Create scaffolding directory

# set the cwd to the juvix directory
juvix_dir = Path.join(:code.priv_dir(:anoma_client), ["juvix", "/src"])
File.cd(juvix_dir)

# ----------------------------------------------------------------------------
# List all the files to be compiled

# list all juvix source files
juvix_src_files =
  juvix_dir
  |> Path.join("**/*.juvix")
  |> Path.wildcard()
  |> Enum.map(&Path.relative_to(&1, juvix_dir))
  |> Enum.filter(fn x -> String.contains?(x, file) end)

# ----------------------------------------------------------------------------
# Compile all the juvix files

for src <- juvix_src_files do
  filename = Path.basename(src, ".juvix")
  dir = Path.dirname(src)
  compiled = Path.join(dir, [".compiled", "/#{filename}.nockma"])

  # create the compiled output directory if necessary
  unless File.exists?(Path.dirname(compiled)) do
    File.mkdir!(Path.dirname(compiled))
  end

  # compile the file
  case System.cmd("juvix", ["compile", "anoma", src, "-o", compiled],
         stderr_to_stdout: true
       ) do
    {_, 0} ->
      :ok

    {output, 1} ->
      if String.contains?(output, "no `main` function") do
        :ok
      else
        raise "failed to compile #{src}, output: #{output}"
      end
  end

  IO.puts("compiled #{src}")
end
