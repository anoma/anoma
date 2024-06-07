defmodule Mix.Tasks.Client do
  @moduledoc "I generate out the TOC for each liveview doc"

  @shortdoc "Simply adds a TOC to each liveview doc"
  use Mix.Task

  def argument_parser() do
    Optimus.new!(
      name: "anoma",
      description: """
      Starts up Anoma.
      """,
      allow_unknown_args: true,
      parse_double_dash: true,
      args: [],
      subcommands: Anoma.Cli.client_commands()
    )
  end

  def run(args) do
    case Optimus.parse!(argument_parser(), args) do
      {[:submit], %{args: %{file: file}}} ->
        run_client_command({:submit_tx, file})

      {[:rm_submit], %{args: %{file: file}}} ->
        run_client_command({:rm_submit_tx, file})

      {[:get], %{args: %{key: key}}} ->
        run_client_command({:get_key, key})

      {[:shutdown], %{}} ->
        run_client_command(:shutdown)

      {[:delete_dump], %{}} ->
        run_client_command(:delete_dump)

      {[:snapshot], %{}} ->
        run_client_command(:snapshot)
    end
  end

  defp run_client_command(operation) do
    Logger.configure(level: :error)
    Anoma.Cli.run_client_command(operation)
    # We sleep as this does not wait for the client to finish, it'll
    # auto exit for us.
    Process.sleep(10_000_000)
  end
end
