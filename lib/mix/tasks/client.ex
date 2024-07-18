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
    Logger.configure(level: :error)

    Optimus.parse!(argument_parser(), args)
    |> Anoma.Cli.run_commands()

    # We sleep as this does not wait for the client to finish, it'll
    # auto exit for us.
    Process.sleep(10_000_000)
  end
end
