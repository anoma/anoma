defmodule Mix.Tasks.Client do
  @moduledoc "I generate out the TOC for each liveview doc"

  @shortdoc "Simply adds a TOC to each liveview doc"

  alias Anoma.Cli.Client
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

  @spec run(any()) :: no_return()
  def run(args) do
    Logger.configure(level: :error)

    {:ok, client} =
      argument_parser()
      |> Optimus.parse!(args)
      |> Anoma.Cli.run_commands(Anoma.Cli.server_anoma_node())

    System.halt(Client.error_code(client))
  end
end
