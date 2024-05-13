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
      subcommands: [
        submit: [
          name: "submit",
          about: "Submit a transaction to the local node.",
          args: [
            file: [
              required: true,
              parser: :string
            ]
          ]
        ],
        get: [
          name: "get",
          about:
            "Fetch the current value of the given key from the local node.",
          args: [
            key: [
              required: true,
              parser: :integer
            ]
          ]
        ]
      ]
    )
  end

  def run(args) do
    case Optimus.parse!(argument_parser(), args) do
      {[:submit], %{args: %{file: file}}} ->
        run_client_command({:submit_tx, file})

      {[:get], %{args: %{key: key}}} ->
        run_client_command({:get_key, key})
    end
  end

  defp run_client_command(operation) do
    Logger.configure(level: :error)
    {:ok, router, transport} = Anoma.Node.Router.start()

    {:ok, _} =
      Anoma.Node.Router.start_engine(
        router,
        Anoma.Cli.Client,
        {router, transport, operation}
      )

    Process.sleep(10_000_000)
    # Cli.Client is reponsible for shutting down the system once it's done
    # whatever it want to do, so we have no more to do here
  end
end
