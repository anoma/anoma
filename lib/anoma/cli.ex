defmodule Anoma.Cli do
  @spec argument_parser() :: Optimus.t()
  def argument_parser() do
    Optimus.new!(
      name: "anoma",
      description: """
      Starts up Anoma.
      """,
      allow_unknown_args: false,
      parse_double_dash: true,
      args: [],
      options: [],
      subcommands: [
        nockma: Nock.Cli.argument_option()
      ]
    )
  end

  @spec main([binary()]) :: :ok
  def main(argv) do
    case Optimus.parse!(argument_parser(), argv) do
      {[:nockma], parsed} ->
        Nock.Cli.main(parsed)

      parsed ->
        IO.inspect(parsed)
        IO.puts("Hello from Anoma. Sadly the Cli is barebones")
    end
  end
end
