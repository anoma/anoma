defmodule Anoma.Cli do
  @spec argument_parser() :: Optimus.t()
  def argument_parser() do
    Optimus.new!(
      name: "anoma",
      description: """
      Starts up Anoma.
      """,
      allow_unknown_args: true,
      parse_double_dash: true,
      args: [],
      flags: [
        nohalt: [
          long: "--no-halt",
          help: "typical IEX command",
          required: false
        ]
      ],
      options: [],
      subcommands: [
        nockma: Nock.Cli.argument_option()
      ]
    )
  end

  # Optimus.t() is opaque so the help fails to type check, but it's OK
  @dialyzer {:nowarn_function, start_application: 1}
  @spec start_application([String.t()]) :: {:ok, pid()} | {:error, any()}
  def start_application(arguments) do
    case Optimus.parse(Anoma.Cli.argument_parser(), arguments) do
      # This will occur when you launch your repl
      {:ok, %{flags: %{nohalt: true}}} ->
        Anoma.start_logic()

      # This will occur when one tries to test the codebase
      {:ok, %{unknown: [_, "test" | _]}} ->
        Anoma.start_logic()

      {:ok, [:nockma], parsed} ->
        Nock.Cli.main(parsed)
        System.halt(0)

      :help ->
        IO.puts(Optimus.help(Anoma.Cli.argument_parser()))
        System.halt(0)

      _ ->
        IO.puts(Optimus.help(Anoma.Cli.argument_parser()))
        System.halt(1)
    end
  end
end
