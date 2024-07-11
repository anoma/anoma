defmodule Anoma.Cli do
  alias Anoma.Node.Transport
  alias Anoma.Node.Router

  @type client_commands() ::
          :delete_dump
          | :get
          | :nockma
          | :rm_submit
          | :shutdown
          | :snapshot
          | :submit

  @type client_info() ::
          {Router.addr(), Router.addr(), Anoma.Dump.dump() | any(),
           Transport.transport_addr()}

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
        ],
        no_rocksdb: [
          long: "--no-rocks",
          help: "Do not use the rocksdb mnesia backend",
          required: false
        ]
      ],
      options: [],
      subcommands: client_commands()
    )
  end

  @spec client_commands() :: [{client_commands(), [{any(), any()}, ...]}, ...]
  def client_commands() do
    [
      nockma: Nock.Cli.argument_option(),
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
      rm_submit: [
        name: "rm-submit",
        about: "Submit a transaction to the local node.",
        args: [
          file: [
            required: true,
            parser: :string
          ]
        ]
      ],
      ro_submit: [
        name: "ro-submit",
        about: "Submit a read-only transaction and print computed value.",
        args: [
          file: [
            required: true,
            parser: :string
          ]
        ]
      ],
      shutdown: [
        name: "shutdown",
        about: "Shutdowns the server"
      ],
      snapshot: [
        name: "snapshot",
        about: "Takes a snapshot of the server"
      ],
      delete_dump: [
        name: "delete-dump",
        about:
          "Deletes the dump file of Anoma, giving a fresh slate. Works even when the node is offline."
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
  end

  @doc """
  Provides taking CLI argument parsing to arguments used by the
  application
  """
  @spec cli_arguments_to_start_arguments(Optimus.ParseResult.t()) ::
          Keyword.t()
  def cli_arguments_to_start_arguments(%Optimus.ParseResult{
        args: _args,
        flags: %{no_rocksdb: not_rocks},
        options: _options,
        unknown: _unknown
      }) do
    [use_rocks: not not_rocks]
  end

  @spec start_application([String.t()]) :: {:ok, pid()} | {:error, any()}
  def start_application(arguments) do
    start_anoma = fn parsed ->
      parsed
      |> cli_arguments_to_start_arguments()
      |> Anoma.start_logic()
    end

    case Optimus.parse(Anoma.Cli.argument_parser(), arguments) do
      # This will occur when you launch your repl
      {:ok, args = %{flags: %{nohalt: true}}} ->
        start_anoma.(args)

      # This will occur when one tries to test the codebase
      {:ok, args = %{unknown: [_, "test" | _]}} ->
        start_anoma.(args)

      # Happens on various debug tooling
      {:ok, args = %{unknown: [debugger]}} ->
        if debugger |> String.contains?("elixir-ls") do
          start_anoma.(args)
        else
          top_level_help()
          System.halt(1)
        end

      {:ok, command, args} ->
        {:ok, client} = run_commands({command, args}, server_anoma_node())

        client
        |> Anoma.Cli.Client.error_code()
        |> System.halt()

      :help ->
        # The default parse! will quit
        Optimus.parse!(Anoma.Cli.argument_parser(), arguments)

      # Happens on sub commands as well
      {:help, _} ->
        Optimus.parse!(Anoma.Cli.argument_parser(), arguments)

      _ ->
        top_level_help()
        System.halt(1)
    end
  end

  @spec run_commands({[client_commands(), ...], map()}, client_info()) ::
          :ok | {:ok, Router.addr()}
  @doc """
  Runs the given client command
  """
  def run_commands({[:submit], %{args: %{file: file}}}, ci) do
    run_client_command({:submit_tx, file}, ci)
  end

  def run_commands({[:rm_submit], %{args: %{file: file}}}, ci) do
    run_client_command({:rm_submit_tx, file}, ci)
  end

  def run_commands({[:ro_submit], %{args: %{file: file}}}, ci) do
    run_client_command({:ro_submit_tx, file}, ci)
  end

  def run_commands({[:get], %{args: %{key: key}}}, ci) do
    run_client_command({:get_key, key}, ci)
  end

  def run_commands({[:shutdown], %{}}, ci) do
    run_client_command(:shutdown, ci)
  end

  def run_commands({[:delete_dump], %{}}, ci) do
    run_client_command(:delete_dump, ci)
  end

  def run_commands({[:snapshot], %{}}, ci) do
    run_client_command(:snapshot, ci)
  end

  def run_commands({[:nockma], parsed}, _ci) do
    Nock.Cli.main(parsed)
  end

  @spec server_anoma_node() :: client_info()
  def server_anoma_node() do
    {:ok, router, transport} = Anoma.Node.Router.start()

    # load info of the running node, erroring if it appears not to exist, and
    # attempt to introduce ourselves to it
    # there should be a better way to find out its id(s)
    dump_path = Anoma.System.Directories.data("node_keys.dmp")
    sock_path = Anoma.System.Directories.data("local.sock")
    server = Anoma.Dump.load(dump_path)
    {router, transport, server, {:unix, sock_path}}
  end

  # Optimus.t() is opaque so the help fails to type check, but it's OK
  @dialyzer {:nowarn_function, top_level_help: 0}
  def top_level_help() do
    IO.puts(Optimus.help(Anoma.Cli.argument_parser()))
  end

  @spec run_client_command(any(), client_info()) :: {:ok, Router.addr()}
  def run_client_command(operation, {router, transport, server, sock}) do
    {:ok, addr} =
      Anoma.Node.Router.start_engine(
        router,
        Anoma.Cli.Client,
        {router, transport, server, sock, operation}
      )

    {:ok, addr}
  end
end
