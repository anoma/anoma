defmodule Anoma.Dump do
  @moduledoc """
  I provide an interface to dump current state and load appropriate
  external files to launch them as Anoma nodes.

  You can also use me to dump info such as current states and tables
  in a readable map format as well as get info stored in external
  files in binary format.

  ### Dumping API

  I give access to following public dumping functionality:

  - `dump/2`
  - `get_all/1`
  - `get_state/1`
  - `get_tables/1`

  ### Loading API

  I give access to following public loading functionality

  - `launch/2`
  - `launch/3`
  - `load/1`
  """

  alias Anoma.Mnesia
  alias Anoma.Node

  alias Anoma.Node.{
    Logger,
    Pinger,
    Mempool,
    Executor,
    Clock,
    Storage,
    Router,
    Dumper
  }

  alias Anoma.Node.Ordering
  alias Anoma.Node.Router.Engine
  alias Anoma.Crypto.Id
  alias Anoma.System.Directories

  @typedoc """
  I control launch options for `launch/2`

  ### Options
  - `:supervisor` - This flag determine if we use a supervisor and if
  so what options. See `t:Supervisor.option/0 ` for supervisor options

  - `:testing` - This flag notes if we are testing the node. This gets
    fed directly into the type `t:Anoma.Node.configuration/0` for
    `Anoma.Node.start_link/1`. Please consult the
    `t:Anoma.Node.configuration/0` documentation for the full effect
    this has on the node
  """

  @type launch_option() ::
          {:supervisor, [Supervisor.option()]}
          | {:testing, boolean()}

  @doc """
  I dump the current state with storage. I accept a string as a name,
  so that the resulting file will be created as name.txt in the
  appropriate data directory. As a second argument I accept a node
  name whose info presented as a map I dump as a binary.

  Note that if the environment is `test` we do not use the XDG format
  for storing data and instead dump the files in the immadiate app
  folder.

  The map typing can be seen in `get_all`
  """

  @spec dump(Path.t(), atom()) :: {:ok, :ok} | {:error, any()}
  def dump(name, node) do
    dump_full_path(Directories.data(name), node)
  end

  def dump_full_path(name, node) do
    IO.puts("Made it before term #{inspect(node)}")
    IO.puts("Trying get_all #{inspect(node)}")
    term = node |> get_all() |> :erlang.term_to_binary()
    IO.puts("Made it after term")
    IO.puts("========================")
    IO.puts("#{inspect(term)}")
    IO.puts("========================")
    name
    |> File.open([:write], fn file ->
      file |> IO.binwrite(term)
    end)
  end

  @doc """
  I launch a node given a file containing a binary version of an 12-tuple
  with appropriate info in the following order:
  - router id
  - mempool topic id
  - executor topic id
  - dumper
  - storage
  - logger
  - clock
  - ordering
  - mempool
  - pinger
  - executor
  - storage names
  - qualified
  - order
  - block_storage

  All engines have info on their states and id's so that checkpointing
  the system will keep all adresses used in the previous session.
  Note that I ensure that the apporpriate tables are new.

  Moreover, I ensure that the mempool and block storage are in sync.
  In particular, I check that the order of the last block is less than
  that of the mempool dumped. If not, I manually remove the last block.

  Check whether your transactions have had an assigned worker. If not,
  relaunch them directly. If blocks were out of sync with mempool,
  relaunch the executions as well.

  ### Options
  see `launch_options/0` for the full list of optional arguments
  """

  @dialyzer {:no_return, launch: 2}
  @spec launch(String.t(), atom(), [launch_option()]) ::
          {:ok, %Node{}} | any()
  def launch(file, name, options \\ []) do
    load = file |> load()

    keys = Keyword.validate!(options, supervisor: nil, testing: false)

    settings = block_check(load)

    node_settings = [
      name: name,
      settings: {:from_dump, settings},
      use_rocks: load[:use_rocks],
      testing: keys[:testing]
    ]

    case keys[:supervisor] do
      nil ->
        Anoma.Node.start_link(node_settings)

      settings ->
        [{Anoma.Node, node_settings}]
        |> Supervisor.start_link([{:strategy, :one_for_one} | settings])
    end
  end

  @doc """
  I read the given file which I assume contains binary info and convert
  it to an Elixir term.

  As the dumped state may have extra atoms not present in the session,
  I currently allow for atom creation in the loaded term.
  """

  @spec load(String.t()) :: any() | dump()
  def load(name) do
    {:ok, bin} = File.read(name)
    Plug.Crypto.non_executable_binary_to_term(bin)
  end

  @doc """
  Removes the given dump files at the specified address and with the
  given configuration.

  See `Anoma.System.Directories` for more informaiton about the path
  resolution and for the second atom.
  """
  @spec remove_dump(Path.t()) :: :ok
  @spec remove_dump(Path.t(), atom()) :: :ok
  def remove_dump(file, env \\ Application.get_env(:anoma, :env)) do
    file |> Directories.data(env) |> File.rm!()
  end

  @type dump_eng :: {Id.Extern.t(), Dumper.t()}
  @type log_eng :: {Id.Extern.t(), Logger.t()}
  @type clock_eng :: {Id.Extern.t(), Clock.t()}
  @type ord_eng :: {Id.Extern.t(), Ordering.t()}
  @type mem_eng :: {Id.Extern.t(), Mempool.t()}
  @type ping_eng :: {Id.Extern.t(), Pinger.t()}
  @type ex_eng :: {Id.Extern.t(), Executor.t()}
  @type storage_eng :: {Id.Extern.t(), Storage.t()}
  @type configuration_eng :: {Id.Extern.t(), Anoma.Node.Configuration.t()}
  @type stores :: {Storage.t(), atom()}

  @type dump() :: %{
          router: Id.t(),
          transport: Id.t(),
          router_state: Router.t(),
          transport_id: Id.Extern.t(),
          logger_topic: Id.Extern.t(),
          mempool_topic: Id.Extern.t(),
          executor_topic: Id.Extern.t(),
          storage_topic: Id.Extern.t(),
          configuration: configuration_eng,
          logger: log_eng,
          clock: clock_eng,
          ordering: ord_eng,
          mempool: mem_eng,
          pinger: ping_eng,
          executor: ex_eng,
          storage: storage_eng,
          dumper: dump_eng,
          storage_data: stores,
          qualified: list(),
          order: list(),
          block_storage: list(),
          use_rocks: boolean()
        }

  @doc """
  I get all the info on the node tables and engines in order:
  - router
  - logger
  - clock
  - ordering
  - mempool
  - pinger
  - executor
  - table names
  - qualified
  - order
  - block_storage
  And turn the info into a tuple
  """

  @spec get_all(atom()) :: dump()
  def get_all(node) do
    # Process.flag(:trap_exit, true)
    try do
      Task.async(fn ->
        IO.puts("get all")
        state = get_state(node)
        IO.puts("State: ---------------------------------------")
        # IO.puts("State: #{inspect(state)}")
        IO.puts("State: ---------------------------------------")
        tables = get_tables(node)
        IO.puts("TABLES: ---------------------------------------")
        # IO.puts("TABLES: #{inspect(tables)}")
        IO.puts("TABLES: ---------------------------------------")
        Map.merge(state, tables)
      end) |>Task.await(100_000)
    rescue
      e -> IO.puts("ERRRRORRRR: #{inspect(__STACKTRACE__)}")
           require IEx; IEx.pry()
    end
  end

  @doc """
  I get the engine states in order:
  - router
  - mempool topic
  - executor topic
  - dumper
  - storage
  - logger
  - clock
  - ordering
  - mempool
  - pinger
  - executor
  """

  @spec get_state(atom()) ::
          %{
            router: Id.t(),
            transport: Id.t(),
            router_state: Router.t(),
            transport_id: Id.Extern.t(),
            logger_topic: Id.Extern.t(),
            mempool_topic: Id.Extern.t(),
            executor_topic: Id.Extern.t(),
            storage_topic: Id.Extern.t(),
            configuration: configuration_eng,
            logger: log_eng,
            clock: clock_eng,
            ordering: ord_eng,
            mempool: mem_eng,
            pinger: ping_eng,
            executor: ex_eng,
            storage: storage_eng,
            dumper: dump_eng
          }
  def get_state(node) do
    state = node |> Node.state()

    IO.puts("1. get_state: state finished")
    node =
      state
      |> Map.filter(fn {key, _value} ->
        key not in [
          :router,
          :transport,
          :logger_topic,
          :mempool_topic,
          :executor_topic,
          :storage_topic,
          :__struct__
        ]
      end)
      |> Map.to_list()

    IO.puts("2. node #{inspect(node)}")
    alive = Enum.map(node, fn {atom, engine} -> {atom, engine.server |> Process.whereis |> Process.alive?} end)
    IO.puts("2.1. node #{inspect(alive)}")
    list =
      node
      |> Enum.map(fn {atom, engine} ->
        IO.puts("2.1.X Getting #{inspect(atom)}")
        IO.puts("2.1.X ID is #{inspect(engine.id)}")
        IO.puts("2.1.X is alive?: #{inspect(engine.server |> Process.whereis |> Process.alive?)}")
        IO.puts("2.1.X info:  #{inspect(engine.server |> Process.whereis |> Process.info)}")
        IO.puts("2.1.X stack: #{inspect(engine.server |> Process.whereis |> Process.info(:current_stacktrace))}")
        IO.puts("2.1.X PID: #{inspect(engine.server |> Process.whereis)}")
        IO.puts("-----------------------------------------------------------------------------")
        # StringIO.flush(Process.group_leader())
        IO.puts("-----------------------------------------------------------------------------")
        res = %{atom => {engine.id, Engine.get_state(engine)}}
        # StringIO.flush(Process.group_leader())
        IO.puts("2.1.Z #{inspect(engine.id)}: giving back res")
        res
      end)

    IO.puts("3. node****************************************")
    map = Enum.reduce(list, fn x, acc -> Map.merge(acc, x) end)

    IO.puts("4. node*****************************************")
    # EVIL, please make this not evil
    internal_transport_id =
      Engine.get_state(state.transport).transport_internal_id

    IO.puts("5. node******************************************")
    # This is rather bad, as we are peeking at the internal state, and
    # we are not using the engine, so it will have issues across
    # nodes....

    IO.puts("6. node******************************************")
    router_id =
      :sys.get_state(Process.whereis(state.router.server)).internal_id

    IO.puts("7. node******************************************")
    router_state = Anoma.Node.Router.dump_state(state.router.server)
    # Back to normal work

    IO.puts("8. node******************************************")
    Map.merge(
      %{
        router: router_id,
        router_state: router_state,
        transport: internal_transport_id,
        # public facing id for other nodes to talk to
        transport_id: state.transport.id,
        logger_topic: state.logger_topic.id,
        mempool_topic: state.mempool_topic.id,
        executor_topic: state.executor_topic.id,
        storage_topic: state.storage_topic.id
      },
      map
    )
  end

  @doc """
  I get the node tables in order:
  - storage (names)
  - qualified
  - order
  - block_storage
  """

  @spec get_tables(atom()) :: %{
          storage_data: stores,
          qualified: list(),
          order: list(),
          block_storage: list(),
          use_rocks: boolean()
        }
  def get_tables(node) do
    IO.puts("tables. ---------------------")
    node = node |> Node.state()
    table = Engine.get_state(Engine.get_state(node.ordering).table)
    block = Engine.get_state(node.mempool).block_storage
    qual = table.qualified
    ord = table.order
    # TODO more robust checking here
    rocks =
      if :ram_copies == :mnesia.table_info(qual, :storage_type) do
        false
      else
        true
      end

    {q, o, b} =
      [qual, ord, block]
      |> Enum.map(fn x ->
        with {:ok, lst} <- Mnesia.dump(x) do
          Enum.map(lst, fn x -> hd(x) end)
        end
      end)
      |> List.to_tuple()

    %{
      storage_data: {table, block},
      qualified: q,
      order: o,
      block_storage: b,
      use_rocks: rocks
    }
  end

  @spec block_check(dump()) :: dump()
  defp block_check(map) do
    block_storage = map.block_storage

    if block_storage != [] do
      last_block_list = block_storage |> List.last()

      last_block = last_block_list |> Anoma.Block.decode()

      {_id, mempool} = map.mempool

      if last_block.round == mempool.round do
        Map.replace(
          map,
          :block_storage,
          List.delete(block_storage, last_block_list)
        )
      else
        map
      end
    else
      map
    end
  end
end
