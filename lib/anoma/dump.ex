defmodule Anoma.Dump do
  @moduledoc """
  I dump the state of the current Node session given a node reference.

  You can also use me to dump info such as current states and tables
  in a readable map format.

  ### Public API

  I give access to following public functionality:

  - `dump/2`
  - `get_all/1`
  - `get_state/1`
  - `get_tables/1`
  """

  alias Anoma.Mnesia
  alias Anoma.Node
  alias Anoma.Node.{Logger, Pinger, Mempool, Executor, Clock}
  alias Anoma.Node.Storage.Ordering
  alias Anoma.Crypto.Id

  @doc """
  I dump the current state with storage. I accept a string as a name,
  so that the resulting file will be created as name.txt and then a
  node whose info presented as a map I dump as a binary.

  The map typing can be seen in `get_all`
  """

  @spec dump(String.t(), atom()) :: {:ok, :ok} | {:error, any()}
  def dump(name, node) do
    term = node |> get_all() |> :erlang.term_to_binary()

    File.open(name <> ".txt", [:write], fn file ->
      file |> IO.binwrite(term)
    end)
  end

  @type log_eng :: {Id.Extern.t(), Logger.t()}
  @type clock_eng :: {Id.Extern.t(), Clock.t()}
  @type ord_eng :: {Id.Extern.t(), Ordering.t()}
  @type mem_eng :: {Id.Extern.t(), Mempool.t()}
  @type ping_eng :: {Id.Extern.t(), Pinger.t()}
  @type ex_eng :: {Id.Extern.t(), Executor.t()}
  @type stores :: {Anoma.Storage.t(), atom()}

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

  @spec get_all(atom()) ::
          %{
            router: Id.Extern.t(),
            mempool_topic: Id.Extern.t(),
            executor_topic: Id.Extern.t(),
            logger: log_eng,
            clock: clock_eng,
            ordering: ord_eng,
            mempool: mem_eng,
            pinger: ping_eng,
            executor: ex_eng,
            storage: stores,
            qualified: list(),
            order: list(),
            block_storage: list()
          }

  def get_all(node) do
    Map.merge(get_state(node), get_tables(node))
  end

  @doc """
  I get the engine states in order:
  - router
  - mempool topic
  - executor topic
  - logger
  - clock
  - ordering
  - mempool
  - pinger
  - executor
  """

  @spec get_state(atom()) ::
          %{
            router: Id.Extern.t(),
            mempool_topic: Id.Extern.t(),
            executor_topic: Id.Extern.t(),
            logger: log_eng,
            clock: clock_eng,
            ordering: ord_eng,
            mempool: mem_eng,
            pinger: ping_eng,
            executor: ex_eng
          }
  def get_state(node) do
    state = node |> Node.state()
    router = state.router

    node =
      state
      |> Map.filter(fn {key, _value} ->
        key not in [
          :router,
          :mempool_topic,
          :executor_topic,
          :__struct__
        ]
      end)
      |> Map.to_list()

    list =
      node
      |> Enum.map(fn {atom, engine} ->
        %{atom => {engine.id, module_match(atom).state(engine)}}
      end)

    map = Enum.reduce(list, fn x, acc -> Map.merge(acc, x) end)

    Map.merge(
      %{
        router: router.id,
        mempool_topic: state.mempool_topic.id,
        executor_topic: state.executor_topic.id
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
          storage: stores,
          qualified: list(),
          order: list(),
          block_storage: list()
        }
  def get_tables(node) do
    node = node |> Node.state()
    table = Ordering.state(node.ordering).table
    block = Mempool.state(node.mempool).block_storage
    qual = table.qualified
    ord = table.order

    {q, o, b} =
      [qual, ord, block]
      |> Enum.map(fn x ->
        with {:ok, lst} <- Mnesia.dump(x) do
          lst
        end
      end)
      |> List.to_tuple()

    %{storage: {table, block}, qualified: q, order: o, block_storage: b}
  end

  @spec module_match(atom()) :: atom()
  defp module_match(address) do
    case address do
      :logger -> Logger
      :pinger -> Pinger
      :mempool -> Mempool
      :executor -> Executor
      :ordering -> Ordering
      :clock -> Clock
    end
  end
end
