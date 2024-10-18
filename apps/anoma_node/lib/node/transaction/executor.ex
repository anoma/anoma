defmodule Anoma.Node.Transaction.Executor do
  alias __MODULE__
  alias Anoma.Node
  alias Node.Transaction.{Backends, Mempool, Ordering}
  alias Node.Registry

  use TypedStruct

  use GenServer

  require Node.Event

  @typep startup_options() :: {:node_id, String.t()}

  typedstruct do
    field(:node_id, String.t())
  end

  typedstruct enforce: true, module: ExecutionEvent do
    field(:result, list({:ok | :error, binary()}))
  end

  @spec start_link([startup_options()]) :: GenServer.on_start()
  def start_link(args) do
    args = Keyword.validate!(args, [:node_id])
    name = Registry.via(args[:node_id], __MODULE__)
    GenServer.start_link(__MODULE__, args, name: name)
  end

  @spec init([startup_options()]) :: {:ok, Executor.t()}
  def init(args) do
    Process.set_label(__MODULE__)

    EventBroker.subscribe_me([
      Node.Event.node_filter(args[:node_id]),
      Mempool.worker_module_filter(),
      complete_filter()
    ])

    state = struct(__MODULE__, Enum.into(args, %{}))
    {:ok, state}
  end

  @spec launch(String.t(), {Backends.backend(), Noun.t()}, binary()) :: :ok
  def launch(node_id, tw_w_backend, id) do
    GenServer.cast(
      Registry.via(node_id, __MODULE__),
      {:launch, tw_w_backend, id}
    )
  end

  @spec execute(String.t(), [binary()]) :: :ok
  def execute(node_id, consensus) do
    GenServer.cast(Registry.via(node_id, __MODULE__), {:execute, consensus})
  end

  def handle_cast({:launch, tw_w_backend, id}, state) do
    Task.start(fn ->
      Backends.execute(state.node_id, tw_w_backend, id)
    end)

    {:noreply, state}
  end

  def handle_cast({:execute, consensus}, state) do
    node_id = state.node_id

    Ordering.order(node_id, consensus)

    res_list =
      for id <- consensus, reduce: [] do
        list ->
          receive do
            %EventBroker.Event{
              body: %Node.Event{
                body: %Backends.CompleteEvent{
                  tx_id: ^id,
                  tx_result: res
                }
              }
            } ->
              [{res, id} | list]
          after
            5000 -> raise "Timeout waiting for #{inspect(id)}"
          end
      end

    execution_event(res_list, node_id)
    {:noreply, state}
  end

  @spec execution_event([{any(), binary()}], String.t()) :: :ok
  def execution_event(res_list, node_id) do
    event =
      Node.Event.new_with_body(node_id, %__MODULE__.ExecutionEvent{
        result: res_list
      })

    EventBroker.event(event)
  end

  def complete_filter() do
    %Backends.CompleteFilter{}
  end
end
