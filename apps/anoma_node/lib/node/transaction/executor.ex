defmodule Anoma.Node.Transaction.Executor do
  alias __MODULE__
  alias Anoma.Node
  alias Node.Transaction.{Backends, Mempool, Ordering}
  alias Node.Registry

  use TypedStruct

  use GenServer

  require Node.Event

  ############################################################
  #                         State                            #
  ############################################################

  @typep startup_options() :: {:node_id, String.t()}

  typedstruct do
    field(:node_id, String.t())
  end

  typedstruct enforce: true, module: ExecutionEvent do
    field(:result, list({{:ok, any} | :error, binary()}))
  end

  ############################################################
  #                    Genserver Helpers                     #
  ############################################################

  @spec start_link([startup_options()]) :: GenServer.on_start()
  def start_link(args) do
    args = Keyword.validate!(args, [:node_id])
    name = Registry.via(args[:node_id], __MODULE__)
    GenServer.start_link(__MODULE__, args, name: name)
  end

  @impl true
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

  ############################################################
  #                      Public RPC API                      #
  ############################################################

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

  ############################################################
  #                      Public Filters                      #
  ############################################################

  @spec complete_filter() :: Backends.CompleteFilter.t()
  def complete_filter() do
    %Backends.CompleteFilter{}
  end

  ############################################################
  #                    Genserver Behavior                    #
  ############################################################

  @impl true
  def handle_cast({:launch, tw_w_backend, id}, state) do
    handle_launch(tw_w_backend, id, state)
    {:noreply, state}
  end

  def handle_cast({:execute, consensus}, state) do
    handle_execute(consensus, state)
    {:noreply, state}
  end

  ############################################################
  #                 Genserver Implementation                 #
  ############################################################

  @spec handle_launch({Backends.backend(), Noun.t()}, binary(), t()) :: :ok
  defp handle_launch(tw_w_backend, id, state = %Executor{}) do
    Task.start(fn ->
      Backends.execute(state.node_id, tw_w_backend, id)
    end)

    :ok
  end

  @spec handle_execute(list(binary()), t()) :: :ok
  defp handle_execute(consensus, state = %Executor{}) do
    node_id = state.node_id

    Ordering.order(node_id, consensus)

    res_list =
      Enum.map(consensus, &listen_for_worker_finish!/1)
      |> Enum.reverse()

    execution_event(res_list, node_id)
  end

  ############################################################
  #                           Helpers                        #
  ############################################################

  @spec listen_for_worker_finish!(integer()) ::
          {{:ok, any()} | :error, binary()}
  defp listen_for_worker_finish!(id) do
    receive do
      %EventBroker.Event{
        body: %Node.Event{
          body: %Backends.CompleteEvent{
            tx_id: ^id,
            tx_result: res
          }
        }
      } ->
        {res, id}
    after
      5000 -> raise "Timeout waiting for #{inspect(id)}"
    end
  end

  @spec execution_event([{{:ok, any()} | :error, binary()}], String.t()) ::
          :ok
  defp execution_event(res_list, node_id) do
    event =
      Node.Event.new_with_body(node_id, %__MODULE__.ExecutionEvent{
        result: res_list
      })

    EventBroker.event(event)
  end
end
