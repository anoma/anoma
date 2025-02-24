defmodule Anoma.Node.Transaction.Executor do
  @moduledoc """
  I am the Executor Engine.

  My main functionality is to launch new workers and keep track of
  finalized transaction execution, sending the consensus ordering data to
  the Ordering Engine and waiting for workers to signal process completion.
  That is, I mark beginning and end of the transaction execution,
  abstracting the functionality away from the Mempool.

  ### Public API

  I provide the following public functionality:

  - `complete_filter/0`
  - `execute/2`
  - `launch/3`
  """

  alias __MODULE__
  alias Anoma.Node
  alias Anoma.Node.Registry
  alias Anoma.Node.Transaction.Backends
  alias Anoma.Node.Transaction.Mempool
  alias Anoma.Node.Transaction.Ordering
  alias Anoma.Node.Events

  require Node.Event

  use GenServer
  use TypedStruct

  ############################################################
  #                         State                            #
  ############################################################

  @typep startup_options() :: {:node_id, String.t()}

  typedstruct do
    @typedoc """
    I am the type of the Executor Engine.

    Currently I only keep information of the relevant node identitfication.

    ### Fileds

    - `:node_id` - The ID of the node to which an Executor instantiation is
                   bound.
    """

    field(:node_id, String.t())
  end

  typedstruct enforce: true, module: TaskCrash do
    @typedoc """
    I am a crash event for a task that failed.
    """
    field(:task, any())
  end

  ############################################################
  #                    Genserver Helpers                     #
  ############################################################

  @doc """
  I am the start_link function of the Executor Engine.

  I register the engine with supplied node ID provided by the arguments.
  """

  @spec start_link([startup_options()]) :: GenServer.on_start()
  def start_link(args) do
    args = Keyword.validate!(args, [:node_id])
    name = Registry.via(args[:node_id], __MODULE__)
    GenServer.start_link(__MODULE__, args, name: name)
  end

  @impl true
  @doc """
  I am the intialization function for the Executor Engine.

  Given the arguments containing a node ID, I subscribe to all messages
  which come from completed workers of that node and launch the Executor
  with appropriate state.
  """

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

  @doc """
  I am the Executor launch function.

  Given a transaction in {backend, code} format with specific ID,
  I launch that transaction as a task. If no ID is provided, I generate one
  randomly.
  """

  @spec launch(String.t(), {Backends.backend(), Noun.t()}, binary()) :: :ok
  def launch(node_id, tw_w_backend, id \\ :crypto.strong_rand_bytes(16)) do
    GenServer.cast(
      Registry.via(node_id, __MODULE__),
      {:launch, tw_w_backend, id}
    )
  end

  @doc """
  I am the Executor execute function.

  I accept the node ID and a list of transaction IDs as my arguments.
  Afterwards, the list is sent to the Ordering.

  At that point, I make Executor go into a receive loop processing all
  messages about finished workers. If all consensus-provided workers with
  specified IDs have finished in time, I broadcast a completion message
  with all the execution results. Otherwise, Executor crashes.
  """

  @spec execute(String.t(), [binary()]) :: :ok
  def execute(node_id, consensus) do
    GenServer.cast(Registry.via(node_id, __MODULE__), {:execute, consensus})
  end

  ############################################################
  #                      Public Filters                      #
  ############################################################

  @doc """
  I am a filter for completion messages from workers.
  """

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

  # @doc """
  # I launch a transaction in its own Task to execute.
  # """
  @spec handle_launch({Backends.backend(), Noun.t()}, binary(), t()) :: :ok
  defp handle_launch(tw_w_backend, id, state = %Executor{}) do
    tx_supervisor = Registry.via(state.node_id, TxSupervisor)

    Task.Supervisor.start_child(tx_supervisor, fn ->
      try do
        Backends.execute(state.node_id, tw_w_backend, id)
      rescue
        _e ->
          task_crash_event(id, state.node_id)
      end
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

    Events.execution_event(res_list, node_id)
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
          body: %Events.CompleteEvent{
            tx_id: ^id,
            tx_result: res
          }
        }
      } ->
        {res, id}
    end
  end

  @spec task_crash_event(any(), String.t()) :: :ok
  defp task_crash_event(task, node_id) do
    event =
      Node.Event.new_with_body(node_id, %__MODULE__.TaskCrash{task: task})

    EventBroker.event(event)
  end
end
