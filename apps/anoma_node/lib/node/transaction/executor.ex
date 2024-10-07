defmodule Anoma.Node.Transaction.Executor do
  alias Anoma.Node.Transaction.{Backends, Mempool, Ordering}
  alias __MODULE__

  use TypedStruct
  require EventBroker.Event

  typedstruct do
  end

  typedstruct enforce: true, module: ExecutionEvent do
    field(:result, list({:ok | :error, binary()}))
  end

  def start_link(default) do
    GenServer.start_link(__MODULE__, default, name: Executor)
  end

  @spec init(any()) :: {:ok, Executor.t()}
  def init(_arg) do
    EventBroker.subscribe_me([
      Mempool.worker_module_filter(),
      complete_filter()
    ])

    {:ok, %__MODULE__{}}
  end

  def launch(tw_w_backend, id) do
    GenServer.cast(__MODULE__, {:launch, tw_w_backend, id})
  end

  def execute(consensus) do
    GenServer.cast(__MODULE__, {:execute, consensus})
  end

  def handle_cast({:launch, tw_w_backend, id}, state) do
    Task.start(fn ->
      Backends.execute(tw_w_backend, id)
    end)

    {:noreply, state}
  end

  def handle_cast({:execute, consensus}, state) do
    Ordering.order(consensus)

    res_list =
      for id <- consensus, reduce: [] do
        list ->
          receive do
            %EventBroker.Event{
              body: %Backends.CompleteEvent{
                tx_id: ^id,
                tx_result: res
              }
            } ->
              [{res, id} | list]
          after
            5000 -> raise "Timeout waiting for #{inspect(id)}"
          end
      end

    execution_event(res_list)
    {:noreply, state}
  end

  def execution_event(res_list) do
    event =
      EventBroker.Event.new_with_body(%__MODULE__.ExecutionEvent{
        result: res_list
      })

    EventBroker.event(event)
  end

  def complete_filter() do
    %Backends.CompleteFilter{}
  end
end
