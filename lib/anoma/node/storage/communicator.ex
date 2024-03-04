defmodule Anoma.Node.Storage.Communicator do
  @moduledoc """

  I am the communicator for the Ordering Node, please read my Ordering
  node to know more about how my API works

  """

  use TypedStruct
  use Anoma.Communicator, sub_field: :subscribers
  alias Anoma.Communicator, as: ACom

  alias __MODULE__
  alias Anoma.Node.Storage.Ordering
  alias Anoma.Node.Utility
  alias Anoma.Storage
  alias Anoma.Node.Logger

  typedstruct do
    field(:primary, atom(), enforce: true)
    field(:subscribers, ACom.t(), default: ACom.new())
    field(:logger, atom(), enforce: false)
  end

  def init(name: name, logger: logger) do
    {:ok,
     %Communicator{
       primary: name,
       logger: logger
     }}
  end

  def start_link(arg) do
    GenServer.start_link(
      __MODULE__,
      arg,
      Utility.name(arg, &Utility.com_name/1)
    )
  end

  ############################################################
  #                      Public RPC API                      #
  ############################################################

  @spec state(GenServer.server()) :: Ordering.t()
  defdelegate state(ordering), to: Ordering

  @spec next_order(GenServer.server()) :: non_neg_integer()
  defdelegate next_order(ordering), to: Ordering

  @spec true_order(GenServer.server(), any()) :: non_neg_integer() | nil
  defdelegate true_order(ordering, id), to: Ordering

  @spec new_order(GenServer.server(), Ordering.ordered_transactions()) ::
          :error | {:ok, any()}
  defdelegate new_order(ordering, ordered), to: Ordering

  @spec get_storage(GenServer.server()) :: Storage.t()
  defdelegate get_storage(ordering), to: Ordering

  @spec reset(GenServer.server()) :: :ok
  defdelegate reset(ordering), to: Ordering

  @spec hard_reset(GenServer.server(), atom()) :: :ok
  defdelegate hard_reset(ordering, initial_snapshot), to: Ordering

  ############################################################
  #                    Genserver Behavior                    #
  ############################################################

  # Please give things to the subscribers

  def handle_call(:state, _from, com) do
    primary = com.primary
    log_info({:state, primary, com.logger})

    {:reply, Ordering.state(primary), com}
  end

  def handle_call(:next_order, _from, com) do
    primary = com.primary
    log_info({:next, primary, com.logger})

    {:reply, Ordering.next_order(primary), com}
  end

  def handle_call({:true_order, id}, _from, com) do
    primary = com.primary
    log_info({true, primary, com.logger})

    {:reply, Ordering.true_order(primary, id), com}
  end

  def handle_call({:new_order, trans}, _from, com) do
    primary = com.primary
    log_info({:new, primary, com.logger})

    {:reply, Ordering.new_order(primary, trans), com}
  end

  def handle_call(:storage, _from, state) do
    primary = state.primary
    log_info({:storage, primary, state.logger})

    {:reply, Ordering.get_storage(state.primary), state}
  end

  def handle_cast(:reset, state) do
    primary = state.primary
    log_info({:reset, primary, state.logger})

    Ordering.reset(state.primary)
    {:noreply, state}
  end

  def handle_cast({:hard_reset, initial}, state) do
    primary = state.primary
    log_info({:hard_reset, primary, state.logger})

    Ordering.hard_reset(state.primary, initial)
    {:noreply, state}
  end

  ############################################################
  #                     Logging Info                         #
  ############################################################

  defp log_info({:state, primary, logger}) do
    Logger.add(
      logger,
      self(),
      :info,
      "Communicator asked for the state of #{inspect(primary)}"
    )
  end

  defp log_info({:next, primary, logger}) do
    Logger.add(
      logger,
      self(),
      :info,
      "Communicator asked for next order of #{inspect(primary)}"
    )
  end

  defp log_info({true, primary, logger}) do
    Logger.add(
      logger,
      self(),
      :info,
      "Asked fo true order from communicator of #{inspect(primary)}"
    )
  end

  defp log_info({:new, primary, logger}) do
    Logger.add(
      logger,
      self(),
      :info,
      "Communicator asked for new order of #{inspect(primary)}"
    )
  end

  defp log_info({:storage, primary, logger}) do
    Logger.add(
      logger,
      self(),
      :info,
      "Communicator asked to get storage of #{inspect(primary)}"
    )
  end

  defp log_info({:reset, primary, logger}) do
    Logger.add(
      logger,
      self(),
      :debug,
      "Communicator asked to reset of #{inspect(primary)}"
    )
  end

  defp log_info({:hard_reset, primary, logger}) do
    Logger.add(
      logger,
      self(),
      :debug,
      "Communicator asked to hard reset of #{inspect(primary)}"
    )
  end
end
