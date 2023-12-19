defmodule Anoma.Node.Executor.Primary do
  @moduledoc """
  I represent the main logic checking functonality of an `Anoma.Node.Executor`.

  I can be communicated by, by my public API, often this is done by my
  `Anoma.Node.Communicator`, however everything can communicate with
  me like my `Communicator`.
  """

  alias __MODULE__
  use TypedStruct
  use GenServer

  alias Anoma.PartialTx
  alias Anoma.Node.Utility

  @type response :: boolean()

  # TODO what gets stored in an executor?
  typedstruct do
  end

  def init(_init) do
    {:ok, %Primary{}}
  end

  def start_link(arg) do
    GenServer.start_link(__MODULE__, arg, Utility.name(arg))
  end

  ############################################################
  #                      Public RPC API                      #
  ############################################################

  # mainly for easier debugging
  @spec dump_state(GenServer.server()) :: t()
  def dump_state(primary) do
    GenServer.call(primary, :dump_state)
  end

  @spec new_transactions(GenServer.server(), Enumerable.t(PartialTx.t())) ::
          response()
  def new_transactions(primary, transactions) do
    GenServer.call(primary, {:transactions, transactions})
  end

  ############################################################
  #                    Genserver Behavior                    #
  ############################################################

  def handle_call({:transactions, trans}, _from, primary) do
    response = check_transaction(trans)
    {:reply, response, primary}
  end

  def handle_call(:dump_state, _pid, basic) do
    {:reply, basic, basic}
  end

  ############################################################
  #                  Genserver Implementation                #
  ############################################################

  @spec check_transaction(Enumerable.t(PartialTx.t())) :: boolean()
  defp check_transaction(transactions) do
    transactions
    |> Enum.all?(&PartialTx.is_valid/1)
  end
end
