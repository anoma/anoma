defmodule Anoma.Node.Executor.Communicator do
  @moduledoc """
  I Manage the Pub Sub behavior

  If new intents come in, I send it to all my subscribers.

  Further I have the job of communicating to the `Primary` node when I
  get information for it to process.

  The information that is communicated is:

  1. intent solutions
  2. new intents

  """
  alias __MODULE__
  use TypedStruct
  use GenServer
  alias Anoma.PartialTx
  alias Anoma.Node.Utility
  alias Anoma.Node.Executor.Primary

  typedstruct do
    field(:primary, atom())
  end

  def init(name: name) do
    {:ok, %Communicator{primary: name}}
  end

  def start_link(arg) do
    GenServer.start_link(__MODULE__, arg, Utility.name(arg, &Utility.com_name/1))
  end

  ############################################################
  #                      Public RPC API                      #
  ############################################################

  # Transactions
  @spec new_transactions(pid(), Enumerable.t(PartialTx.t())) :: Primary.response()
  def new_transactions(communicator, transactions) do
    GenServer.call(communicator, {:transactions, transactions})
  end

  ############################################################
  #                    Genserver Behavior                    #
  ############################################################

  def handle_call({:transactions, trans}, _from, agent) do
    response = broadcast_transactions(agent, trans)
    {:reply, response, agent}
  end

  ############################################################
  #                  Genserver Implementation                #
  ############################################################

  # make this more interesting later
  @spec broadcast_transactions(t(), Enumerable.t(PartialTx.t())) :: Primary.response()
  defp broadcast_transactions(agent, trans) do
    Primary.new_transactions(agent.primary, trans)
  end
end
