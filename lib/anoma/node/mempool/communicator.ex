defmodule Anoma.Node.Mempool.Communicator do
  @moduledoc """

  """

  use GenServer
  use TypedStruct

  alias __MODULE__
  alias Anoma.Node.Utility
  alias Anoma.Node.Mempool.Pool

  typedstruct do
    field(:pool, atom(), require: true)
    field(:executor, GenServer.server())
    field(:intent, GenServer.server())
  end

  def init(name: name, executor: exec, intent: intent) do
    {:ok, %Communicator{pool: name, executor: exec, intent: intent}}
  end

  def start_link(arg) do
    GenServer.start_link(Communicator, arg, Utility.name(arg, &Utility.com_name/1))
  end

  ############################################################
  #                      Public RPC API                      #
  ############################################################

  ############################################################
  #                    Genserver Behavior                    #
  ############################################################
end
