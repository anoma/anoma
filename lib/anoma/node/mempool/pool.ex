defmodule Anoma.Node.Mempool.Pool do
  @moduledoc """

  """

  use TypedStruct
  use GenServer

  alias __MODULE__
  alias Anoma.Node.Utility

  # TODO Get a merkle tree stored here?
  typedstruct do
  end

  def init(_init) do
    {:ok, %Pool{}}
  end

  def start_link(arg) do
    GenServer.start_link(__MODULE__, arg, Utility.name(arg))
  end

  ############################################################
  #                      Public RPC API                      #
  ############################################################

  ############################################################
  #                    Genserver Behavior                    #
  ############################################################
end
