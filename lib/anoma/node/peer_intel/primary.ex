defmodule Anoma.Node.PeerIntel.Primary do
  @moduledoc """
  I am part of the Networking Machine. I maintain a database of known
  nodes

  ```mermaid
  graph BT
      C(Client #3) ~~~ B(Client #2) ~~~ A(Client #1)
      A & B & C -->|request| GenServer
      GenServer -.->|reply| A & B & C
  ```
  """

  alias Anoma.Networking.Identity.External
  alias Anoma.Node.Utility
  use GenServer

  @type address() :: {:inet.ip_address(), integer()}

  def init(_args) do
    {:ok, %{}}
  end

  def start_link(arg) do
    GenServer.start_link(__MODULE__, arg, Utility.name(arg))
  end

  ############################################################
  #                      Public RPC API                      #
  ############################################################

  @spec lookup_identity(GenServer.server(), External.t()) :: :ok
  def lookup_identity(server, id) do
    GenServer.cast(server, {:lookup_identity, id})
  end

  @spec update_identity(GenServer.server(), External.t(), [address()]) :: :ok
  def update_identity(server, id, addresses) do
    GenServer.cast(server, {:update_identity, id, addresses})
  end

  @spec update_identity(GenServer.server(), External.t(), [address()], byte()) ::
          :ok
  def update_identity(server, id, addresses, trust) do
    GenServer.cast(server, {:update_identity, id, addresses, trust})
  end

  ############################################################
  #                    Genserver Behavior                    #
  ############################################################

  def handle_cast({:lookup_identity, _id}, state) do
    {:noreply, state}
  end

  def handle_cast({:update_identity, _id, _addresses}, state) do
    {:noreply, state}
  end

  def handle_cast({:update_identity, _id, _addresses, _trust}, state) do
    {:noreply, state}
  end
end
