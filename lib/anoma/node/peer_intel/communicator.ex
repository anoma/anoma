defmodule Anoma.Node.PeerIntel.Communicator do
  use Anoma.Communicator, sub_field: :subscribers
  alias Anoma.Communicator, as: ACom

  alias __MODULE__
  use TypedStruct
  alias Anoma.Node.Utility
  alias Anoma.Node.PeerIntel.Primary
  alias Anoma.Networking.Identity.External

  typedstruct do
    field(:subscribers, ACom.t(), default: ACom.new())
    field(:primary, atom(), require: true)
  end

  def init(name: name, init: subscribers) do
    {:ok, %Communicator{primary: name, subscribers: subscribers}}
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

  @spec lookup_identity(GenServer.server(), External.t()) :: :ok
  @spec update_identity(GenServer.server(), External.t(), [Primary.address()]) ::
          :ok
  @spec update_identity(
          GenServer.server(),
          External.t(),
          [Primary.address()],
          byte()
        ) ::
          :ok

  defdelegate lookup_identity(communicator, id), to: Primary
  defdelegate update_identity(communicator, id, addresses), to: Primary
  defdelegate update_identity(communicator, id, addresses, trust), to: Primary

  ############################################################
  #                    Genserver Behavior                    #
  ############################################################

  def handle_cast({:lookup_identity, id}, state) do
    Primary.lookup_identity(state.primary, id)
    {:noreply, state}
  end

  def handle_cast({:update_identity, id, addresses}, state) do
    Primary.update_identity(state.primary, id, addresses)
    {:noreply, state}
  end

  def handle_cast({:update_identity, id, addresses, trust}, state) do
    Primary.update_identity(state.primary, id, addresses, trust)
    {:noreply, state}
  end
end
