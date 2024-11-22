defmodule Anoma.Node.Transport.EngineProxy do
  @moduledoc """
  I am an engine proxy and I serve as the single point of contact to a remote node.

  I handle the outgoing messages for the node I proxy.
  For any given message I receive, I look for a connection to the remote node to send the message.
  These connections can be TCP connection, websockets, or any other connection type.

  The incoming messages are sent to the right engine directly by the connection.
  """
  use GenServer
  use TypedStruct

  require Logger

  alias Anoma.Node.Registry
  alias Anoma.Node.Transport.Messages

  ############################################################
  #                    State                                 #
  ############################################################

  typedstruct do
    @typedoc """
    I am the state of the proxy engine.

    My fields contain information to facilitate the connection with a remote engine.

    ### Fields
     - `:remote_id`      - the id of the remote engine.
     - `:type`           - the type of the connection (e.g., :router, :mempool, ..)
    """
    field(:node_id, String.t())
    field(:remote_node_id, String.t())
    field(:type, atom())
    field(:async_queue, [term()], default: [])
    field(:sync_queue, [term()], default: [])
  end

  ############################################################
  #                    Genserver Helpers                     #
  ############################################################

  @spec start_link([any()]) :: GenServer.on_start()
  def start_link(args) do
    args = Keyword.validate!(args, [:node_id, :remote_node_id, :type])

    # register as a type of proxy engine for a remote node, at the local registry.
    # e.g., {TCP.Server, remote_node_id}
    name = Registry.via(args[:remote_node_id], args[:type])

    GenServer.start_link(__MODULE__, args, name: name)
  end

  ############################################################
  #                      Public RPC API                      #
  ############################################################

  ############################################################
  #                    Genserver Behavior                    #
  ############################################################

  @doc """
  I am the init function for an engine proxy.
  I register this process as a proxy engine in the registry for the given remote node id.
  """
  @impl true
  def init(args) do
    Process.set_label(__MODULE__)

    Logger.debug("starting engine proxy with #{inspect(args)}")

    # trap exits to shut down cleanly.
    Process.flag(:trap_exit, true)

    args = Keyword.validate!(args, [:node_id, :remote_node_id, :type])
    state = struct(__MODULE__, Enum.into(args, %{}))

    # subscribe to discovery events
    # EventBroker.subscribe_me([%Filters.SourceModule{module: Handler}])

    {:ok, state}
  end

  # ----------------------------------------------------------------------------
  # Casts

  # @impl true
  # # @doc """
  # # I am the catch all cast handler.
  # # Since I am a proxy for a remote engine, I will simply forward this cast to the remote engine.
  # # """
  # def handle_cast(message, state) do
  #   Logger.debug("forwarding cast to remote engine")
  #   IO.inspect(message, label: "proxy cast")
  #   {:noreply, state}
  # end

  # ----------------------------------------------------------------------------
  # Calls

  @impl true
  # @doc """
  # I am the catch all call handler.
  # Since I am a proxy for a remote engine, I will simply forward this call to the remote engine.
  # """
  def handle_call(request, _from, state) do
    Logger.debug("forwarding call to remote engine")

    message = Messages.call_to_proto(request, state.type, state.node_id)

    case Registry.whereis(state.remote_node_id, state.type, :transport) do
      nil ->
        {:reply, {:error, :no_connection}, state}

      pid ->
        result = GenServer.call(pid, {:tcp_out, message})
        {:reply, result, state}
    end
  end
end
