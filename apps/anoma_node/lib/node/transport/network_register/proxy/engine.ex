defmodule Anoma.Node.Transport.Proxy.Engine do
  @moduledoc """
  I am an engine proxy. I proxy messages for a remote engine. I am a child of
  the Node proxy.

  I exist because I will only receive messages for a specific engine and can
  therefore make it easier to know which messages are meant for which engine.
  """

  use GenServer
  use TypedStruct

  alias Anoma.Node.Registry
  alias Anoma.Node.Transport.Proxy.TransportProtocol

  require Logger

  @args [:node_id, :remote_node_id, :engine]

  ############################################################
  #                       Types                              #
  ############################################################

  @typep startup_options() :: [
           {:node_id, String.t()},
           {:remote_node_id, String.t()},
           {:engine, atom()}
         ]

  ############################################################
  #                       State                              #
  ############################################################

  typedstruct enforce: true do
    @typedoc """
    I am the state of Anoma.Node.Transport.Proxy.Engine.
    """
    field(:engine, atom())
    field(:remote_node_id, String.t())
    field(:node_id, String.t())
  end

  ############################################################
  #                      GenServer Callbackse                #
  ############################################################

  @doc """
  """
  @spec start_link(startup_options) :: GenServer.on_start()
  def start_link(args \\ []) do

    name =
      Registry.via(args[:remote_node_id], args[:engine])

    GenServer.start_link(__MODULE__, args, name: name)
  end

  ############################################################
  #                      Public RPC API                      #
  ############################################################

  ############################################################
  #                    Genserver Behavior                    #
  ############################################################

  @impl true
  @doc """
  """
  @spec init(startup_options) ::
          {:ok, t()}
          | {:ok, t(),
             timeout | :hibernate | {:continue, continue_arg :: term}}
          | :ignore
          | {:stop, reason :: term}
  def init(args) do
    Logger.debug("#{inspect(self())} proxy engine for #{inspect(args)}")
    Process.set_label(__MODULE__)

    args = Keyword.validate!(args, @args)
    state = struct(__MODULE__, Enum.into(args, %{}))
    {:ok, state}
  end

  @impl true
  def handle_call(message, _from, state) do
    IO.inspect(message, label: "engine call")

    remote_node_id = state.remote_node_id
    local_node_id = state.node_id

    # create the message struct that contains all information required to get
    # this to the remote node
    message = %{
      message: message,
      from: local_node_id,
      to: remote_node_id,
      engine: state.engine
    }

    case list_transport_protocols(remote_node_id) do
      [] ->
        Logger.error("no protocols for #{remote_node_id} #{state.engine}")
        {:reply, :ok, state}

      [transport_protocol | _] ->
        address = Registry.via(transport_protocol)
        result = TransportProtocol.call(address, message)
        {:reply, result, state}
    end
  end

  @impl true
  def handle_cast(message, state) do
    IO.inspect(message, label: "engine cast")
    {:noreply, state}
  end

  @impl true
  def handle_info(message, state) do
    IO.inspect(message, label: "engine info")
    {:noreply, state}
  end

  ############################################################
  #                    Genserver Helpers                     #
  ############################################################

  # @doc """
  # I locate a TransportProtocol process for the remote node.
  # """
  defp list_transport_protocols(node_id) do
    Registry.match(node_id, TransportProtocol)
  end
end
