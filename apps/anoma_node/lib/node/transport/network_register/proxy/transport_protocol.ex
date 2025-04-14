defmodule Anoma.Node.Transport.Proxy.TransportProtocol do
  @moduledoc """
  I am the implementation of the transport protocol engine.
  https://specs.anoma.net/pr-320/arch/node/engines/transport_protocol.html

  A Transport Protocol engine is responsible for accepting and initiating
  transport connections for one specific transport protocol, such as QUIC or
  TLS.
  """

  alias Anoma.Node.Registry
  alias Anoma.Node.Transport.NetworkRegister.Advert.GRPCAddress
  alias Anoma.Node.Transport.NetworkRegister.Advert.TCPAddress
  alias Anoma.Node.Transport.GRPC

  use GenServer
  use TypedStruct

  require Logger

  @args [:node_id, :remote_node_id, :address]

  ############################################################
  #                       Types                              #
  ############################################################

  @typep address :: GRPCAddress.t() | TCPAddress.t()

  @typep startup_options() :: [
           {:node_id, String.t()},
           {:address, address},
           {:remote_node_id, String.t()}
         ]

  ############################################################
  #                       State                              #
  ############################################################

  typedstruct enforce: true do
    @typedoc """
    I am the state of Anoma.Node.Transport.TransportProtocol.
    """
    field(:node_id, String.t())
    field(:remote_node_id, String.t())
    field(:address, address)
  end

  ############################################################
  #                      GenServer Callbackse                #
  ############################################################

  @doc """
  """
  @spec start_link(startup_options) :: GenServer.on_start()
  def start_link(args \\ []) do
    args = Keyword.validate!(args, @args)

    # determine the type of the protocol
    type =
      case args[:address] do
        %GRPCAddress{} ->
          :grpc

        %TCPAddress{} ->
          :tcp

        other ->
          raise "invalid address type given to transport protocol: #{inspect(other)}"
      end

    name = Registry.via(args[:remote_node_id], __MODULE__, type)
    GenServer.start_link(__MODULE__, args, name: name)
  end

  ############################################################
  #                      Public RPC API                      #
  ############################################################

  def call(transport_protocol, message) do
    GenServer.call(transport_protocol, {:call, message})
  end

  def cast(transport_protocol, message) do
    GenServer.cast(transport_protocol, {:cast, message})
  end

  def event(transport_protocol, event) do
    GenServer.cast(transport_protocol, {:event, event})
  end

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
    Logger.debug("#{inspect(self())} transport protocol for #{inspect(args)}")
    Process.set_label(__MODULE__)

    args = Keyword.validate!(args, @args)
    state = struct(__MODULE__, Enum.into(args, %{}))
    {:ok, state}
  end

  @impl true
  def handle_call({:call, message}, _from, state) do
    {:ok, result} = make_call(state.address, message)
    {:reply, result, state}
  end

  @impl true
  def handle_cast({:cast, message}, state) do
    make_cast(state.address, message)
    {:noreply, state}
  end

  def handle_cast({:event, %{topic: topic, event: event}}, state) do
    publish_event(state.address, topic, event)
    {:noreply, state}
  end

  @impl true
  def handle_info(_message, state) do
    {:noreply, state}
  end

  ############################################################
  #                    Genserver Helpers                     #
  ############################################################

  defp make_call(address = %GRPCAddress{}, message) do
    GRPC.Behavior.call(address, message)
  end

  defp make_call(_address, _message) do
    IO.puts("undefined")
  end

  defp make_cast(address = %GRPCAddress{}, message) do
    GRPC.Behavior.cast(address, message)
  end

  defp make_cast(_address, _message) do
    IO.puts("undefined")
  end

  defp publish_event(address = %GRPCAddress{}, topic, event) do
    GRPC.Behavior.publish(address, topic, event)
  end

  defp publish_event(_address, _topic, _message) do
    IO.puts("undefined")
  end
end
