defmodule Anoma.Node.Transport.Proxy.TransportProtocol do
  @moduledoc """
  I am the implementation of the transport protocol engine.
  https://specs.anoma.net/pr-320/arch/node/engines/transport_protocol.html

  A Transport Protocol engine is responsible for accepting and initiating
  transport connections for one specific transport protocol, such as QUIC or
  TLS.
  """

  alias Anoma.Node.Registry
  alias Anoma.Node.Transport.IntraNode
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
    field(:channel, IntraNode.connection() | nil, default: nil)
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

  @spec call(GenServer.server(), term()) :: term()
  def call(transport_protocol, message) do
    GenServer.call(transport_protocol, {:call, message})
  end

  @spec cast(GenServer.server(), term()) :: :ok
  def cast(transport_protocol, message) do
    GenServer.cast(transport_protocol, {:cast, message})
  end

  @spec event(GenServer.server(), term()) :: :ok
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
    case ensure_channel(state) do
      {:ok, channel, state} ->
        case GRPC.Behavior.call(channel, message) do
          {:ok, result} ->
            {:reply, result, state}

          {:error, reason} ->
            {:reply, {:error, reason}, %{state | channel: nil}}
        end

      {:error, reason, state} ->
        {:reply, {:error, reason}, state}
    end
  end

  @impl true
  def handle_cast({:cast, message}, state) do
    {:noreply, dispatch(state, &GRPC.Behavior.cast(&1, message))}
  end

  def handle_cast({:event, %{topic: topic, event: event}}, state) do
    {:noreply, dispatch(state, &GRPC.Behavior.publish(&1, topic, event))}
  end

  @impl true
  def handle_info(_message, state) do
    {:noreply, state}
  end

  ############################################################
  #                    Genserver Helpers                     #
  ############################################################

  # I run `fun` with an established channel, dropping the cached
  # channel on failure so the next send reconnects.
  @spec dispatch(t(), (IntraNode.connection() -> :ok | {:error, term()})) ::
          t()
  defp dispatch(state, fun) do
    case ensure_channel(state) do
      {:ok, channel, state} ->
        case fun.(channel) do
          :ok ->
            state

          {:error, reason} ->
            Logger.warning("transport send failed: #{inspect(reason)}")
            %{state | channel: nil}
        end

      {:error, reason, state} ->
        Logger.warning(
          "transport connect to #{state.remote_node_id} failed: " <>
            "#{inspect(reason)}"
        )

        state
    end
  end

  # I return the cached channel, connecting (and caching) on first
  # use and reusing it thereafter. Only GRPC addresses are supported.
  @spec ensure_channel(t()) ::
          {:ok, IntraNode.connection(), t()} | {:error, term(), t()}
  defp ensure_channel(state = %__MODULE__{channel: channel})
       when channel != nil do
    {:ok, channel, state}
  end

  defp ensure_channel(state = %__MODULE__{address: %GRPCAddress{} = address}) do
    case GRPC.Behavior.connect(address) do
      {:ok, channel} -> {:ok, channel, %{state | channel: channel}}
      {:error, reason} -> {:error, reason, state}
    end
  end

  defp ensure_channel(state) do
    {:error, :unsupported_transport, state}
  end
end
