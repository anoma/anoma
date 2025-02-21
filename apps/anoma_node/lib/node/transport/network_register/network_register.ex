defmodule Anoma.Node.Transport.NetworkRegister do
  @moduledoc """
  I am the network registry.

  The single Network Registry engine instance maintains a database of NodeAdvert
  and TopicAdvert messages that arrive from the network on each node. For each
  known node and topic it spawns a Router Engine or a Pub/Sub Topic Engine
  instance, respectively.

  https://specs.anoma.net/main/arch/node/engines/net_registry.html

  """
  use GenServer
  use TypedStruct

  alias Anoma.Node.Registry
  alias Anoma.Node.Transport.NetworkRegister.Advert
  alias Anoma.Node.Transport.GRPC
  alias Anoma.Node.Transport.Proxy

  require Logger

  @args [:node_id, :node_config]

  ############################################################
  #                       Types                              #
  ############################################################

  @typep startup_options() :: [
           {:node_id, String.t()},
           {:node_config, map()}
         ]

  ############################################################
  #                       State                              #
  ############################################################

  typedstruct enforce: true do
    @typedoc """
    I am the state of Anoma.Node.NetworkRegistry.
    """
    field(:node_id, String.t())
    field(:node_config, map())
    field(:nodes, %{String.t() => {Advert.t(), DateTime.t()}}, default: %{})
  end

  ############################################################
  #                      GenServer Callbackse                #
  ############################################################

  @doc """
  """
  @spec start_link(startup_options) :: GenServer.on_start()
  def start_link(args \\ []) do
    name = Registry.via(args[:node_id], __MODULE__)
    GenServer.start_link(__MODULE__, args, name: name)
  end

  ############################################################
  #                      Public RPC API                      #
  ############################################################

  @doc """
  Process a node advertisement.

  A self-signed node advertisement contains the node's cryptographic identity
  and transport addresses.
  https://specs.anoma.net/main/arch/node/engines/net_registry_messages.html#examplerequest-examplereply
  """
  @spec node_advert(String.t(), String.t(), map()) :: :ok
  def node_advert(node_id, remote_node_id, advert) do
    GenServer.cast(
      Registry.via(node_id, __MODULE__),
      {:node_advert, remote_node_id, advert}
    )
  end

  @doc """
  I advertise to the given remote node, using the information I have about that
  node in my register.
  """
  def advertise_to(node_id, remote_node_id) do
    GenServer.cast(
      Registry.via(node_id, __MODULE__),
      {:advertise_to, remote_node_id}
    )
  end

  def susbcribe_to(node_id, remote_node_id, topic) do
    GenServer.cast(
      Registry.via(node_id, __MODULE__),
      {:subscribe_to, remote_node_id, topic}
    )
  end

  @doc """
  Given a node id, I lookup all the information I have about this node.
  """
  @spec lookup(String.t(), String.t()) :: map() | nil
  def lookup(node_id, remote_node_id) do
    GenServer.call(
      Registry.via(node_id, __MODULE__),
      {:lookup, remote_node_id}
    )
  end

  @spec dump_register(String.t()) :: any()
  def dump_register(node_id) do
    GenServer.call(Registry.via(node_id, __MODULE__), :dump_register)
  end

  ############################################################
  #                    Genserver Behavior                    #
  ############################################################

  @impl true
  @spec init(startup_options) ::
          {:ok, t()}
          | {:ok, t(),
             timeout | :hibernate | {:continue, continue_arg :: term}}
          | :ignore
          | {:stop, reason :: term}
  def init(args) do
    Logger.debug("#{inspect(self())} network register for #{inspect(args)}")
    Process.set_label(__MODULE__)

    args = Keyword.validate!(args, @args)

    seed_nodes = Map.get(args[:node_config], :seed_nodes, %{})

    state =
      struct(__MODULE__, Enum.into(args, %{}))
      |> remember_nodes(seed_nodes)

    # advertise to seedlist
    Enum.each(Map.keys(state.nodes), &advertise_to(state.node_id, &1))

    {:ok, state}
  end

  @impl true
  def handle_call({:lookup, remote_node_id}, _from, state) do
    {:reply, do_lookup(remote_node_id, state.nodes), state}
  end

  def handle_call(:dump_register, _from, state) do
    {:reply, state, state}
  end

  def handle_call(_message, _from, state) do
    {:reply, :ok, state}
  end

  @impl true
  def handle_cast({:node_advert, remote_node_id, advert}, state) do
    Logger.debug("node advert for #{remote_node_id} #{inspect(advert)}")

    # remember the node advert
    state = remember_node(state, remote_node_id, advert)

    # create a proxy for the new node
    create_node_proxy(state.node_id, remote_node_id, advert)

    # advertise back if necessary
    advertise_to(state.node_id, remote_node_id)

    {:noreply, state}
  end

  def handle_cast({:advertise_to, remote_node_id}, state) do
    case do_lookup(remote_node_id, state.nodes) do
      {:error, _} ->
        Logger.debug("No advert for #{remote_node_id}")
        {:noreply, state}

      {:ok, {remote_advert, _}} ->
        if time_since_last_advertisement(state.nodes, remote_node_id) > 1 do
          advertise(state.node_config, remote_node_id, remote_advert)
          nodes = mark_as_advertised_to(state.nodes, remote_node_id)
          {:noreply, %{state | nodes: nodes}}
        else
          Logger.debug("not advertising, not long enough ago")
          {:noreply, state}
        end
    end
  end

  def handle_cast({:subscribe_to, remote_node_id, topic}, state) do
    case do_lookup(remote_node_id, state.nodes) do
      {:error, _} ->
        Logger.debug("No advert for #{remote_node_id}")
        {:noreply, state}

      {:ok, {remote_advert, _}} ->
        susbcribe_to(state.node_config, remote_node_id, remote_advert, topic)
        {:noreply, state}
    end
  end

  def handle_cast(_message, state) do
    {:noreply, state}
  end

  @impl true
  def handle_info(_message, state) do
    {:noreply, state}
  end

  ############################################################
  #                    Genserver Helpers                     #
  ############################################################

  defp create_node_proxy(node_id, remote_node_id, advert) do
    # create a proxy node
    {:ok, :created} = Proxy.create(node_id, remote_node_id, advert)

    # create a transport protocol for this advert
    # i.e., a proxy via grpc and tcp
    :ok = Proxy.create_transport_protocol(node_id, remote_node_id, advert)
  end

  # remember a list of nodes based on their advertisement
  defp remember_nodes(state, adverts) do
    Enum.reduce(adverts, state, fn {node_id, advert}, state ->
      remember_node(state, node_id, advert)
    end)
  end

  # remember a node based on its advertisement
  defp remember_node(state, node_id, advert) do
    default = {advert, DateTime.from_unix!(0)}

    nodes =
      Map.update(state.nodes, node_id, default, fn {_advert, seen} ->
        {advert, seen}
      end)

    %{state | nodes: nodes}
  end

  # minutes since last advertisement to this node
  # -1 if never
  defp time_since_last_advertisement(nodes, remote_node_id) do
    case do_lookup(remote_node_id, nodes) do
      {:ok, {_, last_advert}} ->
        DateTime.diff(DateTime.utc_now(), last_advert, :minute)

      _ ->
        -1
    end
  end

  # mark a node as recently advertised
  defp mark_as_advertised_to(nodes, remote_node_id) do
    Map.update!(nodes, remote_node_id, fn {advert, _} ->
      {advert, DateTime.utc_now()}
    end)
  end

  defp do_lookup(node_id, nodes) do
    case Map.get(nodes, node_id) do
      nil ->
        {:error, :unknown_node}

      {advert, last_advert} ->
        {:ok, {advert, last_advert}}
    end
  end

  defp advertise(node_config, remote_node_id, remote_advert) do
    GRPC.Advertise.advertise(node_config, remote_node_id, remote_advert)
    # TCP.Advertise.advertise(node_config, remote_node_id, remote_advert)
    :ok
  end

  defp susbcribe_to(_node_config, _remote_node_id, _remote_advert, _topic) do
    # GRPC.PubSub.subscribe(node_config, remote_node_id, remote_advert, topic)
    # TCP.Advertise.advertise(node_config, remote_node_id, remote_advert)
    :ok
  end
end
