defmodule Anoma.Node.Transport do
  use TypedStruct

  require Logger

  alias __MODULE__
  alias Anoma.Crypto.Id
  alias Anoma.Node.Router

  @type transport_type :: :quic
  @type transport_addr :: {:quic, binary(), non_neg_integer()} # host, port

  typedstruct do
    field(:router, Router.addr())
    field(:connections, %{Id.Extern.t() => MapSet.t({transport_type(), Router.addr()})}, default: %{})
    field(:servers, %{transport_addr => Router.addr()}, default: %{})
    field(:known_nodes, %{Id.Extern.t => transport_addr()}, default: %{})
    # engine => its node
    field(:known_engines, %{Id.Extern.t => Id.Extern.t()}, default: %{})
    field(:pending_messages, %{Id.Extern.t => MapSet.t(binary())})
  end

  def init(router) do
    {:ok, %Transport{router: router}}
  end

  def handle_cast({:start_server, trans = {:quic, host, port}}, _, s) do
    {:noreply,
      case Router.start_engine(s.router, Anoma.Node.Transport.Quic, {s.router, Router.self_addr(s.router), port, host}) do
        {:ok, server} -> %{s | servers: Map.put(s.servers, trans, server)}
        _ -> s
      end}
  end

  def handle_cast({:learn_node, node, trans}, _, s) do
    {:noreply,
      %{s |
        known_nodes: Map.put(s.known_nodes, node, trans),
        known_engines: Map.put(s.known_engines, node, node)}}
  end
  # if an engine claims to be associated with a node, believe it (_not_ the other way around); and of course we believe local advertisements
  def handle_cast({:learn_engine, engine, node}, %Router.Addr{server: server, id: id}, s) when server != nil or id == engine do
    Logger.info("learned engine #{inspect(engine)} is associated with node #{inspect(node)} from #{inspect(id)}")
    {:noreply,
      %{s |
        known_engines: Map.put(s.known_engines, engine, node)}}
  end
  def handle_cast({:connected, node, trans_type}, from, s) do
    Logger.info("connected to #{inspect(node)} via #{inspect(trans_type)}")
    {:noreply, learn_connection(s, node, {trans_type, from})}
  end

  def handle_cast({:send, dst, msg}, _, s) do
    node = Map.get(s.known_engines, dst)
    if node === nil do
      Logger.warning("could not queue message for #{inspect(dst)}")
      {:noreply, s}
    else
      conns = Map.get(s.connections, node)
      if conns do
        # already have an open connection; send to it (just pick the first one for now)
        Router.cast(elem(Enum.at(conns, 0), 1), {:send, node, msg})
        {:noreply, s}
      else
        # see if we can open a new connection (invariant: if we have an engine -> node, we should have a node -> transfer)
          addr = {:quic, _, _} = Map.fetch!(s.known_nodes, node)
          {:ok, conn} = Router.start_engine(s.router, Anoma.Node.Transport.Quic.Connection, {s.router, Router.self_addr(s.router), node, addr})
          Router.cast(conn, {:send, node, msg})
          {:noreply, learn_connection(s, node, {:quic, conn})}
      end
    end
  end

  def handle_call(:ping, _, s) do
    {:reply, :pong, s}
  end

  def learn_connection(s, node, conn) do
    %{s | connections: Map.update(s.connections, node, MapSet.new([conn]), fn conns -> MapSet.put(conns, conn) end)}
  end
end
