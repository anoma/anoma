defmodule Anoma.Node.Transport do
  alias __MODULE__
  alias Anoma.Node.Transport.Connection
  alias Anoma.Node.Router
  alias Anoma.Node.Logger
  alias Anoma.Crypto.Id

  use Router.Engine
  use TypedStruct

  # cap messages at 1mb.  todo should be a configuration parameter, maybe more sophisticated policy, etc.
  @max_message_size 1_000_000

  @type transport_type :: :tcp | :unix
  # host, port
  @type transport_addr ::
          {:tcp, binary(), non_neg_integer()}
          # file path
          | {:unix, binary()}
  # host, port
  @type listen_addr ::
          {:tcp, binary(), non_neg_integer()}
          # file path
          | {:unix, binary()}

  typedstruct module: ConnectionState do
    field(:type, Anoma.Node.Transport.transport_type())
    field(:connection, Router.addr())

    field(
      :state,
      :pending | :handshake_first | :handshake_second | :connected
    )

    # filled in as soon as it's known
    field(:id, Id.Extern.t() | nil)
    field(:partial_message, binary(), default: "")
    # could clean these up once the handshake is over
    field(:outgoing_nonce, binary())
    field(:incoming_nonce, binary() | nil)
  end

  typedstruct do
    field(:router, Router.addr())
    field(:logger, Router.addr())
    field(:node_internal_id, Id.t())
    field(:transport_internal_id, Id.t())
    field(:connection_pool, Supervisor.supervisor())

    field(:node_connections, MapSetMap.t(Id.Extern.t(), Router.addr()),
      default: MapSetMap.new()
    )

    field(:connection_states, %{Router.addr() => ConnectionState.t()},
      default: %{}
    )

    # node -> messages that we would like to send to it, but we haven't
    # established a connection to it yet
    # could just as well be a MapSet
    field(:pending_outgoing_messages, %{Id.Extern.t() => [binary()]},
      default: %{}
    )

    # engine -> messages we would like to send to it, but we don't know which
    # node it belongs to yet
    field(:pending_outgoing_engine_messages, %{Id.Extern.t() => [binary()]},
      default: %{}
    )

    # a transport server we are currently running -> a set of external
    # transport addresses by which messages might reach us through this server
    field(:servers, %{transport_addr => Router.addr()}, default: %{})

    # pidgin network identity store; should become its own engine
    field(:known_nodes, MapSetMap.t(Id.Extern.t(), transport_addr()),
      default: MapSetMap.new()
    )

    # engine => its node
    field(:known_engines, %{Id.Extern.t() => Id.Extern.t()}, default: %{})
  end

  def init({router, node_internal_id, transport_id, transport_pool}) do
    {:ok,
     %Transport{
       router: router,
       node_internal_id: node_internal_id,
       transport_internal_id: transport_id,
       connection_pool: transport_pool
     }}
  end

  @spec transport_type(transport_addr()) :: transport_type()
  def transport_type(addr) do
    elem(addr, 0)
  end

  @doc """
  You are a connection, and want to notify the transport that you have received
  a chunk of data.
  """
  @spec receive_chunk(Router.addr(), binary()) :: :ok
  def receive_chunk(transport, chunk) do
    Router.cast(transport, {:receive_chunk, chunk})
  end

  @doc """
  Attempt to start a new transport server listening on the specified address.
  """
  @spec start_server(Router.addr(), listen_addr()) :: :ok
  def start_server(transport, addr) do
    Router.cast(transport, {:start_server, addr})
  end

  @doc """
  Notify the transport that the specified node can be reached by the specified
  transport address.
  """
  @spec learn_node(Router.addr(), Id.Extern.t(), transport_addr()) :: :ok
  def learn_node(transport, id, addr) do
    Router.cast(transport, {:learn_node, id, addr})
  end

  @doc """
  Notify the transport that the specified engine lives on the specified node.
  """
  @spec learn_engine(Router.addr(), Id.Extern.t(), Id.Extern.t()) :: :ok
  def learn_engine(transport, engine, node) do
    Router.cast(transport, {:learn_engine, engine, node})
  end

  @doc """
  Attempt to send the specified message to the node with the specified id,
  somehow.
  """
  @spec send(Router.addr(), Id.Extern.t(), binary()) :: :ok
  def send(transport, dst, msg) do
    Router.cast(transport, {:send, dst, msg})
  end

  @doc """
  Notify the transport that you, a local engine, are now managing a new
  incoming connection of the specified type.
  """
  @spec new_connection(Router.addr(), transport_type()) :: :ok
  def new_connection(transport, trans_type) do
    Router.cast(transport, {:new_connection, trans_type})
  end

  # notify the transport that the connection you were managing has died
  @spec disconnected(Router.addr(), binary()) :: :ok
  def disconnected(transport, reason) do
    Router.cast(transport, {:disconnected, reason})
  end

  def handle_cast({:receive_chunk, chunk}, from, s) do
    state = Map.get(s.connection_states, from)
    # message still in queue from dropped connection
    if state == nil do
      {:noreply, s}
    else
      # over-large message; drop connection
      if byte_size(state.partial_message) + byte_size(chunk) >
           @max_message_size do
        {:noreply,
         drop_connection(
           s,
           from,
           "received overlarge message (limit #{inspect(@max_message_size)})"
         )}
      else
        {:noreply, handle_recv_chunk(s, from, state.partial_message <> chunk)}
      end
    end
  end

  def handle_cast({:receive_datum, from, msg}, _, s) do
    state = Map.get(s.connection_states, from)

    if state == nil do
      # stale message in queue from dropped connection; ignore (in principle,
      # we could distinguish between connections that are dropped for good vs
      # bad reasons, and still handle messages in the former case, but
      # currently we don't)
      {:noreply, s}
    else
      case state.state do
        # common case: connection to node with known id
        :connected ->
          Router.cast(s.router, {:p2p_raw, state.id, msg})
          {:noreply, s}

        # handshaking connection
        _ ->
          {:noreply, handle_handshake(s, msg, state)}
      end
    end
  end

  def handle_cast({:new_connection, trans_type}, from, s) do
    state =
      Map.get(s.connection_states, from, %ConnectionState{
        type: trans_type,
        connection: from
      })

    state = %{state | state: :handshake_first, outgoing_nonce: new_nonce()}
    s = %{s | connection_states: Map.put(s.connection_states, from, state)}
    send_initial_handshake(s, state)
    {:noreply, s}
  end

  def handle_cast({:start_server, trans}, _, s) do
    {:noreply,
     case Router.start_engine(
            s.router,
            trans_server_mod(transport_type(trans)),
            {s.router, Router.self_addr(s.router), trans, s.connection_pool},
            supervisor: s.connection_pool
          ) do
       {:ok, server} -> %{s | servers: Map.put(s.servers, trans, server)}
       _ -> s
     end}
  end

  def handle_cast({:learn_node, node, addr}, _, s) do
    s = %{
      s
      | known_nodes: MapSetMap.add(s.known_nodes, node, addr)
    }

    s =
      if Map.has_key?(s.pending_outgoing_messages, node) do
        # have anything to send?  try to initiate a connection
        add_connection(s, node, addr)
      else
        s
      end

    # and, of course, a node's id is also the id of its router, so learn that
    handle_cast({:learn_engine, node, node}, Router.self_addr(s.router), s)
  end

  # if an engine claims to be associated with a node, believe it (_not_ the other way around); and of course we believe local advertisements
  # todo condition should perhaps be id signsfor engine, not id == engine?
  def handle_cast(
        {:learn_engine, engine, node},
        %Router.Addr{server: server, id: id},
        s
      )
      when server != nil or id == engine do
    log_info({:learned_engine, engine, node, id, s.logger})
    s = %{s | known_engines: Map.put(s.known_engines, engine, node)}

    {messages, pending_outgoing_engine_messages} =
      Map.pop(s.pending_outgoing_engine_messages, engine, [])

    Enum.each(messages, &send(Router.self_addr(s.router), engine, &1))

    {:noreply,
     %{s | pending_outgoing_engine_messages: pending_outgoing_engine_messages}}
  end

  def handle_cast({:disconnected, reason}, from, s) do
    {:noreply, connection_dropped(s, from, reason)}
  end

  def handle_cast({:send, dst, msg}, _, s) do
    {:noreply, send_msg(s, dst, msg)}
  end

  def handle_call(:ping, _, s) do
    {:reply, :pong, s}
  end

  def handle_recv_chunk(s, from, data) do
    case :msgpack.unpack_stream(data) do
      # message still incomplete; enqueue (todo: the partial_message <> chunk
      # catenation above could force us to do quadratic work if they send a
      # large message one byte at a time; need to write a custom msgpack
      # decoder to deal with this; maybe want one anyway due to type-awareness)
      {:error, :incomplete} ->
        %{
          s
          | connection_states:
              Map.update!(s.connection_states, from, fn state ->
                %{state | partial_message: data}
              end)
        }

      # mal-encoded message; drop the connection summarily
      {:error, err} ->
        drop_connection(s, from, "msgpack encoding error #{inspect(err)}")

      {msg, rest} ->
        Router.cast(Router.self_addr(s.router), {:receive_datum, from, msg})
        handle_recv_chunk(s, from, rest)
    end
  end

  defp send_initial_handshake(s, %ConnectionState{
         connection: conn,
         outgoing_nonce: nonce
       }) do
    # need a detached signature because the other side may not a priori know
    # our id, so doesn't know what to verify against
    packed =
      Anoma.Serialise.pack(%{nonce: nonce, id: s.node_internal_id.external})

    signature =
      Anoma.Crypto.Sign.sign_detached(
        packed,
        s.node_internal_id.internal.sign
      )

    message = Anoma.Serialise.pack(%{data: packed, sig: signature})
    Connection.send(conn, message)
  end

  @spec learn_connection(t(), Router.addr(), Id.Extern.t()) :: t()
  defp learn_connection(s, conn, id) do
    # if there were any messages we were waiting to send to this node, send them now
    {messages, pending_outgoing_messages} =
      Map.pop(s.pending_outgoing_messages, id, [])

    Enum.each(messages, &Connection.send(conn, &1))
    state = Map.fetch!(s.connection_states, conn)
    log_info({:connected, state.id, state.type, s.logger})

    %{
      s
      | node_connections: MapSetMap.add(s.node_connections, id, conn),
        connection_states:
          Map.update!(s.connection_states, conn, fn state ->
            %{state | state: :connected}
          end),
        pending_outgoing_messages: pending_outgoing_messages
    }
  end

  @spec drop_connection(t(), Router.addr(), binary()) :: t()
  defp drop_connection(s, conn, reason) do
    Connection.shutdown(conn)
    connection_dropped(s, conn, reason)
  end

  @spec connection_dropped(t(), Router.addr(), binary()) :: t()
  defp connection_dropped(s, conn, reason) do
    log_info({:disconnected, conn, reason, s.logger})
    {state, connection_states} = Map.pop(s.connection_states, conn)

    node_connections =
      if state do
        MapSetMap.remove(s.node_connections, state.id, conn)
      else
        s.node_connections
      end

    %{
      s
      | connection_states: connection_states,
        node_connections: node_connections
    }
  end

  @spec handle_handshake(t(), term(), ConnectionState.t()) :: t()
  defp handle_handshake(
         s,
         data,
         state = %ConnectionState{connection: conn, state: handshake_stage}
       ) do
    case handshake_stage do
      :handshake_first ->
        case handle_initial_handshake(s, data, state) do
          {:ok, state} ->
            %{
              s
              | connection_states: Map.put(s.connection_states, conn, state)
            }

          err ->
            drop_connection(
              s,
              conn,
              "failed initial handshake: #{inspect(err)}"
            )
        end

      :handshake_second ->
        if handle_second_handshake(s, data, state) do
          learn_connection(s, conn, state.id)
        else
          drop_connection(s, conn, "failed second stage handshake")
        end
    end
  end

  defp handle_initial_handshake(
         s,
         data,
         state = %ConnectionState{connection: conn}
       ) do
    with {:ok, %{data: packed, sig: signature}} <-
           Anoma.Serialise.from_msgpack(data) do
      with {:ok, %{nonce: other_nonce, id: other_id}} <-
             Anoma.Serialise.unpack(packed) do
        # todo check structure of nonce/id
        if (state.id == nil || state.id === other_id) &&
             Anoma.Crypto.Sign.verify_detached(
               signature,
               packed,
               other_id.sign
             ) do
          second_stage =
            Anoma.Serialise.pack(%{
              mynonce: state.outgoing_nonce,
              yournonce: other_nonce,
              yourid: other_id
            })

          signature =
            Anoma.Crypto.Sign.sign_detached(
              second_stage,
              s.node_internal_id.internal.sign
            )

          message =
            Anoma.Serialise.pack(%{data: second_stage, sig: signature})

          Connection.send(conn, message)

          {:ok,
           %{
             state
             | state: :handshake_second,
               id: other_id,
               incoming_nonce: other_nonce
           }}
        else
          nil
        end
      end
    end
  end

  defp handle_second_handshake(s, data, state) do
    with %{data: packed, sig: signature} <- data do
      with {:ok,
            %{mynonce: incoming_nonce, yournonce: outgoing_nonce, yourid: id}} <-
             Anoma.Serialise.unpack(packed) do
        incoming_nonce === state.incoming_nonce &&
          outgoing_nonce === state.outgoing_nonce &&
          id === s.node_internal_id.external &&
          Anoma.Crypto.Sign.verify_detached(signature, packed, state.id.sign)
      end
    end
  end

  @spec send_msg(t(), Id.Extern.t(), binary()) :: t()
  defp send_msg(s, dst, msg) do
    node = Map.get(s.known_engines, dst)

    if node === nil do
      log_info({:queued, dst, nil, nil, s.logger})
      # todo limit queue size
      %{
        s
        | pending_outgoing_engine_messages:
            Map.update(
              s.pending_outgoing_engine_messages,
              dst,
              [msg],
              fn msgs -> [msg | msgs] end
            )
      }
    else
      conns = MapSetMap.get(s.node_connections, node)

      if MapSet.size(conns) > 0 do
        # if we have an active connection, send to it (just pick one for now);
        # if all the connections are still pending, then enqueue
        active_conn =
          Enum.find(conns, fn conn ->
            Map.fetch!(s.connection_states, conn).state == :connected
          end)

        if active_conn != nil do
          Connection.send(active_conn, msg)
          s
        else
          %{
            s
            | pending_outgoing_messages:
                Map.update(
                  s.pending_outgoing_messages,
                  node,
                  [msg],
                  fn msgs -> [msg | msgs] end
                )
          }
        end
      else
        # no current connections--whether pending or active--so try to open one
        # just pick the first address and try to open a new connection to it
        # todo more sophisticated strategy--if the first address doesn't work,
        # try more; ask if anyone else knows more addresses; tune effort
        # according to message priority (qos and
        # https://github.com/anoma/anoma/issues/328 again); exponential backoff
        addr = Enum.at(MapSetMap.get(s.known_nodes, node), 0)

        if addr != nil do
          log_info({:queued, dst, node, addr, s.logger})
          add_connection(s, node, addr)
        else
          # todo attempt to discover an address; should limit outgoing queue
          # size, with lower priority for messages for nodes whose addresses we
          # don't know
          log_info({:queued, dst, node, nil, s.logger})
          s
        end

        %{
          s
          | pending_outgoing_messages:
              Map.update(
                s.pending_outgoing_messages,
                node,
                [msg],
                fn msgs -> [msg | msgs] end
              )
        }
      end
    end
  end

  @spec add_connection(t(), Id.Extern.t(), transport_addr()) :: t()
  defp add_connection(s, node, addr) do
    conn = init_connection(s, addr)

    state = %ConnectionState{
      type: transport_type(addr),
      id: node,
      state: :pending
    }

    %{
      s
      | connection_states: Map.put(s.connection_states, conn, state),
        node_connections: MapSetMap.add(s.node_connections, node, conn)
    }
  end

  @spec init_connection(t(), transport_addr()) ::
          {Router.addr(), transport_type()} | nil
  defp init_connection(s = %__MODULE__{}, trans) do
    case Router.start_engine(
           s.router,
           trans_connection_mod(transport_type(trans)),
           {:client, s.router, Router.self_addr(s.router), trans,
            s.connection_pool},
           supervisor: s.connection_pool
         ) do
      {:ok, addr} -> {addr, transport_type(trans)}
      _ -> nil
    end
  end

  defp trans_connection_mod(type) do
    %{
      tcp: Transport.TCPConnection,
      unix: Transport.TCPConnection
    }[type]
  end

  defp trans_server_mod(type) do
    %{
      tcp: Transport.TCPServer,
      unix: Transport.TCPServer
    }[type]
  end

  defp new_nonce() do
    :crypto.strong_rand_bytes(32)
  end

  defp log_info({:queued, engine, node, addr, logger}) do
    Logger.add(
      logger,
      :debug,
      "queued message for engine #{inspect(engine)} at node #{if node do
        inspect(node)
      else
        "unknown"
      end} via transport address #{if addr do
        inspect(addr)
      else
        "unknown"
      end}"
    )
  end

  defp log_info({:connected, node_id, trans_type, logger}) do
    Logger.add(
      logger,
      :debug,
      "Connected to node #{inspect(node_id)} via #{inspect(trans_type)}"
    )
  end

  defp log_info({:disconnected, conn, reason, logger}) do
    Logger.add(
      logger,
      :debug,
      "dropped connection #{inspect(conn)}: #{reason}"
    )
  end

  defp log_info({:learned_engine, engine, node, id, logger}) do
    Logger.add(
      logger,
      :debug,
      "Learned engine #{inspect(engine)} is associated with node #{inspect(node)} from #{inspect(id)}"
    )
  end
end
