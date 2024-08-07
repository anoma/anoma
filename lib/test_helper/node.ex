defmodule TestHelper.Node do
  alias Anoma.Node.Transport
  alias Anoma.Node.Router
  alias Anoma.Crypto.Id

  @spec become_engine() :: any()
  @spec become_engine({Router.addr(), Router.addr()}) :: any()

  @doc """
  We fake becoming an engine

  If a router is not passed in, we make a new one
  """
  def become_engine() do
    {:ok, client_router, client_transport} = Router.start()
    become_engine({client_router, client_transport})
  end

  def become_engine({router, transport}) do
    # Test engine ID that we are faking being
    id = Anoma.Crypto.Id.new_keypair()

    # this is our fake server name
    server_name = Router.process_name(Transport, id.external)

    with {:registered_name, name} <-
           :erlang.process_info(self(), :registered_name) do
      :erlang.unregister(name)
    end

    # We receive messages at this name
    :erlang.register(server_name, self())

    Process.put(:engine_id, id.external)
    Process.put(:engine_server, server_name)
    Process.put(:engine_router, router)

    GenServer.cast(router.server, {:init_local_engine, id, server_name})
    %{id: id, server_name: server_name, router: router, transport: transport}
  end

  @spec talk_to_server_router(
          Router.addr(),
          Transport.transport_addr(),
          Id.Extern.t(),
          [Router.addr()]
        ) :: any()
  def talk_to_server_router(
        client_transport,
        server_address,
        server_router,
        engines
      ) do
    Transport.learn_node(client_transport, server_router, server_address)

    engines
    |> Enum.each(
      &Transport.learn_engine(
        client_transport,
        Router.Addr.id(&1),
        server_router
      )
    )
  end

  @spec router_talking_to_client(Router.addr(), Router.addr()) :: :ok
  def router_talking_to_client(client_router, server_transport) do
    my_id = Router.Addr.id(Router.self_addr())
    # Tell the server Ι live on the client (ensure we send to the id so the
    # message is sent remotely, since the 'local' server is likely in a
    # different image
    Transport.learn_engine(
      Router.Addr.id!(server_transport),
      my_id,
      Router.Addr.id(client_router)
    )
  end
end
