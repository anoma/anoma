defmodule Anoma.Client.Application do
  @moduledoc """
  The Anoma client application acts as a proxy between a third-party client and an Anoma node.

  The client application is responsible for:
   - Connecting to a remote Anoma node.
   - Proving (todo: explain this a bit better)
   - Forwarding requests to the Anoma node.
  """

  use Application

  require Logger

  alias Anoma.Client
  alias Anoma.Client.ConnectionSupervisor

  @impl true
  def start(_type, _args) do
    Logger.debug("starting client")

    # get the grpc port this vm is supposed to use.
    grpc_port = Application.get_env(:anoma_client, :grpc_port)

    children = [
      # pubsub for client connections
      {Phoenix.PubSub, name: :client_pubsub},
      # the REST endpoint for external clients
      Client.Web.Endpoint,
      # the grpc endpoint to let nodes send data to the client
      {GRPC.Server.Supervisor,
       endpoint: Client.GRPC.Endpoint, port: grpc_port, start_server: true},
      # supervisor for connections to remote nodes
      {DynamicSupervisor, name: ConnectionSupervisor}
    ]

    opts = [strategy: :one_for_one, name: Client.Supervisor]
    Supervisor.start_link(children, opts)
  end
end
