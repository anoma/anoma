defmodule Client.Application do
  @moduledoc """
  The Anoma client application acts as a proxy between a third-party client and an Anoma node.

  The client application is responsible for:
   - Connecting to a remote Anoma node.
   - Proving (todo: explain this a bit better)
   - Forwarding requests to the Anoma node.
  """

  use Application

  require Logger

  @impl true
  def start(_type, _args) do
    args = fetch_args()

    Logger.debug("starting client with args: #{inspect(args)}")

    children = [
      {DynamicSupervisor, name: Client.ConnectionSupervisor},
      {GRPC.Server.Supervisor,
       endpoint: Client.Api.Endpoint,
       port: args[:listen_port],
       start_server: true}
    ]

    opts = [strategy: :one_for_one, name: Client.Supervisor]
    Supervisor.start_link(children, opts)
  end

  @spec fetch_args() :: Keyword.t()
  defp fetch_args() do
    listen_port =
      (System.get_env("LISTEN_PORT") || "50052") |> String.to_integer()

    [listen_port: listen_port]
  end
end
