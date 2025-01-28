defmodule Anoma.Client.Application do
  @moduledoc """
  The Anoma client application acts as a proxy between a third-party client and an Anoma node.

  The client application is responsible for:
   - Connecting to a remote Anoma node.
   - Proving (todo: explain this a bit better)
   - Forwarding requests to the Anoma node.
  """

  require Logger

  use Application

  @impl true
  def start(_type, _args) do
    Logger.debug("starting client")

    children = [
      {DynamicSupervisor, name: Anoma.Client.ConnectionSupervisor}
    ]

    opts = [strategy: :one_for_one, name: Anoma.Client.Supervisor]
    Supervisor.start_link(children, opts)
  end
end
