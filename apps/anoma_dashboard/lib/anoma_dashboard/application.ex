defmodule AnomaDashboard.Application do
  # See https://hexdocs.pm/elixir/Application.html
  # for more information on OTP Applications
  @moduledoc false

  use Application

  @impl true
  def start(_type, _args) do
    children = [
      AnomaDashboard.Telemetry,
      {Phoenix.PubSub, name: AnomaDashboard.PubSub},
      # Start a worker by calling: AnomaDashboard.Worker.start_link(arg)
      # {AnomaDashboard.Worker, arg},
      # Start to serve requests, typically the last entry
      AnomaDashboard.Endpoint
    ]

    # See https://hexdocs.pm/elixir/Supervisor.html
    # for other strategies and supported options
    opts = [strategy: :one_for_one, name: AnomaDashboard.Supervisor]
    Supervisor.start_link(children, opts)
  end

  # Tell Phoenix to update the endpoint configuration
  # whenever the application is updated.
  @impl true
  def config_change(changed, _new, removed) do
    AnomaDashboard.Endpoint.config_change(changed, removed)
    :ok
  end
end
