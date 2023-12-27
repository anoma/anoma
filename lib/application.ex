defmodule Anoma.Application do
  # See https://hexdocs.pm/elixir/Application.html
  # for more information on OTP Applications
  @moduledoc false

  use Application

  @impl true
  def start(_type, _args) do
    children = [
      AnomaWeb.Telemetry,
      {DNSCluster, query: Application.get_env(:anoma, :dns_cluster_query) || :ignore},
      {Phoenix.PubSub, name: Anoma.PubSub},
      # Start the Finch HTTP client for sending emails
      {Finch, name: Anoma.Finch},
      # Start a worker by calling: Anoma.Worker.start_link(arg)
      # {Anoma.Worker, arg},
      # Start to serve requests, typically the last entry
      AnomaWeb.Endpoint
    ]

    # See https://hexdocs.pm/elixir/Supervisor.html
    # for other strategies and supported options
    opts = [strategy: :one_for_one, name: Anoma.Supervisor]
    Supervisor.start_link(children, opts)
  end

  # Tell Phoenix to update the endpoint configuration
  # whenever the application is updated.
  @impl true
  def config_change(changed, _new, removed) do
    AnomaWeb.Endpoint.config_change(changed, removed)
    :ok
  end
end
