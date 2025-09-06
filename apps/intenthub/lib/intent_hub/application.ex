defmodule IntentHub.Application do
  @moduledoc """
  IntentHub Application - Intent registry and management system for Anoma
  """

  use Application

  @impl true
  def start(_type, _args) do
    children = [
      IntentHub.IntentRegistry
    ]

    opts = [strategy: :one_for_one, name: IntentHub.Supervisor]
    Supervisor.start_link(children, opts)
  end
end
