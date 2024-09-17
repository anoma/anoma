defmodule Anoma.Node.Transport.Supervisor do
  @moduledoc """
  I am the Transport Supervisor module.

  I serve as a supervision pool for various transport connections.
  My instance is created by the Anoma Node.
  """

  use DynamicSupervisor

  def init(_arg) do
    DynamicSupervisor.init(
      strategy: :one_for_one,
      # max_restarts: 10_000_000,
      # max_restarts: 10_000_000,
      max_seconds: 1
    )
  end

  @spec start_link(any()) :: GenServer.on_start()
  def start_link(argument) do
    DynamicSupervisor.start_link(__MODULE__, argument)
  end
end
