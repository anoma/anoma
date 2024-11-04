defmodule Anoma.Node.Intents.Supervisor do
  @moduledoc """
  I am the supervisor for the intents subsystem.
  """

  use Supervisor

  @spec start_link([any()]) :: GenServer.on_start()
  def start_link(args) do
    args = Keyword.validate!(args, [:node_id])
    Supervisor.start_link(__MODULE__, args)
  end

  @impl true
  def init(args) do
    Process.set_label(__MODULE__)

    children = [
      {Anoma.Node.Intents.IntentPool, [node_id: args[:node_id]]},
      {Anoma.Node.Intents.Solver, [node_id: args[:node_id]]}
    ]

    Supervisor.init(children, strategy: :one_for_all)
  end
end
