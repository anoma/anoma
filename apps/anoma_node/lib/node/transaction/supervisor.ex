defmodule Anoma.Node.Transaction.Supervisor do
  @moduledoc """
  I am the supervisor for the transaction subsystem.
  """

  use Supervisor

  def start_link(args) do
    args = Keyword.validate!(args, [:node_id])
    Supervisor.start_link(__MODULE__, args)
  end

  def init(args) do
    children = [
      {Anoma.Node.Transaction.Mempool, [node_id: args[:node_id]]}
    ]

    Supervisor.init(children, strategy: :one_for_all)
  end
end
