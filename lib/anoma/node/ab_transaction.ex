defmodule Anoma.Node.AbTransaction do
  @moduledoc """
  abtransaction supervisor
  """

  use Supervisor

  def start_link() do
    Supervisor.start_link(__MODULE__, nil, name: __MODULE__)
  end

  def init(_init_arg) do
    children = [
      Anoma.Node.AbStorage,
      Anoma.Node.AbOrdering,
      Anoma.Node.AbMempool
    ]

    Supervisor.init(children, strategy: :one_for_all)
  end
end
