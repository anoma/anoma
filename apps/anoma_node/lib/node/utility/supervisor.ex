defmodule Anoma.Node.Utility.Supervisor do
  @moduledoc """
  I am the supervisor for the utility subsystem.
  """

  use Supervisor

  require Logger

  require Logger

  def start_link(args) do
    args = Keyword.validate!(args, [:node_id])
    Supervisor.start_link(__MODULE__, args)
  end

  def init(args) do
    Process.set_label(__MODULE__)

    args = Keyword.validate!(args, [:node_id])

    children = [
      {Anoma.Node.Utility.Indexer, Keyword.take(args, [:node_id])}
    ]

    Supervisor.init(children, strategy: :one_for_all)
  end
end
