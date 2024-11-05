defmodule Anoma.Node.Transaction.Supervisor do
  @moduledoc """
  I am the supervisor for the transaction subsystem.
  """

  use Supervisor

  @spec start_link(list({:node_id, String.t()} | {:tx_args, any()})) ::
          GenServer.on_start()
  def start_link(args) do
    args = Keyword.validate!(args, [:node_id, :tx_args])
    Supervisor.start_link(__MODULE__, args)
  end

  @impl true
  def init(args) do
    Process.set_label(__MODULE__)

    tx_args = args[:tx_args]

    children = [
      {Anoma.Node.Transaction.Executor, [node_id: args[:node_id]]},
      {Anoma.Node.Transaction.Ordering,
       [node_id: args[:node_id]] ++ tx_args[:ordering]},
      {Anoma.Node.Transaction.Storage,
       [node_id: args[:node_id]] ++ tx_args[:storage]},
      {Anoma.Node.Transaction.Mempool,
       [node_id: args[:node_id]] ++ tx_args[:mempool]}
    ]

    Supervisor.init(children, strategy: :one_for_all)
  end
end
