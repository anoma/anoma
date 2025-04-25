defmodule Anoma.Controller.Transaction.Supervisor do
  @moduledoc """
  I am the supervisor for the transaction subsystem.
  """

  use Supervisor

  alias Anoma.Controller.Registry
  alias Anoma.Controller.Transaction.Mempool
  alias Anoma.Controller.Transaction.Ordering

  ############################################################
  #                       Types                              #
  ############################################################

  @typedoc """
  The type of the arguments that the supervisor expects.
  """
  @type args_t :: [
          node_id: String.t(),
          mempool: Mempool.args_t(),
          ordering: Ordering.args_t()
        ]

  ############################################################
  #                       Supervisor Implementation          #
  ############################################################

  @spec start_link(args_t) :: GenServer.on_start()
  def start_link(args) do
    Supervisor.start_link(__MODULE__, args)
  end

  @impl true
  def init(args) do
    Process.set_label(__MODULE__)

    children = [
      {Anoma.Controller.Transaction.Ordering,
       [node_id: args[:node_id]] ++ Keyword.get(args, :ordering, [])},
      {Anoma.Controller.Transaction.Storage,
       [node_id: args[:node_id]] ++ Keyword.get(args, :storage, [])},
      {Anoma.Controller.Transaction.Mempool,
       [node_id: args[:node_id]] ++ Keyword.get(args, :mempool, [])},
      {Task.Supervisor, name: Registry.via(args[:node_id], TxSupervisor)},
      {Anoma.Controller.Transaction.Executor, [node_id: args[:node_id]]}
    ]

    Supervisor.init(children, strategy: :one_for_all)
  end
end
