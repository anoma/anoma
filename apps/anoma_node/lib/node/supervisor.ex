defmodule Anoma.Node.Supervisor do
  @moduledoc """
  I am the top level supervisor for the Anoma node.
  """

  require Logger

  use Supervisor

  alias Anoma.Node.Intents
  alias Anoma.Node.Logging
  alias Anoma.Node.Transaction
  alias Anoma.Node.Transport

  @doc """
  The default arguments for the supervisor.
  """
  @args [
    :node_id,
    :transaction,
    :node_config,
    replay: true,
    transaction: [mempool: []]
  ]

  ############################################################
  #                       Types                              #
  ############################################################

  @type args_t() :: [
          {:node_id, String.t()}
          | {:replay, boolean()}
          | {:node_config, map()}
          | {:transaction, any()}
        ]

  ############################################################
  #                      Supervisor Callbacks                #
  ############################################################

  @spec child_spec(any()) :: map()
  def child_spec(args) do
    %{
      id: __MODULE__,
      start: {__MODULE__, :start_link, [args]},
      restart: :temporary
    }
  end

  @spec start_link(args_t) :: any()
  def start_link(args) do
    args = Keyword.validate!(args, @args)
    name = Anoma.Node.Registry.via(args[:node_id], __MODULE__)
    Supervisor.start_link(__MODULE__, args, name: name)
  end

  @impl true
  @spec init(args_t) :: any()
  def init(args) do
    Logger.info("starting node with #{inspect(args)}")
    Process.set_label(__MODULE__)

    # validate arguments
    args = Keyword.validate!(args, @args)

    node_id = args[:node_id]
    transaction = args[:transaction]

    children = [
      {Transport.Supervisor,
       node_id: node_id, node_config: args[:node_config]},
      {Transaction.Supervisor, [node_id: node_id] ++ transaction},
      {Intents.Supervisor, node_id: node_id},
      {Logging, node_id: node_id}
    ]

    Supervisor.init(children, strategy: :one_for_all)
  end
end
