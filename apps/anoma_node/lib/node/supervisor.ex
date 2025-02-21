defmodule Anoma.Node.Supervisor do
  @moduledoc """
  I am the top level supervisor for the Anoma node.
  """

  use Supervisor

  require Logger

  @args [:node_id, :tx_args, :node_config]

  ############################################################
  #                       Types                              #
  ############################################################

  @typep startup_options() :: [
           {:node_id, String.t()}
           | {:tx_args, any()}
           | {:node_config, map()}
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

  @spec start_link(startup_options) :: term()
  def start_link(args) do
    args = Keyword.validate!(args, @args)
    name = Anoma.Node.Registry.via(args[:node_id], __MODULE__)
    Supervisor.start_link(__MODULE__, args, name: name)
  end

  @impl true
  def init(args) do
    Logger.debug("starting node with #{inspect(args)}")
    Process.set_label(__MODULE__)

    args = Keyword.validate!(args, @args)

    children = [
      {Anoma.Node.Transport.Supervisor,
       [node_id: args[:node_id], node_config: args[:node_config]]},
      {Anoma.Node.Transaction.Supervisor,
       [node_id: args[:node_id], tx_args: args[:tx_args]]},
      {Anoma.Node.Intents.Supervisor, node_id: args[:node_id]},
      {Anoma.Node.Logging, node_id: args[:node_id]}
    ]

    Supervisor.init(children, strategy: :one_for_all)
  end
end
