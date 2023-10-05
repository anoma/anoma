defmodule Anoma.Node.Mempool do
  @moduledoc """
  I am the Mempool
  """

  use Supervisor

  def start_link(init_state) do
    Supervisor.start_link(__MODULE__, init_state)
  end

  def init(args) do
    {name, exec, intent} = {args[:name], args[:executor], args[:intent]}

    children = [
      {Anoma.Node.Mempool.Communicator, name: name, executor: exec, intent: intent},
      {Anoma.Node.Mempool.Pool, name: name}
    ]

    Supervisor.init(children, strategy: :one_for_one)
  end
end
