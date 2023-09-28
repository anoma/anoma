defmodule Anoma.Node.Intent do
  @moduledoc """
  I am an intent node.

  I supervise an intent pool, creating nodes to talk to

  I expose a name, so that one can run many intent pools on one system
  for testing, or for running
  """

  use Supervisor

  def start_link(init_state) do
    Supervisor.start_link(__MODULE__, init_state)
  end

  def init(name) do
    children = [
      {Anoma.Node.Intent.Communicator, name: name, init: MapSet.new()},
      {Anoma.Node.Intent.Pool, name: name, init: MapSet.new()}
    ]

    Supervisor.init(children, strategy: :one_for_one)
  end

  def shutdown(supervisor), do: Supervisor.stop(supervisor, :normal)
end
