defmodule Anoma.Node.Executor do
  @moduledoc """
  I am an incomplete Anoma Executor node.
  """

  use Supervisor

  def start_link(inital_state) do
    Supervisor.start_link(__MODULE__, inital_state)
  end

  def init(name) do
    children = [
      {Anoma.Node.Executor.Communicator, name: name},
      {Anoma.Node.Executor.Primary, init: [], name: name}
    ]

    Supervisor.init(children, strategy: :one_for_one)
  end

  def shutdown(supervisor), do: Supervisor.stop(supervisor, :normal)
end
