defmodule Anoma.Node do
  @moduledoc """
  I am an Anoma node.
  """

  use Supervisor

  def start_link(inital_state) do
    Supervisor.start_link(__MODULE__, inital_state)
  end

  def init(name) do
    children = [
      {Anoma.Node.Communicator, name: name},
      {Anoma.Node.Primary, init: [], name: name}
    ]

    Supervisor.init(children, strategy: :one_for_one)
  end

  def shutdown(supervisor), do: Supervisor.stop(supervisor, :normal)
end
