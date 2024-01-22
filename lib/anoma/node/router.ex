defmodule Anoma.Node.Router do
  use Supervisor

  def start_link(init_state) do
    Supervisor.start_link(__MODULE__, init_state)
  end

  def init(name) do
    children = [
      {Anoma.Node.Router.Communicator, name: name, init: MapSet.new()},
      {Anoma.Node.Router.Pool, name: name, init: MapSet.new()}
    ]

    Supervisor.init(children, strategy: :one_for_one)
  end

  def shutdown(supervisor), do: Supervisor.stop(supervisor, :normal)
end
