defmodule Anoma.Node.Solver do
  use Supervisor

  def start_link(init_state) do
    Supervisor.start_link(__MODULE__, init_state)
  end

  def init(name) do
    children = [
      {Anoma.Node.Solver.Communicator, name: name},
      {Anoma.Node.Solver.Solver, name: name}
    ]

    Supervisor.init(children, strategy: :one_for_one)
  end

  def shutdown(supervisor), do: Supervisor.stop(supervisor, :normal)
end
