defmodule Anoma.Node.Solver do
  use Supervisor

  def start_link(init_state) do
    Supervisor.start_link(__MODULE__, init_state)
  end

  def init(names) do
    children = [
      {Anoma.Node.Solver.Communicator,
       name: names[:name], logger: names[:logger]},
      {Anoma.Node.Solver.Solver, name: names[:name], logger: names[:logger]}
    ]

    Supervisor.init(children, strategy: :one_for_one)
  end

  def shutdown(supervisor), do: Supervisor.stop(supervisor, :normal)
end
