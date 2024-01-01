defmodule Anoma.Node.Mempool do
  use Supervisor

  def start_link(init_state) do
    Supervisor.start_link(__MODULE__, init_state)
  end

  def init(args) do
    children = [
      {Anoma.Node.Mempool.Communicator, name: args[:name]},
      {Anoma.Node.Mempool.Primary, args}
    ]

    Supervisor.init(children, strategy: :one_for_all)
  end

  def shutdown(supervisor), do: Supervisor.stop(supervisor, :normal)
end
