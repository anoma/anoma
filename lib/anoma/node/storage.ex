defmodule Anoma.Node.Storage do
  use Supervisor

  def start_link(init_state) do
    Supervisor.start_link(__MODULE__, init_state)
  end

  def init(name: name) do
    init(name: name, table: %Anoma.Storage{})
  end

  def init(name: name, table: table, logger: logger) do
    children = [
      {Anoma.Node.Storage.Communicator, name: name, logger: logger},
      {Anoma.Node.Storage.Ordering, name: name, table: table, logger: logger}
    ]

    Supervisor.init(children, strategy: :one_for_all)
  end

  def shutdown(supervisor), do: Supervisor.stop(supervisor, :normal)
end
