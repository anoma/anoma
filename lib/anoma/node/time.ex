defmodule Anoma.Node.Time do
  @moduledoc """
  I am the Wall Clock Enfgine supervisor.
  """

  use Supervisor

  def start_link(init_state) do
    Supervisor.start_link(__MODULE__, init_state)
  end

  def init(args) do
    name = args[:name]

    children = [
      {Anoma.Node.Time.Communicator, name: name},
      {Anoma.Node.Time.Clock, name: name, start: args[:start]}
    ]

    Supervisor.init(children, strategy: :one_for_all)
  end

  def shutdown(supervisor), do: Supervisor.stop(supervisor, :normal)
end
