defmodule Anoma.Node.Control.Measure do
  @moduledoc """
  I supervise the measurement communicator and engine
  """

  use Supervisor

  def start_link(init_state) do
    Supervisor.start_link(__MODULE__, init_state)
  end

  def init(args) do
    children = [
      {Anoma.Node.Control.Measure.Communicator, name: args[:name]},
      {Anoma.Node.Control.Measure.Engine,
       name: args[:name], storage: args[:storage]}
    ]

    Supervisor.init(children, strategy: :one_for_one)
  end

  def shutdown(supervisor), do: Supervisor.stop(supervisor, :normal)
end
