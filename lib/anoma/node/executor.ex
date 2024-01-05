defmodule Anoma.Node.Executor do
  @moduledoc """
  I am an Anoma Executor node.

  I supervise over two kinds of processes
     1. A communicator that you should talk to
     2. A dynamic worker pool that one can spawn transactions on

  For starting a service, you should send in:
    - a name for process registration
    - variables for how Nock is supposed to be ran
      + The important one are `:ordering` and `:snapshot_path`
  """

  use Supervisor

  def start_link(inital_state) do
    Supervisor.start_link(__MODULE__, inital_state)
  end

  def init(args) do
    children = [
      {Anoma.Node.Executor.Communicator, args},
      {Task.Supervisor, name: args[:name]}
    ]

    Supervisor.init(children, strategy: :one_for_one)
  end

  def shutdown(supervisor), do: Supervisor.stop(supervisor, :normal)
end
