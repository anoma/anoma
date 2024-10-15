defmodule Anoma.Node.Utility.Supervisor do
  @moduledoc """
  I am the supervisor for the utility subsystem.
  """

  use Supervisor
  alias Anoma.Node.Registry

  def start_link(args) do
    name = Registry.via(args[:node_id], __MODULE__)
    Supervisor.start_link(__MODULE__, args, name: name)
  end

  def init(_args) do
    Process.set_label(__MODULE__)

    children = []
    Supervisor.init(children, strategy: :one_for_all)
  end
end
