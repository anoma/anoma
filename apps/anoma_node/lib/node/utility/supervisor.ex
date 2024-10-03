defmodule Anoma.Node.Utility.Supervisor do
  @moduledoc """
  I am the supervisor for the utility subsystem.
  """

  use Supervisor

  def start_link(args) do
    Supervisor.start_link(__MODULE__, args, name: __MODULE__)
  end

  def init(_args) do
    Supervisor.init([], strategy: :one_for_all)
  end
end
