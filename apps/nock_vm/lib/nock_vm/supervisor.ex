defmodule NockVM.Supervisor do
  @moduledoc """
  I am the supervisor over Nock VMs.
  """

  use Supervisor

  def start_link(args) do
    Supervisor.start_link(__MODULE__, args, name: __MODULE__)
  end

  @impl true
  def init(_args) do
    Process.set_label(__MODULE__)

    children = [NockVM.VM]

    Supervisor.init(children, strategy: :one_for_one)
  end
end