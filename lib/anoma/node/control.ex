defmodule Anoma.Node.Control do
  @moduledoc """
  I am the control machine implmentation.

  I supervise the static and dynamic configuration engines alongside the measuring engine.
  """

  use Supervisor

  def start_link(init_state) do
    Supervisor.start_link(__MODULE__, init_state)
  end

  def init(args) do
    storage = args[:storage]

    children = [
      {Anoma.Node.Control.Dynamic, name: args[:dyn], storage: storage},
      {Anoma.Node.Control.Measure, name: args[:meas], storage: storage}
    ]

    Supervisor.init(children, strategy: :one_for_one)
  end

  def shutdown(supervisor), do: Supervisor.stop(supervisor, :normal)

  def handle_subs(subs, key) do
    subs
    |> MapSet.to_list()
    |> Enum.filter(fn {_sub, k} -> k == key end)
    |> Enum.map(fn {sub, _k} -> sub end)
    |> MapSet.new()
  end
end
