defmodule EventBroker.Supervisor do
  @moduledoc """
  I am the EventBroker Supervisor for PubSub.

  I start up 3 children, namely the Broker, the Registry, and the Dynamic
  Supervisor for the filters.
  """

  use Supervisor

  def start_link(args \\ []) do
    {:ok, keys} =
      args
      |> Keyword.validate(
        name: __MODULE__,
        broker_name: EventBroker.Broker,
        registry_name: EventBroker.Registry
      )

    Supervisor.start_link(__MODULE__, keys, name: keys[:name])
  end

  def init(args) do
    dyn_sup_name =
      (Atom.to_string(args[:registry_name]) <> ".DynamicSupervisor")
      |> :erlang.binary_to_atom()

    new_args =
      args
      |> Keyword.put_new(:dyn_sup_name, dyn_sup_name)

    children = [
      {EventBroker.Broker, args},
      {EventBroker.Registry, new_args},
      {DynamicSupervisor,
       name: dyn_sup_name, strategy: :one_for_one, max_restarts: 0}
    ]

    Supervisor.init(children, strategy: :one_for_all)
  end
end
