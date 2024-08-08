defmodule EventBroker do
  @moduledoc """
  The EventBroker Supervisor for Registry and Broker
  """

  alias __MODULE__

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
    children = [
      {EventBroker.Broker, args},
      {EventBroker.Registry, args}
    ]

    Supervisor.init(children, strategy: :one_for_all)
  end
end
