defmodule EventBroker do
  @moduledoc """
  I am the EventBroker Application Module.

  I startup the PubSub system as an own OTP application.
  """

  use Application

  def start(_type, args \\ []) do
    EventBroker.Supervisor.start_link(args)
  end
end
