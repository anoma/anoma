defmodule Anoma.Node do
  use Application

  def start(_type, args) do
    Anoma.Supervisor.start_link(args)
  end
end
