defmodule NockVM do
  @moduledoc """
  I am the application which provides Nock VMs.
  """

  use Application

  @impl true
  def start(_type, args) do
    NockVM.Supervisor.start_link(args)
  end
end
