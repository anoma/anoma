defmodule Anoma.Node.Intent.Communicator do
  @moduledoc """


  """

  use GenServer
  use TypedStruct
  alias __MODULE__
  alias Anoma.Node.Utility

  typedstruct do
    field(:subscribers, MapSet.t(GenServer.server()), default: MapSet.new())
    field(:pool, atom(), require: true)
  end

  def init(name: name, init: subscribers) do
    {:ok, %Communicator{pool: name, subscribers: subscribers}}
  end

  def start_link(arg) do
    GenServer.start_link(__MODULE__, arg, Utility.name(arg, &Utility.com_name/1))
  end
end
