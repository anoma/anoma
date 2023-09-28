defmodule Anoma.Node.Intent.Pool do
  @moduledoc """

  """

  alias __MODULE__
  use TypedStruct
  use GenServer

  alias Anoma.Intent

  typedstruct do
    field(:intents, MapSet.t(Intent.t()), default: MapSet.new())
  end

  def init(_init) do
    {:ok, %Pool{}}
  end

  def start_link(arg) do
    # please do this better, it's duplicated!!!
    name = arg[:name]

    options =
      if name do
        [name: name]
      else
        []
      end

    GenServer.start_link(__MODULE__, arg, options)
  end
end
