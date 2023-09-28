defmodule Anoma.Node.Intent.Pool do
  @moduledoc """

  """

  alias __MODULE__
  use TypedStruct
  use GenServer

  alias Anoma.Intent
  alias Anoma.Node.Utility

  typedstruct do
    field(:intents, MapSet.t(Intent.t()), default: MapSet.new())
  end

  def init(_init) do
    {:ok, %Pool{}}
  end

  def start_link(arg) do
    GenServer.start_link(__MODULE__, arg, Utility.name(arg))
  end
end
