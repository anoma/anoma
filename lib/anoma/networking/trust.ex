defmodule Anoma.Networking.Trust do
  @type t() :: byte()

  @spec new() :: t()
  def new(), do: 128
end
