defmodule Anoma.RM.Transparent.Primitive.FixedSize do
  @cardinality 2 ** 4_000_000

  @spec field_size(integer()) :: integer()
  def field_size(_integer) do
    @cardinality
  end
end
