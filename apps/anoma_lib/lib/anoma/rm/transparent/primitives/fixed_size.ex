defmodule Anoma.RM.Transparent.Primitive.FixedSize do
  @moduledoc """
  I am the FixedSize interface module for the TRM.

  I provide the function to compute the size of the finite field that the
  Transparent RM hashes belong to.

  ### Public API

  I provide the following public functionality:

  - `field_size/1`
  """

  @doc """
  I am the field size function for the TRM.

  Ideally, we would want to say that we are working over an unrealistically
  large finite field such that it's upper bound is a multi-terrabyte-large
  cardinal.
  """
  @spec field_size(integer()) :: integer()
  def field_size(_integer) do
    2 ** 4_000_000_000_000_000_000_000_000
  end
end
