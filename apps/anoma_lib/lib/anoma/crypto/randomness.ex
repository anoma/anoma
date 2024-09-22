defmodule Anoma.Crypto.Randomness do
  @moduledoc """
  I am an implementation of the Local Randomness Engine
  """

  @doc """
  Given a non-negative integer N, I provide a random bit of size N
  """
  @spec get_random(non_neg_integer()) :: binary()
  def get_random(size) do
    :enacl.randombytes(size)
  end
end
