defmodule Anoma.Symbol do
  @moduledoc """
  I am module that provides utilities over symbols

  ### Public API

  - `append/2`

  """

  @typedoc """
  I am either a string or an atom
  """
  @type s() :: atom() | String.t()

  @doc """
  I append two values that implement `to_string/1` and returns an appended symbol
  """
  @spec append(s(), s()) :: atom()
  def append(sym1, sym2) do
    String.to_atom(to_string(sym1) <> to_string(sym2))
  end
end
