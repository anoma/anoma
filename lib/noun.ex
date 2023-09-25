defmodule Noun do
  @moduledoc """
  The noun data structure.

  Represented as Elixir cons cells, which might get annoying.
  """

  @dialyzer :no_improper_lists

  # why are these not in the standard library
  defguard is_even(term) when is_integer(term) and rem(term, 2) == 0
  defguard is_odd(term) when is_integer(term) and rem(term, 2) == 1

  def testing_noun do
    [[4 | 5] | [[12 | 13] | 7]]
  end

  def axis(axis, noun) do
    try do
      case axis do
        0 ->
          :error

        1 ->
          {:ok, noun}

        2 ->
          case noun do
            [h | _] -> {:ok, h}
            _ -> :error
          end

        3 ->
          case noun do
            [_ | t] -> {:ok, t}
            _ -> :error
          end

        x when is_even(x) ->
          {:ok, subnoun} = axis(div(x, 2), noun)
          axis(2, subnoun)

        x when is_odd(x) ->
          {:ok, subnoun} = axis(div(x, 2), noun)
          axis(3, subnoun)

        _ ->
          :error
      end
    rescue
      _ in MatchError -> :error
    end
  end

  def replace(axis, replacement, noun) do
    try do
      case axis do
        0 ->
          :error

        1 ->
          {:ok, replacement}

        x when is_even(x) ->
          subaxis = div(axis, 2)
          {:ok, subnoun} = axis(axis + 1, noun)
          replace(subaxis, [replacement | subnoun], noun)

        x when is_odd(x) ->
          subaxis = div(axis, 2)
          {:ok, subnoun} = axis(axis - 1, noun)
          replace(subaxis, [subnoun | replacement], noun)

        _ ->
          :error
      end
    rescue
      _ in MatchError -> :error
    end
  end
end
