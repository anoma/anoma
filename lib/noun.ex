defmodule Noun do
  @moduledoc """
  The noun data structure.

  Represented as Elixir cons cells, which might get annoying.
  """

  @dialyzer :no_improper_lists

  require Integer

  @type t() :: non_neg_integer() | nonempty_improper_list(t(), t())

  # erlang has something called 'atom' already, so we say is_noun_atom
  defguard is_noun_atom(term) when is_integer(term) and term >= 0
  defguard is_noun_cell(term) when is_list(term) and term != []
  defguard is_even(term) when is_noun_atom(term) and Integer.is_even(term)
  defguard is_odd(term) when is_noun_atom(term) and Integer.is_odd(term)

  @spec axis(non_neg_integer(), t()) :: {:ok, t()} | :error
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

  @spec replace(non_neg_integer(), t(), t()) :: {:ok, t()} | :error
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

  @spec mug(t()) :: non_neg_integer()
  def mug(noun) do
    :erlang.term_to_binary(noun)
    # seed: %mug
    |> XXHash.xxh64(6_780_269)
  end

  # maybe obviate these by treating [] as a zero?
  @spec list_erlang_to_nock([]) :: 0
  def list_erlang_to_nock([]) do
    0
  end

  @spec list_erlang_to_nock(nonempty_list(t())) ::
          nonempty_improper_list(t(), t())
  def list_erlang_to_nock([h | t]) do
    [h | list_erlang_to_nock(t)]
  end

  @spec list_nock_to_erlang(0) :: []
  def list_nock_to_erlang(0) do
    []
  end

  @spec list_nock_to_erlang(nonempty_improper_list(t(), t())) :: list(t())
  def list_nock_to_erlang([h | t]) do
    [h | list_nock_to_erlang(t)]
  end

  @spec atom_integer_to_binary(0) :: <<>>
  # special case: zero is the empty binary
  def atom_integer_to_binary(0) do
    <<>>
  end

  @spec atom_integer_to_binary(pos_integer()) :: binary()
  def atom_integer_to_binary(integer) do
    :binary.encode_unsigned(integer, :little)
  end

  @spec atom_binary_to_integer(binary()) :: non_neg_integer()
  def atom_binary_to_integer(binary) do
    :binary.decode_unsigned(binary, :little)
  end
end
