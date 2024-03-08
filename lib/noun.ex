defmodule Noun do
  @moduledoc """
  The noun data structure.

  Represented as Elixir cons cells, which might get annoying.
  """

  @dialyzer :no_improper_lists

  require Integer

  @type noun_atom() :: non_neg_integer() | binary() | []
  @type noun_cell() :: nonempty_improper_list(t(), t())
  @type t() :: noun_atom() | noun_cell()

  # erlang has something called 'atom' already, so we say is_noun_atom
  defguard is_noun_atom(term)
           when (is_integer(term) and term >= 0) or is_binary(term) or
                  term == []

  defguard is_noun_cell(term) when is_list(term) and term != []

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

        x when Integer.is_even(x) ->
          {:ok, subnoun} = axis(div(x, 2), noun)
          axis(2, subnoun)

        x when Integer.is_odd(x) ->
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

        x when Integer.is_even(x) ->
          subaxis = div(axis, 2)
          {:ok, subnoun} = axis(axis + 1, noun)
          replace(subaxis, [replacement | subnoun], noun)

        x when Integer.is_odd(x) ->
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

  @spec equal(t(), t()) :: boolean()
  def equal(noun_1, noun_2)
      when is_noun_atom(noun_1) and is_noun_atom(noun_2) do
    normalized_noun_1 = normalize_noun_atom(noun_1)
    normalized_noun_2 = normalize_noun_atom(noun_2)

    normalized_noun_1 == normalized_noun_2
  end

  def equal(noun_1, noun_2)
      when is_noun_cell(noun_1) and is_noun_cell(noun_2) do
    [h1 | t1] = noun_1
    [h2 | t2] = noun_2

    equal(h1, h2) && equal(t1, t2)
  end

  def equal(_a, _b) do
    false
  end

  # leave binaries, which are most likely to be large, as binaries.
  @spec normalize_noun_atom(noun_atom()) :: binary()
  def normalize_noun_atom(atom) when is_noun_atom(atom) do
    cond do
      atom == [] -> <<>>
      is_integer(atom) -> atom_integer_to_binary(atom)
      is_binary(atom) -> atom
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
  def atom_integer_to_binary(integer)
      when is_integer(integer) and integer >= 0 do
    :binary.encode_unsigned(integer, :little)
  end

  # be idempotent on binaries
  @spec atom_integer_to_binary(binary()) :: binary()
  def atom_integer_to_binary(binary) when is_binary(binary) do
    binary
  end

  @spec atom_binary_to_integer(binary()) :: non_neg_integer()
  def atom_binary_to_integer(binary) when is_binary(binary) do
    :binary.decode_unsigned(binary, :little)
  end

  # be idempotent on integers
  def atom_binary_to_integer(integer)
      when is_integer(integer) and integer >= 0 do
    integer
  end

  @spec condensed_print(t()) :: String.t()
  def condensed_print([]) do
    "~"
  end

  def condensed_print(<<>>) do
    "~"
  end

  def condensed_print(atom) when is_integer(atom) do
    Integer.to_string(atom)
  end

  def condensed_print(atom) when is_binary(atom) do
    "0x" <> Base.encode16(atom)
  end

  def condensed_print(cell = [h | t]) do
    cond do
      cell == Nock.stdlib_core() -> "<stdlib>"
      cell == Nock.rm_core() -> "<rm>"
      cell == Nock.logics_core() -> "<logics>"
      true -> "[" <> condensed_print(h) <> " " <> condensed_print(t) <> "]"
    end
  end
end
