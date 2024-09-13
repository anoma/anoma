defmodule Noun.Format do
  @moduledoc """
  Parsing and printing of nouns.
  """

  @dialyzer :no_improper_lists

  @spec parse_always(String.t()) :: Noun.t()
  def parse_always(string) do
    {:ok, parsed} = parse(string)
    parsed
  end

  @spec parse(String.t()) :: {:ok, Noun.t()} | :error
  def parse(string) do
    # the hoon compiler emits integers with '.' thousands separators
    # it's easier to pre-strip these than to parse them
    dotless = String.replace(string, ".", "")
    parse_inner(dotless)
  end

  def parse_inner(string) do
    trimmed = String.trim_leading(string)
    maybe_atom = Integer.parse(trimmed)

    case maybe_atom do
      :error ->
        parse_cell(trimmed)

      {atom, ""} ->
        {:ok, atom}

      {atom, rest} ->
        {:continue, atom, rest}
    end
  end

  def parse_cell(<<?[, rest::binary>>) do
    head = parse_inner(rest)

    case head do
      {:ok, _atom} ->
        :error

      {:continue, head_result, rest} ->
        tail_parse = parse_tail(rest)

        case tail_parse do
          {:ok, tail_result} ->
            {:ok, [head_result | tail_result]}

          :error ->
            :error

          {:continue, tail_result, inner_rest} ->
            trimmed_inner_rest = String.trim_leading(inner_rest)

            if trimmed_inner_rest == "" do
              {:ok, [head_result | tail_result]}
            else
              {:continue, [head_result | tail_result], inner_rest}
            end
        end

      :error ->
        :error
    end
  end

  def parse_cell(_) do
    :error
  end

  def parse_tail(string) do
    result = parse_inner(string)

    case result do
      {:continue, head_result, rest} ->
        trimmed_rest = String.trim_leading(rest)

        case trimmed_rest do
          "]" ->
            {:ok, head_result}

          "]" <> inner_rest ->
            {:continue, head_result, inner_rest}

          _ ->
            tail_parse = parse_tail(trimmed_rest)

            case tail_parse do
              {:ok, tail_result} ->
                {:ok, [head_result | tail_result]}

              {:continue, tail_result, inner_rest} ->
                {:continue, [head_result | tail_result], inner_rest}

              _ ->
                :error
            end
        end

      _ ->
        :error
    end
  end

  def print(noun) when is_integer(noun) do
    to_string(noun)
  end

  def print([h | t]) do
    "[" <> print(h) <> " " <> print_tail(t) <> "]"
  end

  def print_tail(noun) when is_integer(noun) do
    to_string(noun)
  end

  def print_tail([h | t]) do
    print(h) <> " " <> print_tail(t)
  end
end
