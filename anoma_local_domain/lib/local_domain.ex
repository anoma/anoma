defmodule Anoma.LocalDomain do
  @moduledoc """
  I contain universal local domain functionality.
  """

  defmacro __using__(_opts) do
    quote do
      import Anoma.LocalDomain
    end
  end

  defmacro sigil_k({:<<>>, _meta, [string]}, _opts) do
    key =
      string
      |> String.split("/", trim: true)
      |> Enum.map(&sigil_k_segment/1)

    quote do: [unquote_splicing(key)]
  end

  defp sigil_k_segment("!" <> var) do
    {String.to_existing_atom(var), [], nil}
  end

  defp sigil_k_segment(literal) do
    literal
  end
end
