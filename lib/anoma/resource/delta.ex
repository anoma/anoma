defmodule Anoma.Resource.Delta do
  import Noun
  use TypedStruct

  @behaviour Noun.Nounable.Kind

  typedstruct enforce: true do
    # usually non_neg_integer, but not in execution
    field(:deltas, %{binary() => integer()})
  end

  @spec empty() :: t()
  def empty(), do: new(%{})

  @spec new(%{binary() => integer()}) :: t()
  def new(map = %{}), do: %__MODULE__{deltas: map}

  @spec add(t(), t()) :: t()
  def add(%__MODULE__{deltas: d1}, %__MODULE__{deltas: d2}) do
    deltas =
      Map.merge(d1, d2, fn _k, v1, v2 -> v1 + v2 end)
      |> Map.reject(fn {_k, v} -> v == 0 end)

    %__MODULE__{deltas: deltas}
  end

  @spec negate(t()) :: t()
  def negate(%__MODULE__{deltas: r}) do
    %__MODULE__{deltas: Map.new(r, fn {k, v} -> {k, -v} end)}
  end

  @spec sub(t(), t()) :: t()
  def sub(d1, d2) do
    add(d1, negate(d2))
  end

  @spec from_noun(noun :: Noun.t()) :: {:ok, t()} | :error
  def from_noun(delta_nock) do
    with {:ok, map} <- Noun.Nounable.Map.from_noun(delta_nock) do
      deltas =
        Map.new(map, fn {key, [v_sign | v_value]} ->
          binary_key = atom_integer_to_binary(key)

          {binary_key,
           if Noun.is_zero(v_sign) do
             v_value
           else
             -v_value
           end}
        end)

      {:ok, %__MODULE__{deltas: deltas}}
    end
  end

  defimpl Noun.Nounable, for: __MODULE__ do
    def to_noun(%Anoma.Resource.Delta{deltas: delta}) do
      delta
      |> Map.new(fn {k, v} ->
        {k,
         if v >= 0 do
           [0 | v]
         else
           [1 | -v]
         end}
      end)
      |> Noun.Nounable.to_noun()
    end
  end
end
