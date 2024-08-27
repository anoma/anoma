defmodule Anoma.Resource.Delta do
  import Noun
  use TypedStruct

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

  # use nock map once it exists
  @spec to_noun(t()) :: Noun.t()
  def to_noun(%__MODULE__{deltas: delta}) do
    for {k, v} <- delta do
      if v >= 0 do
        [k, 0 | v]
      else
        [k, 1 | -v]
      end
    end ++ 0
  end

  @spec from_noun(Noun.t()) :: t()
  def from_noun(delta_nock) do
    delta_list = list_nock_to_erlang(delta_nock)

    deltas =
      for [k, v_sign | v_value] <- delta_list, into: %{} do
        binary_k = atom_integer_to_binary(k)

        if v_sign == 0 do
          {binary_k, v_value}
        else
          {binary_k, -v_value}
        end
      end

    %__MODULE__{deltas: deltas}
  end
end
