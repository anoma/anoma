defmodule Anoma.RM.Transparent.ComplianceUnit do
  @moduledoc """
  I am the Compliance Unit module for the TRM.

  I provide the interface to interact with the compliance units in the
  resource machine.

  ### Public API

  - `delta/1`
  - `created/1`
  - `consumed/1`
  - `verify/1`
  - `create/3`
  """
  alias Anoma.RM.Transparent.ProvingSystem.CPS
  alias Anoma.RM.Transparent.ProvingSystem.CPS.Instance

  use TypedStruct

  typedstruct enforce: true do
    field(:proof, <<>>, default: <<>>)
    field(:instance, Instance.t(), default: %Instance{})
    field(:vk, <<>>, default: CPS.key())
  end

  @doc """
  I am the delta function for a compliance unit in the TRM.

  I simply access the unit delta field in the compliance unit.
  """
  @spec delta(t()) :: integer()
  def delta(t) do
    t.instance.unit_delta
  end

  @doc """
  I am the created function for a compliance unit in TRM.

  I access the created field in a given instance of the unit and collect
  the result in a set.
  """
  @spec created(t()) :: MapSet.t(integer())
  def created(t) do
    t.insance.created |> Enum.map(&elem(&1, 0)) |> MapSet.new()
  end

  @doc """
  I am the consumed function for the compliance unit in TRM.

  I access the consumed field in a given instance of the unit and collect
  the result in a set.
  """
  @spec consumed(t()) :: MapSet.t(integer())
  def consumed(t) do
    t.insance.consumed |> Enum.map(&elem(&1, 0)) |> MapSet.new()
  end

  @doc """
  I am the creation API for the compliance unit of the TRM.

  Given a key, instance, and proof for the unit, I put them as appropriate
  arguments to the unit.
  """
  @spec create(CPS.cps_key(), Instance.t(), <<>>) :: t()
  def create(key, instance, proof) do
    %__MODULE__{instance: instance, vk: key, proof: proof}
  end

  @doc """
  I am the compliance unit verification function.

  I simply call the compliance proving system verification function.
  """
  @spec verify(t()) :: boolean()
  def verify(t) do
    CPS.verify(t.vk, t.instance, <<>>)
  end

  @spec from_noun(Noun.t()) :: {:ok, t()} | :error
  def from_noun([proof, instance | vk]) do
    with true <- Noun.equal?(proof, 0),
         {:ok, instance} <- Instance.from_noun(instance),
         true <- Noun.equal?(vk, 0) do
      {:ok, %__MODULE__{instance: instance}}
    else
      _ -> :error
    end
  end

  @spec to_noun(t()) :: Noun.t()
  def to_noun(t) do
    [<<>>, Instance.to_noun(t.instance) | <<>>]
  end
end
