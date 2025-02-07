defmodule Anoma.RM.Transparent.ComplianceUnit do
  alias Anoma.RM.Transparent.ProvingSystem.CPS
  alias Anoma.RM.Transparent.ProvingSystem.CPS.Instance

  use TypedStruct

  typedstruct enforce: true do
    field(:proof, <<>>, default: <<>>)
    field(:instance, Instance.t(), default: %Instance{})
    field(:vk, <<>>, default: CPS.key())
  end

  @spec delta(t()) :: integer()
  def delta(t) do
    t.instance.unit_delta
  end

  @spec created(t()) :: MapSet.t(integer())
  def created(t) do
    t.insance.created |> Enum.map(&elem(&1, 0)) |> MapSet.new()
  end

  @spec consumed(t()) :: MapSet.t(integer())
  def consumed(t) do
    t.insance.consumed |> Enum.map(&elem(&1, 0)) |> MapSet.new()
  end

  @spec create(CPS.key(), Instance.t(), <<>>) :: t()
  def create(key, instance, proof) do
    %__MODULE__{instance: instance, vk: key, proof: proof}
  end

  @spec verify(t()) :: bool()
  def verify(t) do
    CPS.verify(t.vk, t.instance, <<>>)
  end
end
