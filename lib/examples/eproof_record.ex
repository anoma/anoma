defmodule Examples.EProofRecord do
  use Memoize

  require ExUnit.Assertions
  import ExUnit.Assertions

  alias Anoma.RM.Resource.ProofRecord

  alias Examples.EResource

  # Any interesting properties to show?
  @spec a10_space_proof() :: ProofRecord.t()
  def a10_space_proof() do
    resource = ProofRecord.prove(EResource.a10_space_resource())

    assert {:ok, resource} ==
             ProofRecord.from_noun(Noun.Nounable.to_noun(resource))

    resource
  end

  @spec b10_space_proof() :: ProofRecord.t()
  @spec a5_space_proof() :: ProofRecord.t()
  @spec a10_d0_proof() :: ProofRecord.t()
  @spec b10_d0_proof() :: ProofRecord.t()
  def b10_space_proof(), do: ProofRecord.prove(EResource.b10_space_resource())
  def a5_space_proof(), do: ProofRecord.prove(EResource.a5_space_resource())
  def a10_d0_proof(), do: ProofRecord.prove(EResource.a10_d0_resource())
  def b10_d0_proof(), do: ProofRecord.prove(EResource.b10_d0_resource())

  def ax_proof(), do: ProofRecord.prove(EResource.ax_resource())
  def ay_proof(), do: ProofRecord.prove(EResource.ay_resource())
  def bx_proof(), do: ProofRecord.prove(EResource.bx_resource())
  def by_proof(), do: ProofRecord.prove(EResource.by_resource())

  @spec a0_counter_proof() :: ProofRecord.t()
  def a0_counter_proof(),
    do: ProofRecord.prove(EResource.a0_counter_resource())

  @spec a1_counter_proof() :: ProofRecord.t()
  def a1_counter_proof(),
    do: ProofRecord.prove(EResource.a1_counter_resource())
end
