defmodule Examples.EShieldedResource do
  alias Anoma.Constants
  alias Anoma.RM.Shielded.Resource
  alias Examples.EShieldedResource

  @spec a_npk() :: binary()
  def a_npk do
    input_nf_key = :binary.copy(<<0>>, 31) <> <<1>>
    anpk = Resource.get_npk(input_nf_key)

    anpk
  end

  @spec a_resource() :: %Resource{}
  def a_resource do
    zero_binary = :binary.copy(<<0>>, 32)

    aresource = %Resource{
      # we don't have a real resource logic, use the compliance circuit as resource logic
      logic: Constants.cairo_compliance_program_hash(),
      label: zero_binary,
      quantity: :binary.copy(<<0>>, 31) <> <<5>>,
      data: zero_binary,
      eph: false,
      nonce: zero_binary,
      npk: EShieldedResource.a_npk(),
      rseed: zero_binary
    }

    aresource
  end

  @spec a_resource_commitment() :: binary()
  def a_resource_commitment do
    aresource = EShieldedResource.a_resource()
    acommitment = Resource.commitment(aresource)

    acommitment
  end

  @spec a_resource_nullifier() :: binary()
  def a_resource_nullifier do
    resource = EShieldedResource.a_resource()
    anullifier = Resource.nullifier(resource)

    anullifier
  end

  @spec a_output_resource() :: %Resource{}
  def a_output_resource do
    aninput_resource = EShieldedResource.a_resource()
    anullifier = EShieldedResource.a_resource_nullifier()
    output_resource = Resource.set_nonce(aninput_resource, anullifier)

    output_resource
  end
end
