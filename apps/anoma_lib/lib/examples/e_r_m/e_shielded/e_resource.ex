defmodule Examples.ERM.EShielded.EResource do
  alias Anoma.Constants
  alias Anoma.RM.Shielded.Resource

  @spec a_npk() :: binary()
  def a_npk do
    input_nf_key = <<1::256>>
    anpk = Resource.get_npk(input_nf_key)

    anpk
  end

  @spec a_resource() :: %Resource{}
  def a_resource do
    zero_binary = <<0::256>>

    aresource = %Resource{
      # we don't have a real resource logic, use the compliance circuit as resource logic
      logic: Constants.cairo_compliance_program_hash(),
      label: zero_binary,
      quantity: <<5::256>>,
      data: zero_binary,
      eph: false,
      nonce: zero_binary,
      npk: a_npk(),
      rseed: zero_binary
    }

    aresource
  end

  @spec a_resource_commitment() :: binary()
  def a_resource_commitment do
    aresource = a_resource()
    acommitment = Resource.commitment(aresource)

    acommitment
  end

  @spec a_resource_nullifier() :: binary()
  def a_resource_nullifier do
    resource = a_resource()
    anullifier = Resource.nullifier(resource)

    anullifier
  end

  @spec a_output_resource() :: %Resource{}
  def a_output_resource do
    aninput_resource = a_resource()
    anullifier = a_resource_nullifier()
    output_resource = Resource.set_nonce(aninput_resource, anullifier)

    output_resource
  end
end
