defmodule Examples.ECairo.EResource do
  alias Anoma.CairoResource.Resource
  alias Anoma.Constants

  @spec a_fixed_resource() :: Resource.t()
  def a_fixed_resource do
    zero_binary = <<0::256>>
    input_nf_key = <<1::256>>

    aresource = %Resource{
      # we don't have a real resource logic, use the compliance circuit as resource logic
      logic: Constants.cairo_trivial_resource_logic_hash(),
      label: zero_binary,
      quantity: <<5::256>>,
      data: zero_binary,
      eph: false,
      nonce: zero_binary,
      nk_commitment: Resource.get_nk_commitment(input_nf_key),
      rseed: zero_binary
    }

    aresource
  end

  @spec a_resource_commitment() :: binary()
  def a_resource_commitment do
    aresource = a_fixed_resource()
    acommitment = Resource.commitment(aresource)

    acommitment
  end

  @spec a_resource_nullifier() :: binary()
  def a_resource_nullifier do
    resource = a_fixed_resource()
    anullifier = Resource.nullifier(resource, <<1::256>>)

    anullifier
  end

  @spec a_fixed_output_resource() :: Resource.t()
  def a_fixed_output_resource do
    aninput_resource = a_fixed_resource()
    anullifier = a_resource_nullifier()
    output_resource = Resource.set_nonce(aninput_resource, anullifier)

    output_resource
  end

  @spec a_trivial_input_intent_resource() :: Resource.t()
  def a_trivial_input_intent_resource do
    zero_binary = <<0::256>>
    input_nf_key = <<1::256>>

    aresource = %Resource{
      # we don't have a real resource logic, use the compliance circuit as resource logic
      logic: Constants.cairo_trivial_resource_logic_hash(),
      label: zero_binary,
      quantity: <<1::256>>,
      data: zero_binary,
      eph: true,
      nonce: zero_binary,
      nk_commitment: Resource.get_nk_commitment(input_nf_key),
      rseed: zero_binary
    }

    aresource
  end

  @spec a_trivial_output_intent_resource() :: Resource.t()
  def a_trivial_output_intent_resource do
    input_intent_resource = a_trivial_input_intent_resource()
    input_nullifier = Resource.nullifier(input_intent_resource, <<1::256>>)

    zero_binary = <<0::256>>
    input_nf_key = <<1::256>>

    aresource = %Resource{
      # we don't have a real resource logic, use the compliance circuit as resource logic
      logic: Constants.cairo_trivial_resource_logic_hash(),
      label: zero_binary,
      quantity: <<1::256>>,
      data: zero_binary,
      eph: true,
      nonce: zero_binary,
      nk_commitment: Resource.get_nk_commitment(input_nf_key),
      rseed: zero_binary
    }

    Resource.set_nonce(aresource, input_nullifier)
  end
end
