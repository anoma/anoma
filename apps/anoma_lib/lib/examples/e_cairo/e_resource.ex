defmodule Examples.ECairo.EResource do
  alias Anoma.CairoResource.Resource
  alias Anoma.Constants

  @spec a_fixed_resource() :: Resource.t()
  def a_fixed_resource do
    zero_binary = <<0::256>>
    input_nf_key = <<1::256>>

    aresource = %Resource{
      logic_ref: Constants.cairo_trivial_resource_logic_hash(),
      label_ref: zero_binary,
      quantity: <<5::256>>,
      value_ref: zero_binary,
      is_ephemeral: false,
      nonce: zero_binary,
      nk_commitment: Resource.get_nk_commitment(input_nf_key),
      rand_seed: zero_binary
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
    Resource.nullifier(resource, <<1::256>>)
  end

  @spec a_fixed_output_resource() :: Resource.t()
  def a_fixed_output_resource do
    aninput_resource = a_fixed_resource()
    anullifier = a_resource_nullifier()
    output_resource = Resource.set_nonce(aninput_resource, anullifier)

    output_resource
  end

  @spec a_fixed_output_resource_commitment() :: binary()
  def a_fixed_output_resource_commitment do
    a_fixed_output_resource() |> Resource.commitment()
  end

  @spec a_trivial_input_intent_resource() :: Resource.t()
  def a_trivial_input_intent_resource do
    zero_binary = <<0::256>>
    input_nf_key = <<1::256>>

    aresource = %Resource{
      logic_ref: Constants.cairo_trivial_resource_logic_hash(),
      label_ref: zero_binary,
      quantity: <<1::256>>,
      value_ref: zero_binary,
      is_ephemeral: true,
      nonce: zero_binary,
      nk_commitment: Resource.get_nk_commitment(input_nf_key),
      rand_seed: zero_binary
    }

    aresource
  end

  @spec a_trivial_input_intent_resource_nullifier() :: binary()
  def a_trivial_input_intent_resource_nullifier do
    a_trivial_input_intent_resource() |> Resource.nullifier(<<1::256>>)
  end

  @spec a_trivial_output_intent_resource() :: Resource.t()
  def a_trivial_output_intent_resource do
    zero_binary = <<0::256>>
    input_nf_key = <<1::256>>

    aresource = %Resource{
      logic_ref: Constants.cairo_trivial_resource_logic_hash(),
      label_ref: zero_binary,
      quantity: <<1::256>>,
      value_ref: zero_binary,
      is_ephemeral: true,
      nonce: zero_binary,
      nk_commitment: Resource.get_nk_commitment(input_nf_key),
      rand_seed: zero_binary
    }

    Resource.set_nonce(aresource, a_trivial_input_intent_resource_nullifier())
  end

  @spec a_trivial_output_intent_resource_commitment() :: binary()
  def a_trivial_output_intent_resource_commitment do
    a_trivial_output_intent_resource() |> Resource.commitment()
  end
end
