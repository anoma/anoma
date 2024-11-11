defmodule Examples.ERisc0.EResource do
  alias Anoma.RM.Risc0.Constants
  alias Anoma.RM.Risc0.Resource

  @spec a_npk() :: binary()
  def a_npk do
    input_nf_key = <<1::256>>
    anpk = Resource.get_npk(input_nf_key)

    anpk
  end

  @spec a_resource(nsk: list(byte())) :: %Resource{}
  def a_resource(nsk: nsk) do
    logic = Risc0.random_32()
    label = Risc0.random_32()
    nonce = Risc0.random_32()
    quantity = Risc0.random_32()
    data = Risc0.random_32()
    eph = false
    npk = Risc0.generate_npk(nsk)
    rseed = Risc0.random_32()

    aresource = %Resource{
      # we don't have a real resource logic, use the compliance circuit as resource logic
      logic: logic |> :binary.list_to_bin(),
      label: label |> :binary.list_to_bin(),
      quantity: quantity |> :binary.list_to_bin(),
      data: data |> :binary.list_to_bin(),
      eph: eph,
      nonce: nonce |> :binary.list_to_bin(),
      npk: npk |> :binary.list_to_bin(),
      rseed: rseed |> :binary.list_to_bin()
    }

    aresource
  end

  @spec a_resource_commitment() :: binary()
  def a_resource_commitment do
    nsk = Risc0.random_32()
    aresource = a_resource(nsk)
    acommitment = Resource.commitment(aresource)

    acommitment
  end

  @spec a_resource_nullifier() :: binary()
  def a_resource_nullifier do
    nsk = Risc0.random_32()
    aresource = a_resource(nsk)
    anullifier = Resource.nullifier(aresource)

    anullifier
  end

  @spec a_output_resource() :: %Resource{}
  def a_output_resource do
    nsk = Risc0.random_32()
    aninput_resource = a_resource(nsk)
    anullifier = a_resource_nullifier()
    output_resource = Resource.set_nonce(aninput_resource, anullifier)

    output_resource
  end
end
