defmodule Anoma.RM.Risc0.ProofRecord do
  @moduledoc """
  I am a proof record for a shielded resource for the RISC0 resource machine.
  """

  alias __MODULE__
  alias Anoma.RM.Risc0.ComplianceWitness
  alias Anoma.RM.Risc0.Resource
  use TypedStruct

  @behaviour Noun.Nounable.Kind

  typedstruct enforce: true do
    field(:proof, binary(), default: <<>>)
  end

  @spec from_noun(Noun.t()) :: {:ok, t()} | :error
  def from_noun(proof) do
    {:ok,
     %ProofRecord{
       proof: proof,
     }}
  end


  defimpl Noun.Nounable, for: __MODULE__ do
    def to_noun(proof_record = %ProofRecord{}) do
      {
        proof_record.proof,
      }
      |> Noun.Nounable.to_noun()
    end
  end

  @spec generate_compliance_proof(%ComplianceWitness{}) :: {:ok, t()} | :error
  def generate_compliance_proof(
    %ComplianceWitness{
      input_resource: input_resource,
      output_resource: output_resource,
      rcv: rcv,
      merkle_path: merkle_path,
      nsk: nsk
    }
  ) do
    guest_path = Path.join([
      File.cwd!(),
      "deps",
      "risc0",
      "native",
      "compliance-circuit",
      "target",
      "riscv-guest",
      "riscv32im-risc0-zkvm-elf",
      "release",
      "compliance_guest"
    ])

    IO.puts("File exists?: #{File.exists?(guest_path)}")

    with {:ok, compliance_guest_elf} <- File.read(guest_path) do
      compliance_circuit = Risc0.generate_compliance_circuit(
        input_resource |> Resource.to_bytes(),
        output_resource |> Resource.to_bytes(),
        rcv,
        merkle_path,
        nsk
      )
      receipt = Risc0.prove(compliance_circuit, compliance_guest_elf |> :binary.bin_to_list())

      {:ok,
       %ProofRecord{
         proof: receipt |> :binary.list_to_bin(),
       }}
    else
      _ -> :error
    end
  end
end
