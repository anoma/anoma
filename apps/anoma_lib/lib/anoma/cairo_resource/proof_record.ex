defmodule Anoma.CairoResource.ProofRecord do
  @moduledoc """
  I am a proof record for a shielded resource.
  """

  alias __MODULE__
  use TypedStruct

  @behaviour Noun.Nounable.Kind

  typedstruct enforce: true do
    field(:proof, binary(), default: <<>>)
    field(:public_inputs, binary(), default: <<>>)
  end

  @spec from_noun(Noun.t()) :: {:ok, t()} | :error
  def from_noun([proof | public_inputs]) do
    {:ok,
     %ProofRecord{
       proof: proof,
       public_inputs: public_inputs
     }}
  end

  def from_noun(_), do: :error

  defimpl Noun.Nounable, for: __MODULE__ do
    @impl true
    def to_noun(proof_record = %ProofRecord{}) do
      {
        proof_record.proof,
        proof_record.public_inputs
      }
      |> Noun.Nounable.to_noun()
    end
  end

  @spec generate_compliance_proof(binary()) :: {:ok, t()} | :error
  def generate_compliance_proof(compliance_private_inputs) do
    dir =
      Path.join(:code.priv_dir(:anoma_lib), "params/cairo_compliance.json")

    with {:ok, compliance_circuit} <- File.read(dir) do
      {_output, trace, memory, public_inputs} =
        Cairo.cairo_vm_runner(
          compliance_circuit,
          compliance_private_inputs
        )

      {proof, public_inputs} = Cairo.prove(trace, memory, public_inputs)

      {:ok,
       %ProofRecord{
         proof: proof |> :binary.list_to_bin(),
         public_inputs: public_inputs |> :binary.list_to_bin()
       }}
    else
      _ -> :error
    end
  end

  @spec generate_cairo_proof(binary(), binary()) :: {:ok, t()} | :error
  def generate_cairo_proof(circuit, inputs) do
    {_output, trace, memory, public_inputs} =
      Cairo.cairo_vm_runner(
        circuit,
        inputs
      )

    # TODO: handle the error
    {proof, public_inputs} = Cairo.prove(trace, memory, public_inputs)

    {:ok,
     %ProofRecord{
       proof: proof |> :binary.list_to_bin(),
       public_inputs: public_inputs |> :binary.list_to_bin()
     }}
  end

  @spec get_cairo_program_hash(ProofRecord.t()) :: binary()
  def get_cairo_program_hash(proof_record) do
    proof_record.public_inputs
    |> :binary.bin_to_list()
    |> Cairo.get_program_hash()
    |> :binary.list_to_bin()
  end

  @spec verify(ProofRecord.t()) :: boolean() | {:error, term()}
  def verify(proof) do
    public_inputs =
      proof.public_inputs
      |> :binary.bin_to_list()

    proof.proof
    |> :binary.bin_to_list()
    |> Cairo.verify(public_inputs)
  end
end
