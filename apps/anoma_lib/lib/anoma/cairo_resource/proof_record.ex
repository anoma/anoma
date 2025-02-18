defmodule Anoma.CairoResource.ProofRecord do
  @moduledoc """
  I am a proof record for a shielded resource.
  """

  alias __MODULE__
  use TypedStruct

  @behaviour Noun.Nounable.Kind

  typedstruct enforce: true do
    field(:verifying_key, binary(), default: <<>>)
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

  @doc """
  Generates a cairo proof using the provided proving key and witness.

  ## Parameters

    - `proving_key` (binary): The proving key used for generating the proof.
    - `witness` (binary): The witness data used for generating the proof.
    - `_instance` (any, not used): An instance parameter, defaults to an empty
      binary. In zkvms, the instance is an output rather than an input.

  ## Returns

    - `{:ok, %ProofRecord{}}`: A tuple containing `:ok` and a `ProofRecord`
      struct with the generated proof and public inputs.
    - `{:error, any()}`: A tuple containing `:error` and the reason for the
      failure.

  """
  @spec prove(binary(), binary(), any()) ::
          {:error, any()} | {:ok, t()}
  def prove(proving_key, witness, _instance \\ <<>>) do
    with {_output, trace, memory, public_inputs} <-
           Cairo.cairo_vm_runner(
             proving_key,
             witness
           ),
         {proof, public_inputs} <- Cairo.prove(trace, memory, public_inputs) do
      {:ok,
       %ProofRecord{
         proof: proof |> :binary.list_to_bin(),
         public_inputs: public_inputs |> :binary.list_to_bin()
       }}
    else
      {:error, reason} ->
        {:error, reason}
    end
  end

  @doc """
  Verifies the given `ProofRecord`.

  This function takes a `ProofRecord` and verifies its proof against its public
  inputs. It converts the binary data of both the public inputs and the proof to
  lists before passing them to the `Cairo.verify/2` function. There is no
  verifying key in the Cairo system; instead, a program segment in the instance
  serves this purpose.  For efficiency, we provide a method to retrieve the
  verifying key from public inputs without explicitly using the verifying key
  field.

  ## Parameters

    - proof: A `ProofRecord` containing the verifying key, proof and public
      inputs to be verified.

  ## Returns

    - `true` if the proof is valid.
    - `false` if the proof is invalid.
    - `{:error, term()}` if the verification encounters errors.

  """
  @spec verify(ProofRecord.t()) :: boolean() | {:error, term()}
  def verify(proof) do
    public_inputs =
      proof.public_inputs
      |> :binary.bin_to_list()

    proof.proof
    |> :binary.bin_to_list()
    |> Cairo.verify(public_inputs)
  end

  @doc """
  Generates a compliance proof using the provided witness and the fixed
  compliance proving key.

  This function reads the compliance proving key from a JSON file. It then uses
  this proving key to generate a proof based on the given witness.

  ## Parameters

    - `witness` (binary()): The witness data used to generate the compliance
      proof.

  ## Returns

    - `{:ok, t()}`: If the compliance proof is successfully generated.
    - `{:error, term()}`: If there is an error, such as the compliance proving key
      file not being found.

  """
  @spec generate_compliance_proof(binary()) :: {:ok, t()} | {:error, term()}
  def generate_compliance_proof(witness) do
    dir =
      Path.join(:code.priv_dir(:anoma_lib), "params/cairo_compliance.json")

    with {:ok, compliance_circuit} <- File.read(dir) do
      prove(compliance_circuit, witness)
    else
      _ -> {:error, "cairo_compliance.json not found"}
    end
  end

  @doc """
  Calculates the Cairo program hash for the given `ProofRecord`.

  ## Parameters

    - proof_record: A `ProofRecord` struct containing the public inputs.

  ## Returns

    - A binary representing the Cairo program hash.

  """
  @spec get_cairo_program_hash(ProofRecord.t()) :: binary()
  def get_cairo_program_hash(proof_record) do
    proof_record.public_inputs
    |> :binary.bin_to_list()
    |> Cairo.get_program_hash()
    |> :binary.list_to_bin()
  end
end
