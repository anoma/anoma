defmodule Anoma.CairoResource.Compliance.ComplianceUnit do
  @moduledoc """
  I am a compliance proof unit.
  """

  alias __MODULE__
  use TypedStruct

  alias Anoma.CairoResource.Compliance.Instance

  typedstruct enforce: true do
    field(:verifying_key, binary(), default: <<>>)
    field(:proof, binary(), default: <<>>)
    field(:instance, Instance.t(), default: %Instance{})
    # public_input contains other necessary information for the cairo proof,
    # including the full vk, public memory, etc.
    field(:public_input, binary(), default: <<>>)
  end

  @spec from_noun(Noun.t()) :: {:ok, t()}
  def from_noun([proof | public_input]) do
    instance =
      public_input
      |> :binary.bin_to_list()
      |> Instance.from_public_input()

    verifying_key =
      public_input
      |> :binary.bin_to_list()
      |> Cairo.get_program_hash()
      |> :binary.list_to_bin()

    {:ok,
     %__MODULE__{
       verifying_key: verifying_key,
       proof: Noun.atom_integer_to_binary(proof),
       instance: instance,
       public_input: Noun.atom_integer_to_binary(public_input)
     }}
  end

  defimpl Noun.Nounable, for: __MODULE__ do
    @impl true
    def to_noun(unit = %ComplianceUnit{}) do
      [
        unit.proof
        | unit.public_input
      ]
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

    - `{:ok, %ComplianceUnit{}}`: A tuple containing `:ok` and a `ComplianceUnit`
      struct with the generated proof and instance.
    - `{:error, any()}`: A tuple containing `:error` and the reason for the
      failure.

  """
  @spec prove(binary(), binary(), any()) ::
          {:error, any()} | {:ok, t()}
  def prove(proving_key, witness, _instance \\ <<>>) do
    with {_output, trace, memory, instance_str} <-
           Cairo.cairo_vm_runner(
             proving_key,
             witness
           ),
         {proof, public_input} <- Cairo.prove(trace, memory, instance_str),
         verifying_key <-
           public_input
           |> Cairo.get_program_hash()
           |> :binary.list_to_bin(),
         instance <-
           public_input
           |> Instance.from_public_input() do
      {:ok,
       %__MODULE__{
         verifying_key: verifying_key,
         proof: proof |> :binary.list_to_bin(),
         instance: instance,
         public_input: public_input |> :binary.list_to_bin()
       }}
    else
      {:error, reason} ->
        {:error, reason}
    end
  end

  @doc """
  Verifies the given `ComplianceUnit`.

  This function takes a `ComplianceUnit` and verifies its proof against its public
  inputs. It converts the binary data of both the instance and the proof to
  lists before passing them to the `Cairo.verify/2` function. There is no
  verifying key in the Cairo system; instead, a program segment in the instance
  serves this purpose.  For efficiency, we provide a method to retrieve the
  verifying key from instance without explicitly using the verifying key field.

  ## Parameters

    - proof: A `ComplianceUnit` containing the verifying key, proof and public
      inputs to be verified.

  ## Returns

    - `true` if the proof is valid.
    - `false` if the proof is invalid.
    - `{:error, term()}` if the verification encounters errors.

  """
  @spec verify(ComplianceUnit.t()) :: boolean() | {:error, term()}
  def verify(proof) do
    public_input =
      proof.public_input
      |> :binary.bin_to_list()

    proof.proof
    |> :binary.bin_to_list()
    |> Cairo.verify(public_input)
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
  @spec generate_compliance_proof(binary()) ::
          {:ok, t()} | {:error, term()}
  def generate_compliance_proof(witness) do
    dir =
      Path.join(:code.priv_dir(:anoma_lib), "params/cairo_compliance.json")

    with {:ok, compliance_proving_key} <- File.read(dir) do
      prove(compliance_proving_key, witness)
    else
      _ -> {:error, "cairo_compliance.json not found"}
    end
  end
end
