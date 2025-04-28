defmodule Anoma.CairoResource.Logic.ProofRecord do
  @moduledoc """
  I am a proof record for a shielded resource.
  """

  alias __MODULE__
  use TypedStruct

  alias Anoma.CairoResource.Logic.Instance

  typedstruct enforce: true do
    field(:verifying_key, binary(), default: <<>>)
    field(:proof, binary(), default: <<>>)
    field(:instance, Instance.t(), default: %Instance{})
    # public_input contains other necessary information for the cairo proof,
    # including the full vk, public memory, etc.
    field(:public_input, binary(), default: <<>>)
  end

  @spec from_noun(Noun.t()) :: {:ok, t()} | :error
  def from_noun([proof | public_input]) do
    instance =
      public_input
      |> :binary.bin_to_list()
      |> Instance.to_instance()

    verifying_key =
      public_input
      |> :binary.bin_to_list()
      |> Cairo.get_program_hash()
      |> :binary.list_to_bin()

    {:ok,
     %ProofRecord{
       verifying_key: verifying_key,
       proof: Noun.atom_integer_to_binary(proof),
       instance: instance,
       public_input: Noun.atom_integer_to_binary(public_input)
     }}
  end

  defimpl Noun.Nounable, for: __MODULE__ do
    @impl true
    def to_noun(proof_record = %ProofRecord{}) do
      [
        proof_record.proof
        | proof_record.public_input
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

    - `{:ok, %ProofRecord{}}`: A tuple containing `:ok` and a `ProofRecord`
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
         {proof, public_input} <-
           Cairo.prove(trace, memory, instance_str),
         verifying_key <-
           public_input
           |> Cairo.get_program_hash()
           |> :binary.list_to_bin(),
         instance <-
           public_input
           |> Instance.to_instance() do
      {:ok,
       %ProofRecord{
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
  Verifies the given `ProofRecord`.

  This function takes a `ProofRecord` and verifies its proof against its public
  inputs. It converts the binary data of both the instance and the proof to
  lists before passing them to the `Cairo.verify/2` function. There is no
  verifying key in the Cairo system; instead, a program segment in the instance
  serves this purpose.  For efficiency, we provide a method to retrieve the
  verifying key from instance without explicitly using the verifying key field.

  ## Parameters

    - proof: A `ProofRecord` containing the verifying key, proof and public
      inputs to be verified.

  ## Returns

    - `true` if the proof is valid.
    - `false` if the proof is invalid.
    - `{:error, term()}` if the verification encounters errors.

  """
  @spec verify(ProofRecord.t()) :: boolean() | {:error, term()}
  def verify(pr) do
    public_input =
      pr.public_input
      |> :binary.bin_to_list()

    pr.proof
    |> :binary.bin_to_list()
    |> Cairo.verify(public_input)
  end

  @spec padding_resource_logic_proving_key() ::
          {:error, atom()} | {:ok, binary()}
  def padding_resource_logic_proving_key() do
    proving_key_dir =
      Path.join(
        :code.priv_dir(:anoma_lib),
        "params/trivial_resource_logic.json"
      )

    File.read(proving_key_dir)
  end
end
