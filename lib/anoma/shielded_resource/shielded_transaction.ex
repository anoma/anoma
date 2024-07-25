defmodule Anoma.ShieldedResource.ShieldedTransaction do
  @moduledoc """
  I am a shielded resource machine transaction.
  """

  require Logger

  alias __MODULE__
  use TypedStruct
  alias Anoma.ShieldedResource.PartialTransaction

  typedstruct enforce: true do
    # TODO: The roots, commitments, and nullifiers can be eliminated. We can
    # obtain them from public inputs. Then we can make the same improvement for
    # transparent transactions. However, they are used in the executor atm.
    field(:roots, list(binary()), default: [])
    field(:commitments, list(binary()), default: [])
    field(:nullifiers, list(binary()), default: [])
    field(:partial_transactions, list(PartialTransaction.t()), default: [])
    field(:delta, binary(), default: %{})
  end

  @spec to_noun(t()) :: Noun.t()
  def to_noun(transaction = %ShieldedTransaction{}) do
    [
      transaction.roots,
      transaction.commitments,
      transaction.nullifiers,
      Noun.Nounable.to_noun(transaction.partial_transactions),
      transaction.delta
    ]
  end

  @spec from_noun(Noun.t()) :: {:ok, t()}
  def from_noun([
        roots,
        commitments,
        nullifiers,
        partial_transactions,
        delta
      ]) do
    ptxs =
      partial_transactions
      |> Noun.list_nock_to_erlang()
      |> Enum.map(&PartialTransaction.from_noun/1)

    checked = Enum.all?(ptxs, &(elem(&1, 0) == :ok))

    with true <- checked do
      {:ok,
       %ShieldedTransaction{
         roots: roots,
         commitments: commitments,
         nullifiers: nullifiers,
         partial_transactions: Enum.map(ptxs, &elem(&1, 1)),
         delta: delta
       }}
    end
  end

  @spec compose(t(), t()) :: t()
  def compose(tx1, tx2) do
    # I still don't know if proofs have to be unique...
    if Enum.any?(tx1.commitments, fn x -> x in tx2.commitments end) ||
         Enum.any?(tx1.nullifiers, fn x -> x in tx2.nullifiers end) do
      nil
    else
      %ShieldedTransaction{
        roots: tx1.roots ++ tx2.roots,
        commitments: tx1.commitments ++ tx2.commitments,
        nullifiers: tx1.nullifiers ++ tx2.nullifiers,
        partial_transactions:
          tx1.partial_transactions ++ tx2.partial_transactions,
        # fix delta when adding binding signature
        delta: tx1.delta
      }
    end
  end

  # TODO: We can return roots, commitments, and nullifiers instead of just a
  # boolean value so that we can get rid of them in the ShieldedTransaction struct. We
  # can apply the same improvement to the transparent Transaction.
  @spec verify(t()) :: boolean()
  def verify(transaction = %__MODULE__{}) do
    # check proofs
    all_proofs_valid =
      for ptx <- transaction.partial_transactions,
          reduce: true do
        acc ->
          result = PartialTransaction.verify(ptx)
          Logger.debug("partial_transactions result: #{inspect(result)}")
          acc && result
      end

    # Decode the compliance_output
    compliance_outputs =
      transaction.partial_transactions
      |> Enum.flat_map(fn ptx ->
        ptx.compliance_proofs
        |> Enum.map(fn proof_record ->
          Anoma.SheildedResource.ComplianceOutput.from_public_input(
            proof_record.public_inputs
            |> :binary.bin_to_list()
          )
        end)
      end)

    # Collect binding public keys
    binding_pub_keys =
      compliance_outputs |> Enum.reduce([], &[&1.delta_x ++ &1.delta_y | &2])

    # Collect binding signature msgs
    binding_messages =
      compliance_outputs
      |> Enum.reduce([], &[&1.nullifier | [&1.output_cm | &2]])

    # delta check(verify the binding signature)
    list_delta = :binary.bin_to_list(transaction.delta)

    delta_valid =
      Cairo.sig_verify(binding_pub_keys, binding_messages, list_delta)

    # Collect resource logics from compliance proofs
    resource_logics_from_compliance =
      compliance_outputs
      |> Enum.reduce([], &[&1.output_label | [&1.input_label | &2]])
      |> Enum.reverse()

    # Compute the program hash of resource logic proofs
    resource_logic_from_program =
      transaction.partial_transactions
      |> Enum.flat_map(fn ptx ->
        ptx.logic_proofs
        |> Enum.map(fn proof_record ->
          Cairo.get_program_hash(
            proof_record.public_inputs
            |> :binary.bin_to_list()
          )
        end)
      end)

    resource_logic_valid =
      resource_logics_from_compliance == resource_logic_from_program

    all_proofs_valid && delta_valid && resource_logic_valid
  end
end
