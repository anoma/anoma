defmodule Anoma.ShieldedResource.ShieldedTransaction do
  @moduledoc """
  I am a shielded resource machine transaction.
  """

  @behaviour Noun.Nounable.Kind

  require Logger

  alias __MODULE__
  use TypedStruct
  alias Anoma.ShieldedResource.PartialTransaction
  alias Anoma.ShieldedResource.ComplianceOutput
  alias Anoma.Node.DummyStorage, as: Storage

  typedstruct enforce: true do
    # TODO: The roots, commitments, and nullifiers can be eliminated. We can
    # obtain them from public inputs. Then we can make the same improvement for
    # transparent transactions. However, they are used in the executor atm.
    field(:roots, list(binary()), default: [])
    field(:commitments, list(binary()), default: [])
    field(:nullifiers, list(binary()), default: [])
    field(:partial_transactions, list(PartialTransaction.t()), default: [])

    # When the tx is not finalized, the delta is the collection of private keys
    # When the tx is finalized, the delta is the binding signature
    field(:delta, binary(), default: <<>>)
  end

  @spec from_noun(Noun.t()) :: {:ok, t()}
  def from_noun([
        roots,
        commitments,
        nullifiers,
        partial_transactions
        | delta
      ]) do
    ptxs =
      partial_transactions
      |> Noun.list_nock_to_erlang()
      |> Enum.map(&PartialTransaction.from_noun/1)

    checked = Enum.all?(ptxs, &(elem(&1, 0) == :ok))

    if checked do
      {:ok,
       %ShieldedTransaction{
         roots: roots |> Noun.list_nock_to_erlang(),
         commitments: commitments |> Noun.list_nock_to_erlang(),
         nullifiers: nullifiers |> Noun.list_nock_to_erlang(),
         partial_transactions: Enum.map(ptxs, &elem(&1, 1)),
         delta: delta
       }}
    else
      :error
    end
  end

  defimpl Noun.Nounable, for: __MODULE__ do
    def to_noun(transaction = %ShieldedTransaction{}) do
      {
        transaction.roots,
        transaction.commitments,
        transaction.nullifiers,
        transaction.partial_transactions,
        transaction.delta
      }
      |> Noun.Nounable.to_noun()
    end
  end

  defimpl Anoma.RM.Transaction, for: __MODULE__ do
    def commitments(%ShieldedTransaction{commitments: cm}), do: cm
    def nullifiers(%ShieldedTransaction{nullifiers: nf}), do: nf

    def storage_commitments(tx), do: commitments(tx)
    def storage_nullifiers(tx), do: nullifiers(tx)

    def compose(tx1, tx2) do
      unless Anoma.RM.Trans.compose_pre_check(tx1, tx2) do
        nil
      else
        %ShieldedTransaction{
          roots: tx1.roots ++ tx2.roots,
          commitments: tx1.commitments ++ tx2.commitments,
          nullifiers: tx1.nullifiers ++ tx2.nullifiers,
          partial_transactions:
            tx1.partial_transactions ++ tx2.partial_transactions,
          delta: tx1.delta <> tx2.delta
        }
      end
    end

    # TODO: We can return roots, commitments, and nullifiers instead of just a
    # boolean value so that we can get rid of them in the ShieldedTransaction struct. We
    # can apply the same improvement to the transparent Transaction.
    def verify(transaction = %ShieldedTransaction{}) do
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
            ComplianceOutput.from_public_input(proof_record.public_inputs)
          end)
        end)

      # Collect binding public keys
      binding_pub_keys = get_binding_pub_keys(compliance_outputs)

      # Collect binding signature msgs
      binding_messages = ShieldedTransaction.get_binding_messages(transaction)

      delta_valid =
        Cairo.sig_verify(
          binding_pub_keys,
          binding_messages,
          transaction.delta |> :binary.bin_to_list()
        )

      # Collect resource logics from compliance proofs
      resource_logics_from_compliance =
        compliance_outputs
        |> Enum.reduce([], &[&1.output_logic | [&1.input_logic | &2]])
        |> Enum.reverse()

      # Compute the program hash of resource logic proofs
      resource_logic_from_program =
        transaction.partial_transactions
        |> Enum.flat_map(fn ptx ->
          ptx.logic_proofs
          |> Enum.map(fn proof_record ->
            proof_record.public_inputs
            |> :binary.bin_to_list()
            |> Cairo.get_program_hash()
            |> :binary.list_to_bin()
          end)
        end)

      resource_logic_valid =
        resource_logics_from_compliance == resource_logic_from_program

      all_proofs_valid && delta_valid && resource_logic_valid
    end

    def cm_tree(_tx, _storage) do
      CommitmentTree.new(
        CommitmentTree.Spec.cairo_poseidon_cm_tree_spec(),
        nil
      )
    end

    def resource_existence_check(transaction, storage) do
      for root <- transaction.roots, reduce: true do
        acc ->
          root_key = ["rm", "commitment_root", root]
          acc && Storage.get(storage, root_key) == {:ok, true}
      end
    end

    @spec get_binding_pub_keys(list(binary())) :: list(byte())
    defp get_binding_pub_keys(compliance_outputs) do
      compliance_outputs
      |> Enum.reduce(
        [],
        &[:binary.bin_to_list(&1.delta_x <> &1.delta_y) | &2]
      )
    end
  end

  def get_compliance_outputs(transaction) do
    transaction.partial_transactions
    |> Enum.flat_map(fn ptx ->
      ptx.compliance_proofs
      |> Enum.map(fn proof_record ->
        proof_record.public_inputs
        |> ComplianceOutput.from_public_input()
      end)
    end)
  end

  @spec get_binding_messages(ShieldedTransaction.t()) :: list(list(byte()))
  def get_binding_messages(tx = %ShieldedTransaction{}) do
    (tx.nullifiers ++ tx.commitments)
    |> Enum.map(&:binary.bin_to_list/1)
  end

  # sign(binding signature) the transation when the transaction is balanced and finalized
  @spec sign(ShieldedTransaction.t()) :: ShieldedTransaction.t()
  defp sign(tx = %ShieldedTransaction{}) do
    msgs = get_binding_messages(tx)

    binding_signature =
      tx.delta
      |> :binary.bin_to_list()
      |> Cairo.sign(msgs)
      |> :binary.list_to_bin()

    %ShieldedTransaction{tx | delta: binding_signature}
  end

  # complete and sign the tx
  @spec finalize(ShieldedTransaction.t()) :: ShieldedTransaction.t()
  def finalize(tx = %ShieldedTransaction{}) do
    compliance_outputs = get_compliance_outputs(tx)
    roots = Enum.map(compliance_outputs, & &1.root)
    commitments = Enum.map(compliance_outputs, & &1.output_cm)
    nullifiers = Enum.map(compliance_outputs, & &1.nullifier)

    %ShieldedTransaction{
      tx
      | roots: roots,
        commitments: commitments,
        nullifiers: nullifiers
    }
    |> sign()
  end
end
