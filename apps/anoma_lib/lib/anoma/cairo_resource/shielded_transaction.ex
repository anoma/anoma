defmodule Anoma.CairoResource.Transaction do
  @moduledoc """
  I am a shielded resource machine transaction.
  """

  @behaviour Noun.Nounable.Kind

  require Logger

  alias __MODULE__
  use TypedStruct

  alias Anoma.CairoResource.{
    Action,
    ComplianceInstance,
    ProofRecord,
    Resource,
    LogicInstance,
    Utils,
    Workflow
  }

  typedstruct enforce: true do
    # TODO: The roots, commitments, and nullifiers can be eliminated. We can
    # obtain them from public inputs. Then we can make the same improvement for
    # transparent transactions. However, they are used in the executor atm.
    field(:roots, list(<<_::256>>), default: [])
    field(:commitments, list(<<_::256>>), default: [])
    field(:nullifiers, list(<<_::256>>), default: [])
    field(:actions, list(Action.t()), default: [])

    # When the tx is not finalized, the delta is the collection of private keys
    # When the tx is finalized, the delta is the binding signature
    field(:delta, binary(), default: <<>>)
  end

  @spec from_noun(Noun.t()) :: {:ok, t()} | :error
  def from_noun([
        roots,
        commitments,
        nullifiers,
        actions
        | delta
      ]) do
    actions =
      actions
      |> Noun.list_nock_to_erlang()
      |> Enum.map(&Action.from_noun/1)

    checked = Enum.all?(actions, &(elem(&1, 0) == :ok))

    if checked do
      {:ok,
       %Transaction{
         roots: roots |> Noun.list_nock_to_erlang(),
         commitments: commitments |> Noun.list_nock_to_erlang(),
         nullifiers: nullifiers |> Noun.list_nock_to_erlang(),
         actions: Enum.map(actions, &elem(&1, 1)),
         delta: delta
       }}
    else
      :error
    end
  end

  defimpl Noun.Nounable, for: __MODULE__ do
    @impl true
    def to_noun(transaction = %Transaction{}) do
      {
        transaction.roots,
        transaction.commitments,
        transaction.nullifiers,
        transaction.actions,
        transaction.delta
      }
      |> Noun.Nounable.to_noun()
    end
  end

  defimpl Anoma.RM.Transaction, for: __MODULE__ do
    @impl true
    def commitments(%Transaction{commitments: cm}), do: cm
    @impl true
    def nullifiers(%Transaction{nullifiers: nf}), do: nf

    @impl true
    def storage_commitments(tx), do: commitments(tx)
    @impl true
    def storage_nullifiers(tx), do: nullifiers(tx)

    @impl true
    def compose(tx1, tx2) do
      unless Anoma.RM.Trans.compose_pre_check(tx1, tx2) do
        nil
      else
        %Transaction{
          roots: tx1.roots ++ tx2.roots,
          commitments: tx1.commitments ++ tx2.commitments,
          nullifiers: tx1.nullifiers ++ tx2.nullifiers,
          actions: tx1.actions ++ tx2.actions,
          delta: tx1.delta <> tx2.delta
        }
      end
    end

    # TODO: We can return roots, commitments, and nullifiers instead of just a
    # boolean value so that we can get rid of them in the Transaction struct. We
    # can apply the same improvement to the transparent Transaction.
    @impl true
    def verify(transaction = %Transaction{}) do
      with true <- verify_proofs(transaction),
           true <- verify_duplicate_nfs(transaction),
           compliance_instances = decode_compliance_instances(transaction),
           true <- verify_delta(transaction, compliance_instances),
           true <- verify_resource_logic(transaction, compliance_instances) do
        true
      else
        reason -> reason
      end
    end

    @spec verify_proofs(Transaction.t()) :: true | {:error, String.t()}
    defp verify_proofs(tx) do
      failed =
        Enum.reject(tx.actions, &Action.verify/1)

      Enum.empty?(failed) or
        {:error, "Compliance proofs or logic proofs verification failure"}
    end

    defp decode_compliance_instances(tx) do
      tx.actions
      |> Enum.flat_map(fn action ->
        action.compliance_proofs
        |> Enum.map(fn proof_record ->
          ComplianceInstance.from_public_input(proof_record.public_inputs)
        end)
      end)
    end

    @spec verify_delta(Transaction.t(), list(binary())) ::
            true | {:error, String.t()}
    defp verify_delta(tx, compliance_instances) do
      # Collect binding public keys
      binding_pub_keys = get_binding_pub_keys(compliance_instances)

      # Collect binding signature msgs
      binding_messages = Transaction.get_binding_messages(tx)

      case Cairo.sig_verify(
             binding_pub_keys,
             binding_messages,
             tx.delta |> :binary.bin_to_list()
           ) do
        true -> true
        _ -> {:error, "Delta proof verification failure"}
      end
    end

    @spec verify_resource_logic(Transaction.t(), list(binary())) ::
            true | {:error, String.t()}
    defp verify_resource_logic(tx, compliance_instances) do
      # Collect resource logics from compliance proofs
      resource_logics_from_compliance =
        compliance_instances
        |> Enum.reduce([], &[&1.output_logic | [&1.input_logic | &2]])
        |> Enum.reverse()

      # Compute the program hash of resource logic proofs
      resource_logic_from_program =
        tx.actions
        |> Enum.flat_map(fn action ->
          action.logic_proofs
          |> Enum.map(&ProofRecord.get_cairo_program_hash/1)
        end)

      if resource_logics_from_compliance == resource_logic_from_program do
        true
      else
        {:error, "Resource logic check failure"}
      end
    end

    @spec verify_duplicate_nfs(Transaction.t()) ::
            true | {:error, String.t()}
    defp verify_duplicate_nfs(tx) do
      if Enum.uniq(tx.nullifiers) == tx.nullifiers do
        true
      else
        {:error, "Duplicate nullifiers error"}
      end
    end

    @spec get_binding_pub_keys(list(binary())) :: list(byte())
    defp get_binding_pub_keys(compliance_instances) do
      compliance_instances
      |> Enum.reduce(
        [],
        &[:binary.bin_to_list(&1.delta_x <> &1.delta_y) | &2]
      )
    end
  end

  @spec get_compliance_instances(t()) :: list(ComplianceInstance.t())
  def get_compliance_instances(transaction) do
    transaction.actions
    |> Enum.flat_map(fn action ->
      action.compliance_proofs
      |> Enum.map(fn proof_record ->
        proof_record.public_inputs
        |> ComplianceInstance.from_public_input()
      end)
    end)
  end

  @spec get_binding_messages(Transaction.t()) :: list(list(byte()))
  def get_binding_messages(tx = %Transaction{}) do
    (tx.nullifiers ++ tx.commitments)
    |> Enum.map(&:binary.bin_to_list/1)
  end

  # sign(binding signature) the transation when the transaction is balanced and finalized
  @spec sign(Transaction.t()) :: Transaction.t()
  defp sign(tx = %Transaction{}) do
    msgs = get_binding_messages(tx)

    binding_signature =
      tx.delta
      |> :binary.bin_to_list()
      |> Cairo.sign(msgs)
      |> :binary.list_to_bin()

    %Transaction{tx | delta: binding_signature}
  end

  # complete and sign the tx
  @spec finalize(Transaction.t()) :: Transaction.t()
  def finalize(tx = %Transaction{}) do
    compliance_instances = get_compliance_instances(tx)
    roots = Enum.map(compliance_instances, & &1.root)
    commitments = Enum.map(compliance_instances, & &1.output_cm)
    nullifiers = Enum.map(compliance_instances, & &1.nullifier)

    %Transaction{
      tx
      | roots: roots,
        commitments: commitments,
        nullifiers: nullifiers
    }
    |> sign()
  end

  @spec create_from_compliance_units(
          list(Jason.OrderedObject.t()),
          list(binary()),
          list(Jason.OrderedObject.t()),
          list(binary()),
          list(Jason.OrderedObject.t())
        ) ::
          {:ok, Transaction.t()} | {:error, term()}
  @doc """
    The arguments are JSON strings of compliance units, input logics, input
    witnesses, output logics, and output witnesses.
  """
  def create_from_compliance_units(
        compliance_units,
        input_logics,
        input_witnesses,
        output_logics,
        output_witnesses
      ) do
    with input_resource_jsons =
           Enum.map(compliance_units, & &1["input"]),
         output_resource_jsons =
           Enum.map(compliance_units, & &1["output"]),
         {:ok, input_nf_keys} <-
           Enum.map(
             compliance_units,
             &Utils.parse_json_field_to_binary32(&1, "input_nf_key")
           )
           |> Utils.check_list(),
         {:ok, input_resources} <-
           Workflow.get_input_resources(
             input_resource_jsons,
             input_logics,
             input_nf_keys
           ),
         input_nullifiers =
           Enum.zip_with(
             input_resources,
             input_nf_keys,
             &Resource.nullifier/2
           ),
         {:ok, output_resources} <-
           Workflow.get_output_resources(
             output_resource_jsons,
             output_logics,
             input_nullifiers
           ),
         output_commitments =
           Enum.map(output_resources, &Resource.commitment/1),
         {:ok, input_paths, output_paths} <-
           Workflow.create_merkle_tree_paths(
             input_nullifiers,
             output_commitments
           ),
         {:ok, updated_input_witnesses} <-
           Workflow.update_witnesses(
             input_witnesses,
             input_resources,
             input_nf_keys,
             input_paths
           ),
         {:ok, updated_output_witnesses} <-
           Workflow.update_witnesses(
             output_witnesses,
             output_resources,
             input_nf_keys,
             output_paths
           ),
         {:ok, input_logic_proofs} <-
           Workflow.generate_resource_logic_proofs(
             input_logics,
             updated_input_witnesses
           ),
         {:ok, output_logic_proofs} <-
           Workflow.generate_resource_logic_proofs(
             output_logics,
             updated_output_witnesses
           ),
         {:ok, compliance_inputs} <-
           Workflow.create_compliance_inputs(
             compliance_units,
             input_resources,
             output_resources
           ),
         {:ok, compliance_proofs} <-
           Workflow.generate_compliance_proofs(compliance_inputs),
         action =
           Workflow.create_action(
             input_logic_proofs,
             output_logic_proofs,
             compliance_proofs
           ),
         {:ok, priv_keys} <-
           Workflow.create_private_keys(compliance_units) do
      {:ok,
       %Transaction{
         actions: [action],
         delta: priv_keys
       }}
    else
      {:error, x} -> {:error, x}
    end
  end

  @spec cm_tree() :: CommitmentTree.t()
  def cm_tree() do
    CommitmentTree.new(
      CommitmentTree.Spec.cairo_poseidon_cm_tree_spec(),
      nil
    )
  end

  @doc """
  Retrieves the cipher texts from the given transaction.

  ## Parameters
    - tx: A `Transaction` struct containing the transaction data.

  ## Returns
    - A list of tuples where each tuple contains a binary tag(commitment) and a cipher text(a list of binary).
  """
  @spec get_cipher_texts(Transaction.t()) ::
          list(%{tag: binary(), cipher: list(binary())})
  def get_cipher_texts(tx) do
    tx.actions
    |> Enum.flat_map(& &1.logic_proofs)
    |> Enum.map(&LogicInstance.from_public_input(&1.public_inputs))
    |> Enum.map(&%{tag: &1.tag, cipher: &1.cipher})
  end
end
