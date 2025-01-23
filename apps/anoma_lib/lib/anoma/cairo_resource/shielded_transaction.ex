defmodule Anoma.CairoResource.Transaction do
  @moduledoc """
  I am a shielded resource machine transaction.
  """

  @behaviour Noun.Nounable.Kind

  alias __MODULE__
  alias Anoma.CairoResource.Action
  alias Anoma.CairoResource.ComplianceInstance
  alias Anoma.CairoResource.LogicInstance
  alias Anoma.CairoResource.Resource
  alias Anoma.CairoResource.Utils
  alias Anoma.CairoResource.Workflow

  require Logger

  use TypedStruct

  typedstruct enforce: true do
    # roots: A set of valid commitment tree roots used to prove the existence of the
    # resources being consumed in the transaction.
    field(:roots, MapSet.t(binary()), default: MapSet.new())
    field(:actions, list(Action.t()), default: [])

    # When the tx is not finalized, the delta is the collection of private keys
    # When the tx is finalized, the delta is the binding signature
    field(:delta, binary(), default: <<>>)
  end

  @spec from_noun(Noun.t()) :: {:ok, t()} | :error
  def from_noun([
        actions
        | delta
      ]) do
    actions =
      actions
      |> Noun.list_nock_to_erlang()
      |> Enum.map(&Action.from_noun/1)

    checked = Enum.all?(actions, &(elem(&1, 0) == :ok))

    actions = Enum.map(actions, fn {:ok, x} -> x end)
    compliance_instances = get_compliance_instances(actions)
    roots = Enum.map(compliance_instances, & &1.root)

    if checked do
      {:ok,
       %Transaction{
         roots: MapSet.new(roots),
         actions: actions,
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
        transaction.actions,
        transaction.delta
      }
      |> Noun.Nounable.to_noun()
    end
  end

  defimpl Anoma.RM.Transaction, for: __MODULE__ do
    @impl true
    def commitments(transaction = %Transaction{}) do
      transaction.actions
      |> Enum.flat_map(& &1.created_commitments)
    end

    @impl true
    def nullifiers(transaction = %Transaction{}) do
      transaction.actions
      |> Enum.flat_map(& &1.consumed_nullifiers)
    end

    @impl true
    def compose(tx1, tx2) do
      unless Anoma.RM.Trans.compose_pre_check(tx1, tx2) do
        nil
      else
        %Transaction{
          roots: MapSet.union(tx1.roots, tx2.roots),
          actions: tx1.actions ++ tx2.actions,
          delta: tx1.delta <> tx2.delta
        }
      end
    end

    @impl true
    def verify(transaction = %Transaction{}) do
      with true <- verify_actions(transaction),
           true <- verify_duplicate_nfs(transaction),
           true <- verify_delta(transaction) do
        true
      else
        reason -> reason
      end
    end

    @spec verify_actions(Transaction.t()) :: true | {:error, String.t()}
    defp verify_actions(tx) do
      failed =
        Enum.reject(tx.actions, &Action.verify/1)

      Enum.empty?(failed) or
        {:error, "Compliance proofs or logic proofs verification failure"}
    end

    @spec verify_delta(Transaction.t()) ::
            true | {:error, String.t()}
    defp verify_delta(tx) do
      # Collect binding public keys
      binding_pub_keys = get_binding_pub_keys(tx)

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

    @spec verify_duplicate_nfs(Transaction.t()) ::
            true | {:error, String.t()}
    defp verify_duplicate_nfs(tx) do
      nullifiers = Anoma.RM.Transaction.nullifiers(tx)

      if Enum.uniq(nullifiers) == nullifiers do
        true
      else
        {:error, "Duplicate nullifiers error"}
      end
    end

    @spec get_binding_pub_keys(Transaction.t()) :: list(byte())
    defp get_binding_pub_keys(tx) do
      tx.actions
      |> Enum.flat_map(fn action ->
        action.compliance_proofs
        |> Enum.map(fn proof_record ->
          proof_record.public_inputs
          |> ComplianceInstance.from_public_input()
        end)
      end)
      |> Enum.reduce(
        [],
        &[:binary.bin_to_list(&1.delta_x <> &1.delta_y) | &2]
      )
    end
  end

  @spec get_compliance_instances(list(Action.t())) ::
          list(ComplianceInstance.t())
  def get_compliance_instances(actions) do
    actions
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
    (Anoma.RM.Transaction.nullifiers(tx) ++
       Anoma.RM.Transaction.commitments(tx))
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
    compliance_instances = get_compliance_instances(tx.actions)
    roots = Enum.map(compliance_instances, & &1.root)

    %Transaction{
      tx
      | roots: MapSet.new(roots)
    }
    |> sign()
  end

  @spec create_from_compliance_units(
          list(binary()),
          list(binary()),
          list(binary()),
          list(binary()),
          list(binary())
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
    with {:ok, compliance_input_jsons} <-
           Enum.map(
             compliance_units,
             &Jason.decode(&1, objects: :ordered_objects)
           )
           |> Utils.check_list(),
         input_resource_jsons =
           Enum.map(compliance_input_jsons, & &1["input"]),
         output_resource_jsons =
           Enum.map(compliance_input_jsons, & &1["output"]),
         {:ok, input_nf_keys} <-
           Enum.map(
             compliance_input_jsons,
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
             compliance_input_jsons,
             input_resources,
             output_resources
           ),
         {:ok, compliance_proofs} <-
           Workflow.generate_compliance_proofs(compliance_inputs),
         action =
           Workflow.create_action(
             output_commitments,
             input_nullifiers,
             input_logic_proofs,
             output_logic_proofs,
             compliance_proofs
           ),
         {:ok, priv_keys} <-
           Workflow.create_private_keys(compliance_input_jsons) do
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
    |> Enum.map(fn {_tag, proof_record} ->
      proof_record.public_inputs
      |> LogicInstance.from_public_input()
    end)
    |> Enum.map(&%{tag: &1.tag, cipher: &1.cipher})
  end
end
