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
    # The roots seems redundant here as we can get the roots from the instance
    # of actions
    field(:roots, MapSet.t(binary()), default: MapSet.new())
    field(:actions, MapSet.t(Action.t()), default: MapSet.new())

    # When the tx is not finalized/signed, the delta_proof is the collection of
    # private keys. When the tx is finalized/signed, the delta_proof is the
    # binding signature/proof
    field(:delta_proof, binary(), default: <<>>)
  end

  @spec commitments(t()) :: list(binary())
  def commitments(transaction = %Transaction{}) do
    transaction.actions
    |> Enum.flat_map(& &1.created_commitments)
  end

  @spec nullifiers(t()) :: list(binary())
  def nullifiers(transaction = %Transaction{}) do
    transaction.actions
    |> Enum.flat_map(& &1.consumed_nullifiers)
  end

  @spec compose(t(), t()) :: t()
  def compose(tx1, tx2) do
    %Transaction{
      roots: MapSet.union(tx1.roots, tx2.roots),
      actions: MapSet.union(tx1.actions, tx2.actions),
      delta_proof: tx1.delta_proof <> tx2.delta_proof
    }
  end

  @spec prove_delta(Transaction.t()) :: Transaction.t()
  def prove_delta(tx = %Transaction{}) do
    tx |> sign()
  end

  @spec verify(t()) :: true | {:error, String.t()}
  def verify(transaction = %Transaction{}) do
    with true <- verify_actions(transaction),
         true <- verify_duplicate_nfs(transaction),
         true <- verify_delta(transaction) do
      true
    else
      reason -> reason
    end
  end

  @doc """
  Gets the Cairo Poseidon commitment tree specification.
  """
  @spec cm_tree() :: CommitmentTree.t()
  def cm_tree() do
    CommitmentTree.new(
      CommitmentTree.Spec.cairo_poseidon_cm_tree_spec(),
      nil
    )
  end

  @doc """
  Retrieves the cipher texts from the given transaction.

  ## Returns
    - A list of tuples where each tuple contains a binary tag(commitment) and a
      cipher text(a list of binary).
  """
  @spec get_cipher_texts(Transaction.t()) ::
          list(%{tag: binary(), cipher: list(binary())})
  def get_cipher_texts(tx) do
    tx.actions
    |> Enum.flat_map(& &1.logic_proofs)
    |> Enum.map(fn {_tag, {_logic_hash, proof_record}} ->
      proof_record.instance
      |> LogicInstance.from_public_input()
    end)
    |> Enum.map(&%{tag: &1.tag, cipher: &1.cipher})
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
    binding_messages = get_binding_messages(tx)

    case Cairo.sig_verify(
           binding_pub_keys,
           binding_messages,
           tx.delta_proof |> :binary.bin_to_list()
         ) do
      true -> true
      _ -> {:error, "Delta proof verification failure"}
    end
  end

  @spec verify_duplicate_nfs(Transaction.t()) ::
          true | {:error, String.t()}
  defp verify_duplicate_nfs(tx) do
    nullifiers = Transaction.nullifiers(tx)

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
      action.compliance_units
      |> Enum.map(fn proof_record ->
        proof_record.instance
        |> ComplianceInstance.from_public_input()
      end)
    end)
    |> Enum.reduce(
      [],
      &[:binary.bin_to_list(&1.delta_x <> &1.delta_y) | &2]
    )
  end

  @spec get_binding_messages(Transaction.t()) :: list(list(byte()))
  defp get_binding_messages(tx = %Transaction{}) do
    (Transaction.nullifiers(tx) ++
       Transaction.commitments(tx))
    |> Enum.map(&:binary.bin_to_list/1)
  end

  @spec sign(Transaction.t()) :: Transaction.t()
  defp sign(tx = %Transaction{}) do
    msgs = get_binding_messages(tx)

    binding_signature =
      tx.delta_proof
      |> :binary.bin_to_list()
      |> Cairo.sign(msgs)
      |> :binary.list_to_bin()

    %Transaction{tx | delta_proof: binding_signature}
  end

  @spec from_noun(Noun.t()) :: {:ok, t()} | :error
  def from_noun([
        actions
        | delta_proof
      ]) do
    actions =
      actions
      |> Noun.list_nock_to_erlang()
      |> Enum.map(&Action.from_noun/1)

    checked = Enum.all?(actions, &(elem(&1, 0) == :ok))

    actions = MapSet.new(Enum.map(actions, fn {:ok, x} -> x end))

    if checked do
      {:ok,
       %Transaction{
         actions: actions,
         delta_proof: delta_proof
       }}
    else
      :error
    end
  end

  defimpl Noun.Nounable, for: __MODULE__ do
    @impl true
    def to_noun(transaction = %Transaction{}) do
      [
        Enum.map(transaction.actions, &Noun.Nounable.to_noun/1)
        | transaction.delta_proof
      ]
    end
  end

  @doc """
    The arguments are JSON strings of compliance units, input logics, input
    witnesses, output logics, and output witnesses.
  """
  @spec create_from_compliance_units(
          list(binary()),
          list(binary()),
          list(binary()),
          list(binary()),
          list(binary())
        ) ::
          {:ok, Transaction.t()} | {:error, term()}
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
         input_is_consumed_flags =
           Enum.map(input_resources, fn _ -> true end),
         {:ok, updated_input_witnesses} <-
           Workflow.update_witnesses(
             input_witnesses,
             input_resources,
             input_is_consumed_flags,
             input_nf_keys,
             input_paths
           ),
         output_is_consumed_flags =
           Enum.map(output_resources, fn _ -> false end),
         {:ok, updated_output_witnesses} <-
           Workflow.update_witnesses(
             output_witnesses,
             output_resources,
             output_is_consumed_flags,
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
         {:ok, compliance_witness} <-
           Workflow.create_compliance_inputs(
             compliance_input_jsons,
             input_resources,
             output_resources
           ),
         {:ok, compliance_units} <-
           Workflow.generate_compliance_proofs(compliance_witness),
         action =
           Workflow.create_action(
             output_commitments,
             input_nullifiers,
             input_logic_proofs,
             output_logic_proofs,
             compliance_units
           ),
         {:ok, priv_keys} <-
           Workflow.create_private_keys(compliance_input_jsons) do
      {:ok,
       %Transaction{
         actions: MapSet.new([action]),
         delta_proof: priv_keys
       }}
    else
      {:error, x} -> {:error, x}
    end
  end
end
