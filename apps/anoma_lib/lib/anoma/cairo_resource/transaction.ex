defmodule Anoma.CairoResource.Transaction do
  @moduledoc """
  I am a shielded resource machine transaction.
  """

  @behaviour Noun.Nounable.Kind

  alias __MODULE__
  alias Anoma.CairoResource.Action

  use TypedStruct

  typedstruct enforce: true do
    field(:actions, MapSet.t(Action.t()), default: MapSet.new())

    # When the tx is not finalized/signed, the delta_proof is the collection of
    # private keys. When the tx is finalized/signed, the delta_proof is the
    # binding signature/proof
    field(:delta_proof, binary(), default: <<>>)
  end

  @spec create(MapSet.t(Action.t()), binary()) :: t()
  def create(actions, delta_proof) do
    %__MODULE__{actions: actions, delta_proof: delta_proof}
  end

  @spec commitments(t()) :: list(binary())
  def commitments(transaction = %Transaction{}) do
    transaction.actions
    |> Enum.flat_map(&Action.commitments/1)
  end

  @spec nullifiers(t()) :: list(binary())
  def nullifiers(transaction = %Transaction{}) do
    transaction.actions
    |> Enum.flat_map(&Action.nullifiers/1)
  end

  @spec roots(t()) :: MapSet.t()
  def roots(transaction = %Transaction{}) do
    transaction.actions
    |> Enum.flat_map(&Action.roots/1)
    |> MapSet.new()
  end

  @spec compose(t(), t()) :: t()
  def compose(tx1, tx2) do
    %Transaction{
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
    CommitmentTree.new(CommitmentTree.Spec.cairo_poseidon_cm_tree_spec())
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
    |> Enum.flat_map(& &1.resource_logic_proofs)
    |> Enum.map(fn {tag, proof_record} ->
      %{tag: tag, cipher: proof_record.instance.cipher}
    end)
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
    binding_pub_keys = delta(tx)

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

  @spec delta(Transaction.t()) :: list(byte())
  defp delta(tx) do
    tx.actions
    |> Enum.flat_map(fn action ->
      Action.delta(action)
    end)
    |> Enum.reduce(
      [],
      &[:binary.bin_to_list(&1) | &2]
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

  @spec from_noun(Noun.t()) :: {:ok, Transaction.t()} | :error
  def from_noun([actions | proof]) do
    with {:ok, a_set} <- Noun.Nounable.MapSet.from_noun(actions),
         a_set <-
           Enum.into(a_set, MapSet.new(), fn a ->
             {:ok, act} = Action.from_noun(a)
             act
           end) do
      {:ok,
       %__MODULE__{
         actions: a_set,
         delta_proof: Noun.atom_integer_to_binary(proof)
       }}
    else
      _ -> :error
    end
  end

  defimpl Noun.Nounable, for: Transaction do
    @impl true
    def to_noun(t = %Transaction{}) do
      {t.actions, t.delta_proof} |> Noun.Nounable.to_noun()
    end
  end

  @doc """
    The arguments are JSON strings of compliance units, input logics, input
    witnesses, output logics, and output witnesses.
  """
  @spec create_from_compliance_units(
          list(Jason.OrderedObject.t()),
          list(binary()),
          list(Jason.OrderedObject.t()),
          list(binary()),
          list(Jason.OrderedObject.t())
        ) ::
          {:ok, Transaction.t()} | {:error, term()}
  def create_from_compliance_units(
        compliance_units,
        input_logics,
        input_witnesses,
        output_logics,
        output_witnesses
      ) do
    with {:ok, {action, delta_witness}} <-
           Action.create(
             compliance_units,
             input_logics,
             input_witnesses,
             output_logics,
             output_witnesses
           ) do
      {:ok, Transaction.create(MapSet.new([action]), delta_witness)}
    else
      {:error, x} -> {:error, x}
    end
  end
end
