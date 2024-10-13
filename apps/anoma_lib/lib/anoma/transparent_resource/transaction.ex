defmodule Anoma.TransparentResource.Transaction do
  use TypedStruct

  alias Anoma.TransparentResource.Action
  alias Anoma.TransparentResource.Delta
  alias __MODULE__

  typedstruct enforce: true do
    field(:roots, MapSet.t(binary()), default: MapSet.new())
    field(:actions, MapSet.t(Action.t()), default: MapSet.new())
    field(:delta, Delta.t(), default: %{})
    # useless field for shielded only.
    field(:delta_proof, <<>>, default: <<>>)
  end

  def verify(tx = %Transaction{}) do
    with true <- verify_tx_roots(tx),
         true <- verify_tx_action_distinctness(tx),
         true <- verify_tx_action_compliance(tx),
         true <- verify_tx_action_delta_sum(tx),
         true <- verify_tx_has_zero_delta(tx),
         true <- verify_tx_action_logics(tx) do
      true
    else
      _ -> false
    end
  end

  # every consumed resource referenced must exist in a referenced root
  def verify_tx_roots(_) do
    true
  end

  # actions must contain disjoint sets of commitments and nullifiers
  @spec verify_tx_action_distinctness(t()) :: boolean()
  def verify_tx_action_distinctness(tx = %Transaction{}) do
    comms = tx.actions |> Enum.map(& &1.commitments)
    nulls = tx.actions |> Enum.map(& &1.nullifiers)

    number_of = fn x -> x |> Stream.map(&MapSet.size/1) |> Enum.sum() end

    uniq_number_of = fn x ->
      x |> Enum.reduce(&MapSet.union/2) |> MapSet.size()
    end

    {comm_size, uniq_comm_size} = {number_of.(comms), uniq_number_of.(comms)}
    {null_size, uniq_null_size} = {number_of.(nulls), uniq_number_of.(nulls)}

    # TODO Add failure logging
    comm_size == uniq_comm_size && null_size == uniq_null_size
  end

  # actions must be compliant, i.e., contain a proof for each resource
  def verify_tx_action_compliance(_) do
    true
  end

  # the sum of all action deltas we compute here must equal
  # the transaction delta
  def verify_tx_action_delta_sum(%Transaction{
        actions: actions,
        delta: tx_delta
      }) do
    action_delta_sum =
      for action <- actions, reduce: %{} do
        acc -> Delta.add(acc, Action.delta(action))
      end

    action_delta_sum == tx_delta
  end

  # the tx's delta must be zero
  def verify_tx_has_zero_delta(%Transaction{delta: delta}) do
    delta == %{}
  end

  # all transaction logic proofs must pass
  @spec verify_tx_action_logics(t()) :: boolean()
  def verify_tx_action_logics(tx = %Transaction{}) do
    tx.actions
    |> Stream.flat_map(& &1.proofs)
    |> Enum.all?(&Anoma.TransparentResource.LogicProof.verify/1)
  end
end
