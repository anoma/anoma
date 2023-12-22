defmodule Anoma.Transaction do
  @moduledoc """
  I represent a resource machine transaction
  """

  alias __MODULE__
  use TypedStruct

  import Anoma.Resource
  alias Anoma.Delta

  # doesn't have all the fields yet.
  typedstruct enforce: true do
    field(:roots, list(binary()), default: [])
    field(:commitments, list(binary()), default: [])
    field(:nullifiers, list(binary()), default: [])
    field(:proofs, list(Anoma.ProofRecord.t()), default: [])
    field(:delta, Anoma.Delta.t(), default: %{})
    field(:extra, list(binary()), default: [])
    field(:preference, term(), default: nil)
  end

  # stub for testing. real function will convert the whole tx
  # (when that's ready on the nock side)
  def to_noun(transaction = %Transaction{}) do
    Delta.to_noun(transaction.delta)
  end

  def verify(transaction) do
    # the transparent proofs are just all the involved resources
    resources =
      for proof_record <- transaction.proofs do
        proof_record.proof.resource
      end

    # todo: check that this is an exact partition
    {committed, nullified} =
      partition_resources(resources, transaction.commitments, transaction.nullifiers)

    resource_set_valid =
      length(committed) == length(transaction.commitments) &&
        length(nullified) == length(transaction.nullifiers)

    IO.inspect(resource_set_valid, label: "resource set valid")

    committed_delta_sum =
      for %{resource: r} <- committed, reduce: %{} do
        sum ->
          IO.inspect(sum, label: "running committed delta sum")
          Delta.add(sum, delta(r))
      end

    IO.inspect(committed_delta_sum, label: "committed delta sum")

    nullified_delta_sum =
      for %{resource: r} <- nullified, reduce: %{} do
        sum ->
          IO.inspect(sum, label: "running nullified delta sum")
          Delta.add(sum, delta(r))
      end

    IO.inspect(nullified_delta_sum, label: "nullified delta sum")

    tx_delta_sum = Delta.sub(committed_delta_sum, nullified_delta_sum)
    IO.inspect(tx_delta_sum, label: "summed deltas")

    delta_valid = tx_delta_sum == transaction.delta
    IO.inspect(delta_valid, label: "delta valid")

    # now run the resource logics, passing the transactions
    logic_valid =
      for resource <- resources, reduce: true do
        acc ->
          result = transparent_run_resource_logic(transaction, resource)
          IO.inspect(result, label: "ran resource logic")
          acc && result
      end

    IO.inspect(logic_valid, label: "all logics valid")

    resource_set_valid && delta_valid && logic_valid
  end

  # todo: not efficient
  def partition_resources(resources, commitments, nullifiers) do
    {committed_set, nullified_set} =
      for r <- resources,
          c <- commitments,
          n <- nullifiers,
          reduce: {MapSet.new(), MapSet.new()} do
        {committed, nullified} ->
          cond do
            c |> commits_to(r) ->
              {MapSet.put(committed, %{commitment: c, resource: r}), nullified}

            n |> nullifies(r) ->
              {committed, MapSet.put(nullified, %{nullifier: n, resource: r})}

            true ->
              {committed, nullified}
          end
      end

    {MapSet.to_list(committed_set), MapSet.to_list(nullified_set)}
  end
end
