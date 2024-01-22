defmodule Anoma.Resource.Transaction do
  @moduledoc """
  I represent a resource machine transaction
  """

  require Logger

  alias __MODULE__
  use TypedStruct

  import Anoma.Resource
  alias Anoma.Resource.Delta
  alias Anoma.Resource.ProofRecord

  # doesn't have all the fields yet.
  typedstruct enforce: true do
    field(:roots, list(binary()), default: [])
    field(:commitments, list(binary()), default: [])
    field(:nullifiers, list(binary()), default: [])
    field(:proofs, list(ProofRecord.t()), default: [])
    field(:delta, Delta.t(), default: %{})
    field(:extra, list(binary()), default: [])
    field(:preference, term(), default: nil)
  end

  # preference function not yet supported
  @spec to_noun(t()) :: Noun.t()
  def to_noun(transaction = %Transaction{}) do
    [
      transaction.roots,
      transaction.commitments,
      transaction.nullifiers,
      for proof <- transaction.proofs do
        ProofRecord.to_noun(proof)
      end,
      Delta.to_noun(transaction.delta),
      transaction.extra
      | [[1 | 0], 0 | 0]
    ]
  end

  @spec from_noun(Noun.t()) :: t()
  def from_noun([
        roots,
        commitments,
        nullifiers,
        proofs,
        delta,
        extra | _preference
      ]) do
    %Transaction{
      roots: roots,
      commitments: commitments,
      nullifiers: nullifiers,
      proofs:
        for proof <- proofs do
          ProofRecord.from_noun(proof)
        end,
      delta: Delta.from_noun(delta),
      extra: extra,
      preference: nil
    }
  end

  @spec verify(t()) :: boolean()
  def verify(transaction) do
    # the transparent proofs are just all the involved resources
    proved_resources =
      for proof_record <- transaction.proofs do
        proof_record.proof.resource
      end

    # todo: check that this is an exact partition
    {committed, nullified} =
      partition_resources(
        proved_resources,
        transaction.commitments,
        transaction.nullifiers
      )

    Logger.debug("committed resource set: #{inspect(committed)}")
    Logger.debug("nullified resource set: #{inspect(nullified)}")

    resource_set_valid =
      length(committed) == length(transaction.commitments) &&
        length(nullified) == length(transaction.nullifiers)

    Logger.debug("resource set valid: #{inspect(resource_set_valid)}")

    committed_delta_sum =
      for %{resource: r} <- committed, reduce: %{} do
        sum ->
          Logger.debug("running committed delta sum: #{inspect(sum)}")
          Delta.add(sum, delta(r))
      end

    Logger.debug("committed delta sum: #{inspect(committed_delta_sum)}")

    nullified_delta_sum =
      for %{resource: r} <- nullified, reduce: %{} do
        sum ->
          Logger.debug("running nullified delta sum: #{inspect(sum)}")
          Delta.add(sum, delta(r))
      end

    Logger.debug("nullified delta sum: #{inspect(nullified_delta_sum)}")

    tx_delta_sum = Delta.sub(committed_delta_sum, nullified_delta_sum)
    Logger.debug("summed deltas: #{inspect(tx_delta_sum)}")

    delta_valid = tx_delta_sum == transaction.delta
    Logger.debug("delta valid: #{inspect(delta_valid)}")

    # now run the resource logics, passing the transactions
    all_logics_valid =
      for resource <- proved_resources, reduce: true do
        acc ->
          result = transparent_run_resource_logic(transaction, resource)
          Logger.debug("resource logic result: #{inspect(result)}")
          acc && result
      end

    Logger.debug("all logics valid: #{inspect(all_logics_valid)}")

    resource_set_valid && delta_valid && all_logics_valid
  end

  # todo: not efficient
  def partition_resources(resources, commitments, nullifiers) do
    Logger.debug(
      "partitioning resources into #{inspect(length(commitments))}c, #{inspect(length(nullifiers))}n: #{inspect(resources)}"
    )

    # silly hack. will be deleting this function anyway
    commitments = [<<>> | commitments]
    nullifiers = [<<>> | nullifiers]

    {committed_set, nullified_set} =
      for r <- resources,
          c <- commitments,
          n <- nullifiers,
          reduce: {MapSet.new(), MapSet.new()} do
        {committed, nullified} ->
          cond do
            c |> commits_to(r) ->
              {MapSet.put(committed, %{commitment: c, resource: r}),
               nullified}

            n |> nullifies(r) ->
              {committed, MapSet.put(nullified, %{nullifier: n, resource: r})}

            true ->
              {committed, nullified}
          end
      end

    {MapSet.to_list(committed_set), MapSet.to_list(nullified_set)}
  end
end
