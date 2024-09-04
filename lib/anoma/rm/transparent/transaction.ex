defmodule Anoma.RM.Transparent.Transaction do
  @moduledoc """
  I represent a resource machine transaction
  """

  @behaviour Noun.Nounable.Kind

  require Logger

  alias Noun.Nounable
  alias __MODULE__
  use TypedStruct

  import Anoma.RM.Transparent.Resource
  alias Anoma.RM.Transparent.Resource
  alias Anoma.RM.Transparent.Delta
  alias Anoma.RM.Transparent.ProofRecord
  import Noun

  # doesn't have all the fields yet.
  typedstruct enforce: true do
    field(:roots, list(binary()), default: [])
    field(:commitments, list(binary()), default: [])
    field(:nullifiers, list(binary()), default: [])
    field(:proofs, list(ProofRecord.t()), default: [])
    field(:delta, Delta.t(), default: Delta.new(%{}))
    field(:extra, list(binary()), default: [])
    field(:preference, term(), default: nil)
  end

  @spec from_noun(Noun.t()) :: {:ok, t()} | :error
  def from_noun([
        roots,
        commitments,
        nullifiers,
        proofs,
        delta,
        extra | _preference
      ]) do
    proofs =
      for proof <- list_nock_to_erlang(proofs) do
        ProofRecord.from_noun(proof)
      end

    if Enum.all?(proofs, &(elem(&1, 0) == :ok)) do
      with {:ok, delta} <- Delta.from_noun(delta),
           proofs = Enum.map(proofs, &elem(&1, 1)) do
        {:ok,
         %Transaction{
           roots: list_nock_to_erlang(roots),
           commitments: list_nock_to_erlang(commitments),
           nullifiers: list_nock_to_erlang(nullifiers),
           proofs: proofs,
           delta: delta,
           extra: list_nock_to_erlang(extra),
           preference: nil
         }}
      end
    else
      :error
    end
  end

  defimpl Nounable, for: __MODULE__ do
    # preference function not yet supported
    def to_noun(transaction = %Transaction{}) do
      {
        transaction.roots,
        transaction.commitments,
        transaction.nullifiers,
        transaction.proofs,
        transaction.delta,
        transaction.extra,
        [[1 | 0], 0 | 0]
      }
      |> Nounable.to_noun()
    end
  end

  defimpl Anoma.RM.Transaction, for: __MODULE__ do
    def commitments(%Transaction{commitments: cm}), do: cm
    def nullifiers(%Transaction{nullifiers: nf}), do: nf

    def storage_commitments(tx),
      do: commitments(tx) |> Enum.map(&Resource.commitment_hash/1)

    def storage_nullifiers(tx),
      do: nullifiers(tx) |> Enum.map(&Resource.nullifier_hash/1)

    def compose(tx1, tx2) do
      # I still don't know if proofs have to be unique...
      unless Anoma.RM.Transaction.Helpers.compose_pre_check(tx1, tx2) do
        nil
      else
        %Transaction{
          roots: tx1.roots ++ tx2.roots,
          commitments: tx1.commitments ++ tx2.commitments,
          nullifiers: tx1.nullifiers ++ tx2.nullifiers,
          proofs: tx1.proofs ++ tx2.proofs,
          delta: Delta.add(tx1.delta, tx2.delta),
          extra: tx1.extra ++ tx2.extra
          # preference
        }
      end
    end

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
        for %{resource: r} <- committed, reduce: Delta.empty() do
          sum ->
            Logger.debug("running committed delta sum: #{inspect(sum)}")
            Delta.add(sum, delta(r))
        end

      Logger.debug("committed delta sum: #{inspect(committed_delta_sum)}")

      nullified_delta_sum =
        for %{resource: r} <- nullified, reduce: Delta.empty() do
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
                {committed,
                 MapSet.put(nullified, %{nullifier: n, resource: r})}

              true ->
                {committed, nullified}
            end
        end

      {MapSet.to_list(committed_set), MapSet.to_list(nullified_set)}
    end

    def cm_tree(_tx, storage) do
      CommitmentTree.new(
        CommitmentTree.Spec.cm_tree_spec(),
        Anoma.Node.Router.Engine.get_state(storage).rm_commitments
      )
    end

    # TODO: add the check for transparent resources
    def resource_existence_check(_transaction, _storage) do
      true
    end
  end
end
