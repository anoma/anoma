defmodule Anoma.TransparentResource.Action do
  use TypedStruct

  alias Anoma.TransparentResource.Delta
  alias Anoma.TransparentResource.LogicProof
  alias Anoma.TransparentResource.Resource
  alias __MODULE__

  typedstruct enforce: true do
    field(:commitments, MapSet.t(binary()), default: MapSet.new())
    field(:nullifiers, MapSet.t(binary()), default: MapSet.new())
    field(:proofs, MapSet.t(LogicProof.t()), default: MapSet.new())
    field(:app_data, binary(), default: <<>>)
  end

  def precis(%Action{proofs: proofs}) do
    for proof <- proofs,
        reduce: %{commited: MapSet.new(), nullified: MapSet.new()} do
      %{committed: committed, nullified: nullified} ->
        case proof.polarity do
          :committed ->
            new_committed = MapSet.put(committed, proof.resource)
            %{committed: new_committed, nullified: nullified}

          :nullified ->
            new_nullified = MapSet.put(nullified, proof.resource)
            %{committed: committed, nullified: new_nullified}
        end
    end
  end

  def delta(action = %Action{}) do
    %{committed: committed, nullified: nullified} = precis(action)

    committed_delta =
      for resource <- committed, reduce: %{} do
        acc -> Delta.add(acc, Resource.delta(resource))
      end

    nullified_delta =
      for resource <- nullified, reduce: %{} do
        acc -> Delta.add(acc, Resource.delta(resource))
      end

    Delta.sub(committed_delta, nullified_delta)
  end

  @spec verify_correspondence(t()) :: boolean()
  def verify_correspondence(action = %Action{}) do
    # Bail out early, if there are more committed and nullified
    # resources than there are actual resource proofs
    if MapSet.size(action.proofs) <
         MapSet.size(action.commitments) + MapSet.size(action.nullifiers) do
      false
    else
      # TODO Should I check that LogicProof.commitments =
      # Action.commitments, as well as the nullifiers? Or can I assume
      # that they are the same context. I could technically make it
      # lie if I constructed it to lie, no?
      action.proofs
      |> Enum.all?(fn
        proof = %LogicProof{} ->
          LogicProof.verify_resource_corresponds_to_tag(proof) &&
            verify_resource_is_accounted_for?(action, proof) and
            verify_action_resources_correspond_to_proofs?(action, proof)
      end)
    end
  end

  @spec from_noun(Noun.t()) :: {:ok, t()} | :error
  def from_noun([commits, nulls, proofs, app_data | terminator])
      when terminator in [0, <<>>, <<0>>, []] do
    with {:ok, proofs} <- from_noun_proofs(proofs) do
      {:ok,
       %Action{
         commitments: MapSet.new(Noun.list_nock_to_erlang(commits)),
         nullifiers: MapSet.new(Noun.list_nock_to_erlang(nulls)),
         proofs: proofs,
         app_data: app_data
       }}
    end
  end

  def from_noun(_) do
    :error
  end

  defimpl Noun.Nounable, for: Action do
    def to_noun(trans = %Action{}) do
      [
        MapSet.to_list(trans.commitments),
        MapSet.to_list(trans.nullifiers),
        Enum.map(trans.proofs, &Noun.Nounable.to_noun/1),
        trans.app_data
      ]
    end
  end

  @spec from_noun_proofs(Noun.t()) :: {:ok, MapSet.t(LogicProof.t())}
  defp from_noun_proofs(noun) when is_list(noun) do
    maybe_proofs =
      Enum.map(Noun.list_nock_to_erlang(noun), &proof_from_noun/1)

    if Enum.any?(maybe_proofs, &(:error == &1)) do
      :error
    else
      # We handle compliance proofs by simply dropping them
      return_set =
        Enum.map(maybe_proofs, fn {:ok, x} -> x end)
        |> Enum.reject(&is_binary/1)

      {:ok, MapSet.new(return_set)}
    end
  end

  @spec proof_from_noun(Noun.t()) :: {:ok, LogicProof.t() | binary()} | :error
  defp proof_from_noun(noun) when is_integer(noun) do
    proof_from_noun(Noun.atom_integer_to_binary(noun))
  end

  defp proof_from_noun(noun) when is_binary(noun) do
    if noun == "compliance" do
      {:ok, "compliance"}
    else
      :error
    end
  end

  defp proof_from_noun(noun) do
    LogicProof.from_noun(noun)
  end

  # Check that the resource is in the set
  @spec verify_resource_is_accounted_for?(t(), LogicProof.t()) :: boolean()
  defp verify_resource_is_accounted_for?(self = %Action{}, %LogicProof{
         self_tag: {:committed, commitment}
       }) do
    MapSet.member?(self.commitments, commitment)
  end

  defp verify_resource_is_accounted_for?(self = %Action{}, %LogicProof{
         self_tag: {:nullified, nullifier}
       }) do
    MapSet.member?(self.nullifiers, nullifier)
  end

  @spec verify_action_resources_correspond_to_proofs?(t(), LogicProof.t()) ::
          boolean()
  defp verify_action_resources_correspond_to_proofs?(
         %Action{commitments: action_commits, nullifiers: action_nulls},
         %LogicProof{commitments: commitments, nullifiers: nullifiers}
       ) do
    action_commits == commitments && action_nulls == nullifiers
  end
end
