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
      |> Enum.all?(fn %LogicProof{resource: resource} ->
        commitment = resource |> Resource.commitment()
        nullifier = resource |> Resource.nullifier()

        MapSet.member?(action.commitments, commitment) ||
          MapSet.member?(action.nullifiers, nullifier)
      end)
    end
  end
end
