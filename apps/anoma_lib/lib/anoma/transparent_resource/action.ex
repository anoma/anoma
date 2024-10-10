defmodule Anoma.TransparentResource.Action do
  use TypedStruct

  alias Anoma.TransparentResource.Delta
  alias Anoma.TransparentResource.LogicProof
  alias Anoma.TransparentResource.Resource
  alias __MODULE__

  typedstruct enforce: true do
    field(:commitments, MapSet.t(binary()), default: [])
    field(:nullifiers, MapSet.t(binary()), default: [])
    field(:proofs, MapSet.t(LogicProof.t()), default: [])
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
end
