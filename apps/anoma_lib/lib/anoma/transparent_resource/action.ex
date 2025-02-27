defmodule Anoma.TransparentResource.Action do
  alias __MODULE__
  alias Anoma.TransparentResource.Delta
  alias Anoma.TransparentResource.LogicProof
  alias Anoma.TransparentResource.Resource

  use TypedStruct

  typedstruct enforce: true do
    field(:commitments, MapSet.t(binary()), default: MapSet.new())
    field(:nullifiers, MapSet.t(binary()), default: MapSet.new())
    field(:proofs, MapSet.t(LogicProof.t()), default: MapSet.new())
    field(:app_data, %{binary() => {any(), bool()}}, default: %{})
  end

  @spec precis(t()) :: %{nullified: Delta.t(), committed: Delta.t()}
  def precis(%Action{proofs: proofs}) do
    for proof <- proofs,
        reduce: %{committed: MapSet.new(), nullified: MapSet.new()} do
      %{committed: committed, nullified: nullified} ->
        case proof.self_tag do
          {:committed, _} ->
            new_committed = MapSet.put(committed, proof.resource)
            %{committed: new_committed, nullified: nullified}

          {:nullified, _} ->
            new_nullified = MapSet.put(nullified, proof.resource)
            %{committed: committed, nullified: new_nullified}
        end
    end
  end

  @spec delta(t()) :: Delta.t()
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

  @spec verify_correspondence(t()) :: true | {:error, String.t()}
  def verify_correspondence(action = %Action{}) do
    # Bail out early, if there are more committed and nullified
    # resources than there are actual resource proofs
    if MapSet.size(action.proofs) <
         MapSet.size(action.commitments) + MapSet.size(action.nullifiers) do
      {:error,
       "there are more commitments and nullifiers than actual logic proofs\n" <>
         "#{inspect(action, pretty: true)}"}
    else
      # TODO Should I check that LogicProof.commitments =
      # Action.commitments, as well as the nullifiers? Or can I assume
      # that they are the same context. I could technically make it
      # lie if I constructed it to lie, no?
      failed_proofs =
        action.proofs
        |> Enum.map(fn proof = %LogicProof{} ->
          cond do
            not LogicProof.verify_resource_corresponds_to_tag(proof) ->
              "Logic Proof failed, the resource's commitment\nullifier:\n" <>
                "#{inspect(proof.resource, pretty: true)}\n" <>
                "does not match the commitment/nullifier: #{inspect(proof.self_tag)}"

            not verify_resource_is_accounted_for?(action, proof) ->
              "The resource:\n" <>
                "#{inspect(proof.resource, pretty: true)}\n" <>
                "Is not found in the Action's commitment/nullifier set"

            not verify_action_resources_correspond_to_proofs?(action, proof) ->
              "Either the action's commitments:\n" <>
                "#{inspect(action.commitments, pretty: true)}\n" <>
                "does not match the proof's commitments:\n" <>
                "#{inspect(proof.commitments, pretty: true)}\n" <>
                "or the action's nullifiers:\n" <>
                "#{inspect(action.nullifiers, pretty: true)}\n" <>
                "does not match the proof's nullifiers:\n" <>
                "#{inspect(proof.nullifiers, pretty: true)}"

            true ->
              true
          end
        end)
        |> Enum.reject(&(&1 == true))

      Enum.empty?(failed_proofs) ||
        {:error,
         "The following correspondence proofs failed:\n" <>
           Enum.join(failed_proofs, "\n")}
    end
  end

  @spec from_noun(Noun.t()) :: {:ok, t()} | :error
  def from_noun([commits, nulls, proofs | app_data]) do
    with {:ok, proofs} <- from_noun_proofs(proofs),
         {:ok, app_data_map} <- Noun.Nounable.Map.from_noun(app_data),
         {:ok, commitments} <- Noun.Nounable.MapSet.from_noun(commits),
         {:ok, nullifiers} <- Noun.Nounable.MapSet.from_noun(nulls) do
      {:ok,
       %Action{
         commitments: commitments,
         nullifiers: nullifiers,
         proofs: proofs,
         app_data:
           Enum.reduce(app_data_map, %{}, fn {key, [val | bool]}, acc ->
             Map.put(
               acc,
               key,
               {val,
                if Noun.equal?(bool, 0) do
                  true
                else
                  false
                end}
             )
           end)
       }}
    end
  end

  def from_noun(_) do
    :error
  end

  defimpl Noun.Nounable, for: Action do
    @impl true
    def to_noun(trans = %Action{}) do
      [
        Noun.Nounable.to_noun(trans.commitments),
        Noun.Nounable.to_noun(trans.nullifiers),
        trans.proofs |> Noun.Nounable.to_noun()
        | Noun.Nounable.to_noun(trans.app_data)
      ]
    end
  end

  @spec from_noun_proofs(Noun.t()) :: {:ok, MapSet.t(LogicProof.t())}
  defp from_noun_proofs(noun) when is_list(noun) do
    {:ok, set} = Noun.Nounable.MapSet.from_noun(noun)

    maybe_proofs =
      Enum.map(set, &proof_from_noun/1)

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

  defp from_noun_proofs(noun) when noun in [0, <<>>, []] do
    {:ok, MapSet.new([])}
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

  ##############################################################################
  #                                Accessing                                   #
  ##############################################################################

  @spec commitments(t()) :: MapSet.t(Resource.commitment())
  def commitments(%Action{commitments: commitments}) do
    commitments
  end

  @spec nullifiers(t()) :: MapSet.t(Resource.nullifier())
  def nullifiers(%Action{nullifiers: nullifiers}) do
    nullifiers
  end

  @spec resources(t()) :: MapSet.t(Resource.t())
  def resources(self = %Action{}) do
    MapSet.new(self.proofs, & &1.resource)
  end

  @spec nullified_resources(t()) :: MapSet.t(Resource.t())
  def nullified_resources(self = %Action{}) do
    self.proofs
    |> Stream.filter(fn
      %LogicProof{self_tag: {:nullified, _}} -> true
      _ -> false
    end)
    |> Stream.map(& &1.resource)
    |> Enum.into(MapSet.new())
  end
end
