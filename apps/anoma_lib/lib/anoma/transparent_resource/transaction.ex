defmodule Anoma.TransparentResource.Transaction do
  use TypedStruct

  @behaviour Noun.Nounable.Kind

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

  def compose(tx1 = %Transaction{}, tx2 = %Transaction{}) do
    %Transaction{
      roots: MapSet.union(tx1.roots, tx2.roots),
      actions: MapSet.union(tx1.actions, tx2.actions),
      delta: Delta.add(tx1.delta, tx2.delta),
      delta_proof: <<>>
    }
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
  @spec verify_tx_action_compliance(t()) :: boolean()
  def verify_tx_action_compliance(%Transaction{actions: actions}) do
    Enum.all?(actions, &Action.verify_correspondence/1)
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

  # We any here, as it's giving a weird error
  @spec from_noun(Noun.t()) :: {:ok, t()} | :error
  def from_noun([roots, actions, delta, delta_proof | terminator])
      when terminator in [0, <<>>, <<0>>, []] do
    with {:ok, actions} <- from_noun_actions(actions),
         {:ok, delta} <- Delta.from_noun(delta) do
      {:ok,
       %Transaction{
         roots: MapSet.new(Noun.list_nock_to_erlang(roots)),
         actions: actions,
         delta: delta,
         delta_proof: delta_proof
       }}
    end
  end

  def from_noun(_) do
    :error
  end

  defimpl Noun.Nounable, for: Transaction do
    def to_noun(trans = %Transaction{}) do
      [
        MapSet.to_list(trans.roots),
        Enum.map(trans.actions, &Noun.Nounable.to_noun/1),
        Map.to_list(trans.delta) |> Enum.map(fn {x, y} -> [x, y] end),
        # Consider better provinance value
        trans.delta_proof
      ]
    end
  end

  @spec from_noun_actions(Noun.t()) :: {:ok, MapSet.t(Action.t())}
  defp from_noun_actions(noun) when is_list(noun) do
    maybe_actions =
      Enum.map(Noun.list_nock_to_erlang(noun), &Action.from_noun/1)

    if Enum.any?(maybe_actions, &(:error == &1)) do
      :error
    else
      {:ok, MapSet.new(Enum.map(maybe_actions, fn {:ok, x} -> x end))}
    end
  end

  ##############################################################################
  #                                Accessing                                   #
  ##############################################################################

  @spec commitments(t()) ::
          MapSet.t(Anoma.TransparentResource.Resource.commitment())
  def commitments(self = %Transaction{}) do
    self.actions
    |> Stream.map(&Action.commitments/1)
    |> Enum.reduce(MapSet.new(), &MapSet.union/2)
  end

  @spec nullifiers(t()) ::
          MapSet.t(Anoma.TransparentResource.Resource.nullifier())
  def nullifiers(self = %Transaction{}) do
    self.actions
    |> Stream.map(&Action.nullifiers/1)
    |> Enum.reduce(MapSet.new(), &MapSet.union/2)
  end

  @spec resources(t()) :: MapSet.t(Anoma.TransparentResource.Resource.t())
  def resources(self = %Transaction{}) do
    self.actions
    |> Stream.map(&Action.resources/1)
    |> Enum.reduce(MapSet.new(), &MapSet.union/2)
  end

  @spec nullified_resources(t()) ::
          MapSet.t(Anoma.TransparentResource.Resource.t())
  def nullified_resources(self = %Transaction{}) do
    self.actions
    |> Stream.map(&Action.nullified_resources/1)
    |> Enum.reduce(MapSet.new(), &MapSet.union/2)
  end
end
