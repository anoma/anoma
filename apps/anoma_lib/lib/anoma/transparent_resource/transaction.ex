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
  def verify_tx_action_distinctness(_) do
    true
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
  def verify_tx_action_logics(_) do
    true
  end

  # We any here, as it's giving a weird error
  @spec from_noun(Noun.t()) :: {:ok, t()} | :error | any()
  def from_noun([roots, actions, delta, delta_proof | terminator])
      when terminator in [0, <<>>, <<0>>, []] do
    with {:ok, actions} <- Action.from_noun(actions),
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
end
