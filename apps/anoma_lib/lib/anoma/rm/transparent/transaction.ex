defmodule Anoma.RM.Transparent.Transaction do
  alias Anoma.RM.Transparent.ProvingSystem.DPS
  alias Anoma.RM.Transparent.Primitive.DeltaHash
  alias Anoma.RM.Transparent.Action

  require Logger
  use TypedStruct

  typedstruct enforce: true do
    # why do we need roots this high up?
    # why not just let them rest in compliance units?
    field(:roots, MapSet.t(integer()), default: MapSet.new())
    field(:actions, MapSet.t(Action.t()), default: MapSet.new())
    field(:delta_proof, <<>>, default: <<>>)
  end

  @spec create(MapSet.t(integer()), MapSet.t(Action.t())) :: t()
  def create(roots, actions) do
    # specs not updated
    # it has transaction deltas
    %__MODULE__{roots: roots, actions: actions}
  end

  @spec compose(t(), t()) :: t()
  def compose(t1, t2) do
    # interface not updated tx.transactiondelta
    roots = MapSet.union(t1.roots, t2.roots)
    actions = MapSet.union(t1.actions, t2.actions)
    create(roots, actions)
  end

  @spec delta(t()) :: integer()
  def delta(t) do
    for action <- t.actions, reduce: 2 do
      acc -> DeltaHash.delta_add(acc, Action.delta(action))
    end
  end

  @spec verify(t()) :: bool()
  def verify(t) do
    # do we need to check that a transaction is balanced elsewhere?
    # probably? unless there is a predetermined 0 balance for each PS

    # checks to be done at the Node level:
    # 1. Roots existed at some point
    # 2. Created resources weren't created priot
    # 3. Consumed resources weren't consumed prior
    # 3 delta_verification
    with true <- actions_check(t),
         # 1 valid actions
         true <- partition_verification(t),
         # 2 partition of state
         true <-
           DPS.verify(
             DPS.key(),
             %DPS.Instance{delta: delta(t)},
             t.delta_proof
           ) do
      true
    else
      _ -> false
    end
  end

  @spec actions_check(t()) :: bool()
  def actions_check(t) do
    Enum.all?(t.actions, fn action ->
      Action.verify(action)
    end)
  end

  @spec partition_verification(t()) :: bool()
  def partition_verification(t) do
    with {:ok, _} <- action_precis(t) do
      true
    else
      {:error, msg} ->
        Logger.error(msg)
        false
    end
  end

  @spec action_precis(t()) ::
          {:ok,
           %{created: MapSet.t(integer()), consumed: MapSet.t(integer())}}
          | {:error, String.t()}
  def action_precis(t) do
    Enum.reduce_while(
      t.actions,
      {:ok, %{created: MapSet.new(), consumed: MapSet.new()}},
      fn action, {:ok, acc} ->
        created = action.created |> MapSet.new()
        consumed = action.consumed |> MapSet.new()

        if MapSet.disjoint?(acc.created, created) and
             MapSet.disjoint?(acc.consumed, consumed) do
          {:cont,
           {:ok,
            %{
              created: MapSet.union(acc.created, created),
              consumed: MapSet.union(acc.consumed, consumed)
            }}}
        else
          {:halt,
           {:error,
            "Not disjoint actions. Repeating commitments:\n" <>
              "#{inspect(MapSet.intersection(acc.created, created), pretty: true)}\n" <>
              "Repeating nullifiers:\n" <>
              "#{inspect(MapSet.intersection(acc.consumed, consumed), pretty: true)}"}}
        end
      end
    )
  end
end
