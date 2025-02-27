defmodule Anoma.RM.Transparent.Transaction do
  @moduledoc """
  I am the Transaction module for the TRM.

  I provide access to the interfaces needed to interact with the transparent
  transaction structure.

  ### Public API

  I provide the following public functionality

  - `delta/1`
  - `verify/1`
  - `actions_check/1`
  - `disjointness_verification/1`
  - `action_precis/1`
  - `create/2`
  - `compose/2`
  """
  alias Anoma.RM.Transparent.ProvingSystem.DPS
  alias Anoma.RM.Transparent.Primitive.DeltaHash
  alias Anoma.RM.Transparent.Action
  alias __MODULE__

  require Logger
  use TypedStruct

  typedstruct enforce: true do
    # why do we need roots this high up?
    # why not just let them rest in compliance units?
    field(:roots, MapSet.t(integer()), default: MapSet.new())
    field(:actions, MapSet.t(Action.t()), default: MapSet.new())
    field(:delta_proof, <<>>, default: <<>>)
  end

  @doc """
  I am the transparent transaction creation interface.

  Given roots and actions, I create an appropriate transaction.
  """
  @spec create(MapSet.t(integer()), MapSet.t(Action.t())) :: t()
  def create(roots, actions) do
    # specs not updated
    # it has transaction deltas
    %__MODULE__{roots: roots, actions: actions}
  end

  @doc """
  I am the compose interface for transparent transaction.

  I take unions of the provided roots and actions.
  """
  @spec compose(t(), t()) :: t()
  def compose(t1, t2) do
    # interface not updated tx.transactiondelta
    roots = MapSet.union(t1.roots, t2.roots)
    actions = MapSet.union(t1.actions, t2.actions)
    create(roots, actions)
  end

  @doc """
  I am the transparent transaction delta.

  Given a transaction, I simply sum over the contained actions.
  """
  @spec delta(t()) :: integer()
  def delta(t) do
    for action <- t.actions, reduce: 2 do
      acc -> DeltaHash.delta_add(acc, Action.delta(action))
    end
  end

  @doc """
  I am the transparent transaction verification function.

  I provide all the specified checks for the transaction to be considered
  valid.
  """
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
         true <- disjointness_verification(t),
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

  @doc """
  I am the root function for the transparent transaction.

  I collect the roots for non-ephemeral consumed resources that are
  kept in the underlying actions.
  """
  @spec roots(t()) :: MapSet.t(integer())
  def roots(t) do
    for action <- t.actions, reduce: MapSet.new() do
      acc -> Action.roots(action) |> MapSet.union(acc)
    end
  end

  @doc """
  I am a function for getting the commitments out of a transparent
  transaction
  """
  @spec commitments(t()) :: MapSet.t(integer())
  def commitments(t) do
    {:ok, precis} = Transaction.action_precis(t)
    precis.created
  end

  @doc """
  I am a function for getting the nullifiers out of a transparent
  transaction
  """
  @spec nullifiers(t()) :: MapSet.t(integer())
  def nullifiers(t) do
    {:ok, precis} = Transaction.action_precis(t)
    precis.consumed
  end

  @doc """
  I am a function for getting the app data out of a transparent
  transaction.

  I go through the list of actions inside the transaction and gather their
  app data
  """
  @spec app_data(t()) :: MapSet.t({binary(), bool()})
  def app_data(t) do
    for action <- t.actions, reduce: MapSet.new() do
      acc -> Action.app_data(action) |> MapSet.union(acc)
    end
  end

  @doc """
  I am the actions check for the transparent transaction.

  I verify each action in the approprite field using the provided interface.
  """
  @spec actions_check(t()) :: bool()
  def actions_check(t) do
    Enum.all?(t.actions, fn action ->
      Action.verify(action)
    end)
  end

  @doc """
  I am the disjointness verification function for the transparent transaction.

  I check whether all actions are disjoint in terms of created and consumed
  resources.
  """
  @spec disjointness_verification(t()) :: bool()
  def disjointness_verification(t) do
    with {:ok, _} <- action_precis(t) do
      true
    else
      {:error, msg} ->
        Logger.error(msg)
        false
    end
  end

  @doc """
  I am the action precis function for the transparent transaction.

  Given a transaction, I go through the actions contained therein gathering
  all consumed and created resource hashes. If any action has an intersection,
  I error.
  """
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

  @spec from_noun(Noun.t()) :: {:ok, t()} | :error
  def from_noun([roots, actions | delta_proof]) do
    with {:ok, roots} <- Noun.Nounable.MapSet.from_noun(roots),
         {:ok, action_nouns} <- Noun.Nounable.MapSet.from_noun(actions),
         list_actions <- Enum.map(action_nouns, &Action.from_noun/1),
         true <-
           Enum.all?(list_actions, fn
             {:ok, _res} -> true
             :error -> false
           end),
         true <- Noun.equal?(delta_proof, 0) do
      {:ok,
       %__MODULE__{
         roots:
           roots |> Enum.into(MapSet.new(), &Noun.atom_binary_to_integer/1),
         actions:
           list_actions |> Enum.into(MapSet.new(), fn {:ok, res} -> res end)
       }}
    else
      _ -> :error
    end
  end

  defimpl Noun.Nounable, for: Transaction do
    @impl true
    def to_noun(t) do
      [
        Noun.Nounable.to_noun(t.roots),
        Noun.Nounable.to_noun(t.actions) | <<>>
      ]
    end
  end
end

defimpl Anoma.RM.Intent, for: Anoma.RM.Transparent.Transaction do
  alias Anoma.RM.Transparent.Transaction

  @impl true
  def compose(t1 = %Transaction{}, t2 = %Transaction{}) do
    Transaction.compose(t1, t2)
  end

  @impl true
  def verify(tx = %Transaction{}) do
    Transaction.verify(tx)
  end

  @impl true
  def nullifiers(tx = %Transaction{}) do
    Transaction.nullifiers(tx)
  end

  @impl true
  def commitments(tx = %Transaction{}) do
    Transaction.commitments(tx)
  end
end
