defmodule Anoma.RM.Transparent.Action do
  @doc """
  I am the Action module of the TRM.

  I provide the interfaces for handling transparent actions.

  ### Public API

  I provide the following public functionality

  - `delta/1`
  - `verify/1`
  - `cu_check/1`
  - `cu_precis/1`
  - `partition_check/1`
  - `created_logic_check/2`
  - `consumed_logic_check/2`
  - `create/3`
  """
  alias Anoma.RM.Transparent.Resource
  alias Anoma.RM.Transparent.ComplianceUnit
  alias Anoma.RM.Transparent.ProvingSystem.CPS
  alias Anoma.RM.Transparent.ProvingSystem.RLPS
  alias Anoma.RM.Transparent.Primitive.DeltaHash
  alias Anoma.RM.Transparent.ProvingSystem.CPS.Instance
  alias __MODULE__

  require Logger
  use TypedStruct

  typedstruct enforce: true do
    field(:created, list(integer()), default: [])
    field(:consumed, list(integer()), default: [])

    field(:resource_logic_proofs, %{integer() => {integer(), <<>>}},
      default: %{}
    )

    field(:compliance_units, MapSet.t(ComplianceUnit.t()),
      default: MapSet.new([])
    )

    field(:app_data, %{integer() => list({binary(), boolean()})},
      default: %{}
    )
  end

  @doc """
  I am the creation interface for the transparent action.

  I compute the appropriate compliance unit for the action by going through
  a given list of nullified and created resources, then put the rest into
  appropriate structure slots.
  """
  # use lists here if order really matters (?)
  @spec create(
          list({<<_::256>>, Resource.t(), integer()}),
          list(Resource.t()),
          %{integer() => list({binary(), boolean()})}
        ) :: t()
  def create(to_nullify, to_commit, app_data) do
    consumed =
      Enum.map(
        to_nullify,
        fn {nfkey, resource, acc} ->
          {Resource.nullifier_hash(nfkey, resource), acc, resource.logicref}
        end
      )

    created =
      to_commit
      |> Enum.map(fn resource ->
        {Resource.commitment_hash(resource), resource.logicref}
      end)

    consumed_delta =
      to_nullify
      |> Enum.reduce(2, fn {_, res, _}, acc ->
        DeltaHash.delta_add(acc, Resource.delta(res))
      end)

    created_delta =
      to_commit
      |> Enum.reduce(2, fn res, acc ->
        DeltaHash.delta_add(acc, Resource.delta(res))
      end)

    cu_instance = %Instance{
      consumed: consumed,
      created: created,
      unit_delta: DeltaHash.delta_sub(consumed_delta, created_delta)
    }

    cu = ComplianceUnit.create(CPS.key(), cu_instance, <<>>)

    %__MODULE__{
      created: created |> Enum.map(&elem(&1, 0)),
      consumed: consumed |> Enum.map(&elem(&1, 0)),
      resource_logic_proofs: generate_proofs(to_nullify, to_commit),
      compliance_units: MapSet.new([cu]),
      app_data: app_data
    }
  end

  @doc """
  I am the delta computation function for the transparent action.

  I simply sum up the unit deltas across the compliance units.
  """
  @spec delta(t()) :: integer()
  def delta(t) do
    for cu <- t.compliance_units, reduce: 2 do
      acc -> DeltaHash.delta_add(acc, ComplianceUnit.delta(cu))
    end
  end

  @doc """
  I am the action verification function.

  I first check that there are no repeated resources across nullified and
  committed hashes. Then check that the compliance units actually partition
  the action. Finally, I run logic checks for each created and consumed
  resource.
  """
  @spec verify(t()) :: boolean()
  def verify(t) do
    # 3
    with true <- cu_check(t),
         # Extra
         {:ok, _} <- is_list_unique(t.consumed),
         # Extra
         {:ok, _} <- is_list_unique(t.created),
         # Extra
         {:ok, true} <- partition_check(t),
         # 2
         true <- consumed_logic_check(t),
         # 1
         true <- created_logic_check(t) do
      true
    else
      {:error, msg} -> Logger.error(msg)
      _ -> false
    end
  end

  @doc """
  I am the root function for the transparent action.

  I go through the roots in the compliance units that are used for non
  ephemetal resources and collect them in a set
  """
  @spec roots(t()) :: MapSet.t(integer())
  def roots(t) do
    for cu <- t.compliance_units, reduce: MapSet.new() do
      acc -> ComplianceUnit.roots(cu) |> MapSet.union(acc)
    end
  end

  @doc """
  I am the app data function.

  I gather all the app data used up in a transparent transaction.
  """
  @spec app_data(t()) :: MapSet.t({binary(), bool()})
  def app_data(t) do
    for {_key, value} <- t.app_data, reduce: MapSet.new() do
      acc -> MapSet.put(acc, value)
    end
  end

  @doc """
  I am the check for compliance units in a transparent action.

  I simply go through the compliance units and verify each one using the
  appropriate interface.
  """
  @spec cu_check(t()) :: boolean()
  def cu_check(t) do
    Enum.all?(t.compliance_units, fn cu -> ComplianceUnit.verify(cu) end)
  end

  @doc """
  I am the function to check created resource logics.

  I go through the created resources and verify them using the proving
  system interface.
  """
  @spec created_logic_check(t()) :: bool()
  def created_logic_check(t) do
    Enum.all?(t.created, fn cm ->
      with {:ok, resource} <- RLPS.match_resource(cm, false) do
        RLPS.verify(
          Noun.atom_integer_to_binary(resource.logicref),
          %RLPS.Instance{
            tag: cm,
            flag: false,
            consumed: t.consumed,
            created: t.created,
            app_data: Map.get(t.app_data, cm)
          },
          <<>>
        )
      else
        _ -> false
      end
    end)
  end

  @doc """
  I am the function to check consumed resource logics.

  I go through the consumed resources and verify them using the proving
  system interface.
  """
  @spec consumed_logic_check(t()) :: boolean()
  def consumed_logic_check(t) do
    Enum.all?(t.consumed, fn nf ->
      with {:ok, resource} <- RLPS.match_resource(nf, true) do
        RLPS.verify(
          Noun.atom_integer_to_binary(resource.logicref),
          %RLPS.Instance{
            tag: nf,
            flag: true,
            consumed: t.consumed,
            created: t.created,
            app_data: Map.get(t.app_data, nf)
          },
          <<>>
        )
      else
        _ -> false
      end
    end)
  end

  @doc """
  I am the partition check for a transparent action.

  Given an action, I check that the compliance units fully cover it.
  """
  @spec partition_check(t()) :: {:ok, bool()} | {:error, String.t()}
  def partition_check(t) do
    with {:ok, %{created: created, consumed: consumed}} <- cu_precis(t) do
      {:ok,
       MapSet.new(t.created) == created and MapSet.new(t.consumed) == consumed}
    else
      {:error, msg} -> {:error, msg}
    end
  end

  @doc """
  I am the precis function for the compliance units.

  I gather all commitments and nullifiers from the appropriate compliance
  units in a provided action. Moreover, I check that they are indeed
  disjoint across, provind an error in the opposite case.
  """
  @spec cu_precis(t()) ::
          {:ok, %{consumed: MapSet.t(), created: MapSet.t()}}
          | {:error, String.t()}
  def cu_precis(t) do
    Enum.reduce_while(
      t.compliance_units,
      {:ok, %{created: MapSet.new(), consumed: MapSet.new()}},
      fn cu, {:ok, acc} ->
        created = ComplianceUnit.created(cu)
        consumed = ComplianceUnit.consumed(cu)

        if (MapSet.size(acc.created) == 0 or
              MapSet.disjoint?(acc.created, created)) and
             (MapSet.size(acc.consumed) == 0 or
                MapSet.disjoint?(acc.consumed, consumed)) do
          {:cont,
           {:ok,
            %{
              created: MapSet.union(acc.created, created),
              consumed: MapSet.union(acc.consumed, consumed)
            }}}
        else
          {:halt,
           {:error,
            "Not disjoint Compliance Units. Repeated created:\n" <>
              "#{inspect(MapSet.intersection(created, acc.created), pretty: true)}\n" <>
              "Repeated consumed:\n" <>
              "#{inspect(MapSet.intersection(consumed, acc.consumed))}"}}
        end
      end
    )
  end

  @spec is_list_unique(list()) :: {:ok, list()} | {:error, String.t()}
  defp is_list_unique(list) do
    Enum.reduce_while(list, {:ok, []}, fn elem, {:ok, acc} ->
      unless Enum.any?(acc, fn x -> x == elem end) do
        {:cont, {:ok, [elem | acc]}}
      else
        {:halt,
         {:error,
          "Element\n" <>
            "#{inspect(elem, pretty: true)}\n" <> "repeats in an action"}}
      end
    end)
  end

  @spec generate_proofs(
          list({<<_::256>>, Resource.t(), integer()}),
          list(Resource.t())
        ) :: %{integer() => {<<_::256>>, <<>>}}
  defp generate_proofs(to_nullify, to_commit) do
    map =
      for {nlf_key, res, _} <- to_nullify, reduce: %{} do
        acc ->
          Map.put(
            acc,
            Resource.nullifier_hash(nlf_key, res),
            {res.logicref, <<>>}
          )
      end

    for res <- to_commit, reduce: map do
      acc -> Map.put(acc, Resource.commitment_hash(res), {res.logicref, <<>>})
    end
  end

  @spec from_noun(Noun.t()) :: {:ok, t()} | :error
  def from_noun([created, consumed, rl_proofs, cus | app_data]) do
    with {:ok, list_created} <- Noun.Nounable.List.from_noun(created),
         {:ok, list_consumed} <- Noun.Nounable.List.from_noun(consumed),
         {:ok, proof_map} <- Noun.Nounable.Map.from_noun(rl_proofs),
         {:ok, cu_map} <- Noun.Nounable.MapSet.from_noun(cus),
         {:ok, appdata} <- Noun.Nounable.Map.from_noun(app_data) do
      {:ok,
       %__MODULE__{
         created: list_created |> Enum.map(&Noun.atom_binary_to_integer/1),
         consumed: list_consumed |> Enum.map(&Noun.atom_binary_to_integer/1),
         resource_logic_proofs:
           proof_map
           |> Enum.into(%{}, fn {tag, [bin | proof]} ->
             {Noun.atom_binary_to_integer(tag),
              {Noun.atom_binary_to_integer(bin), proof}}
           end),
         compliance_units:
           cu_map
           |> Enum.into(MapSet.new(), fn x ->
             {:ok, cu} = ComplianceUnit.from_noun(x)
             cu
           end),
         app_data:
           appdata
           |> Enum.into(%{}, fn {tag, [bin | bool]} ->
             {Noun.atom_binary_to_integer(tag),
              {Noun.atom_integer_to_binary(bin), Noun.equal?(bool, 0)}}
           end)
       }}
    else
      _ -> :error
    end
  end

  defimpl Noun.Nounable, for: Action do
    @impl true
    def to_noun(t = %Action{}) do
      [
        Noun.Nounable.to_noun(t.created),
        Noun.Nounable.to_noun(t.consumed),
        Noun.Nounable.to_noun(t.resource_logic_proofs),
        Noun.Nounable.to_noun(t.compliance_units)
        | Noun.Nounable.to_noun(t.app_data)
      ]
    end
  end
end
