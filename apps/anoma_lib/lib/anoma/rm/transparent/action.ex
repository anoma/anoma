defmodule Anoma.RM.Transparent.Action do
  alias Anoma.RM.Transparent.Resource
  alias Anoma.RM.Transparent.ComplianceUnit
  alias Anoma.RM.Transparent.ProvingSystem.CPS
  alias Anoma.RM.Transparent.ProvingSystem.RLPS
  alias Anoma.RM.Transparent.Primitive.DeltaHash
  alias Anoma.RM.Transparent.ProvingSystem.CPS.Instance

  require Logger
  use TypedStruct

  typedstruct enforce: true do
    field(:created, list(integer()), default: [])
    field(:consumed, list(integer()), default: [])

    field(:resource_logic_proofs, %{binary() => {<<_::256>>, <<>>}},
      default: %{}
    )

    field(:compliance_units, MapSet.t(ComplianceUnit.t()),
      default: MapSet.new([])
    )

    field(:app_data, %{binary() => {binary(), bool()}}, default: %{})
  end

  @doc """
  I create a trivial transaction with a single compliance unit.
  """
  # use lists here if order really matters (?)
  @spec create(
          list({<<_::256>>, Resource.t(), integer()}),
          list(Resource.t()),
          list({binary(), bool()})
        ) :: t()
  def create(to_nullify, to_commit, app_data) do
    consumed =
      Enum.map(
        to_nullify,
        fn {nfkey, resource, acc} ->
          {Resource.nullifier_hash(nfkey, resource),
           Noun.Jam.cue(acc)
           |> Noun.atom_binary_to_integer(), resource.logic}
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
      compliance_units: cu,
      app_data: app_data
    }
  end

  @spec delta(t()) :: integer()
  def delta(t) do
    for cu <- t.compliance_units, reduce: 2 do
      acc -> DeltaHash.delta_add(acc, ComplianceUnit.delta(cu))
    end
  end

  @spec verify(t()) :: bool()
  def verify(t) do
    vk = RLPS.key()

    # 3
    with true <- cu_check(t),
         # Extra
         {:ok, _} <- is_list_unique(t.consumed),
         # Extra
         {:ok, _} <- is_list_unique(t.created),
         # Extra
         {:ok, true} <- partition_check(t),
         # 2
         true <- consumed_logic_check(t, vk),
         # 1
         true <- created_logic_check(t, vk) do
      true
    else
      {:error, msg} -> Logger.error(msg)
      _ -> false
    end
  end

  @spec cu_check(t()) :: bool()
  def cu_check(t) do
    Enum.all?(t.compliance_units, fn cu -> ComplianceUnit.verify(cu) end)
  end

  @spec created_logic_check(t(), RLPS.key()) :: bool()
  def created_logic_check(t, vk) do
    Enum.all?(t.created, fn cm ->
      RLPS.verify(
        vk,
        %RLPS.Instance{
          tag: cm,
          flag: false,
          consumed: t.consumed,
          created: t.created,
          appdata: Map.get(t.app_data, cm)
        },
        <<>>
      )
    end)
  end

  @spec consumed_logic_check(t(), RLPS.key()) :: bool()
  def consumed_logic_check(t, vk) do
    Enum.all?(t.consumed, fn nf ->
      RLPS.verify(
        vk,
        %RLPS.Instance{
          tag: nf,
          flag: true,
          consumed: t.consumed,
          created: t.created,
          appdata: Map.get(t.app_data, nf)
        },
        <<>>
      )
    end)
  end

  @spec partition_check(t()) :: {:ok, bool()} | {:error, String.t()}
  def partition_check(t) do
    with {:ok, %{created: created, consumed: consumed}} <- cu_precis(t) do
      {:ok,
       MapSet.new(t.created) == created and MapSet.new(t.consumed) == consumed}
    else
      {:error, msg} -> {:error, msg}
    end
  end

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
      if Enum.any?(acc, fn x -> x == elem end) do
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
end
