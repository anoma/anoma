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
  - `create/2`
  - `to_instance/2`
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

  @type appdata :: list({binary(), boolean()})
  @type consume_data ::
          list(
            {<<_::256>>, Resource.t(), <<_::256>>, <<>>, integer(), appdata(),
             binary()}
          )
  @type create_data :: list({Resource.t(), <<_::256>>, appdata(), binary()})

  typedstruct enforce: true do
    # map from tag to flag, logic, appdata, and proof
    field(
      :resource_logic_proofs,
      %{integer() => {boolean(), integer(), appdata(), <<>>}},
      default: %{}
    )

    # list of compliance units
    field(:compliance_units, [ComplianceUnit.t()], default: [])
  end

  @doc """
  I am the creation interface for the transparent action.

  I compute the appropriate compliance unit for the action by going through
  a given list of nullified and created resources, then put the rest into
  appropriate structure slots.
  """
  @spec create(consume_data, create_data) :: t()
  def create(to_nullify, to_commit) do
    consumed =
      Enum.map(
        to_nullify,
        fn {nfkey, resource, _extra, _path, root, _appdata, _appwitness} ->
          {Resource.nullifier_hash(nfkey, resource), root, resource.logicref}
        end
      )

    created =
      to_commit
      |> Enum.map(fn {resource, _extra, _appdata, _appwitness} ->
        {Resource.commitment_hash(resource), resource.logicref}
      end)

    consumed_delta =
      to_nullify
      |> Enum.reduce(2, fn {_, res, _, _, _, _, _}, acc ->
        DeltaHash.delta_add(acc, Resource.delta(res))
      end)

    created_delta =
      to_commit
      |> Enum.reduce(2, fn {res, _, _, _}, acc ->
        DeltaHash.delta_add(acc, Resource.delta(res))
      end)

    cu_instance = %Instance{
      consumed: consumed,
      created: created,
      unit_delta: DeltaHash.delta_sub(consumed_delta, created_delta)
    }

    cus =
      if cu_instance == %Instance{} do
        []
      else
        [ComplianceUnit.create(<<>>, CPS.key(), cu_instance, <<>>)]
      end

    %__MODULE__{
      resource_logic_proofs: generate_proofs(to_nullify, to_commit),
      compliance_units: cus
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
         {:ok, _} <- t |> consumed() |> is_list_unique(),
         # Extra
         {:ok, _} <- t |> created() |> is_list_unique(),
         # Extra
         {:ok, true} <- partition_check(t),
         # 2
         true <- consumed_logic_check(t),
         # 1
         true <- created_logic_check(t) do
      true
    else
      {:error, msg} ->
        Logger.error(msg)
        false

      _ ->
        false
    end
  end

  @doc """
  I am a function generating an instance for a resource logic given its tag
  and appropriate action containing it
  """
  @spec to_instance(Action.t(), integer()) ::
          {:ok, RLPS.Instance.t()} | :error
  def to_instance(t, tag) do
    with {:ok, %{created: created, consumed: consumed}} <- cu_precis(t) do
      case Map.get(t.resource_logic_proofs, tag) do
        {true, _logic, app_data, <<>>} ->
          {:ok,
           %RLPS.Instance{
             tag: tag,
             flag: true,
             consumed: consumed |> Enum.reject(&(&1 == tag)),
             created: created,
             app_data: app_data
           }}

        {false, _logic, app_data, <<>>} ->
          {:ok,
           %RLPS.Instance{
             tag: tag,
             flag: false,
             consumed: consumed,
             created: created |> Enum.reject(&(&1 == tag)),
             app_data: app_data
           }}

        nil ->
          :error
      end
    else
      _ -> :error
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
  @spec app_data(t()) :: [{binary(), bool()}]
  def app_data(t) do
    for {_tag, {_flag, _logic, appdata, <<>>}} <- t.resource_logic_proofs,
        reduce: [] do
      acc -> appdata ++ acc
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
    t
    |> created()
    |> Enum.all?(fn cm ->
      with {:ok, resource} <- RLPS.match_resource(cm, false),
           {:ok, instance} <- to_instance(t, cm) do
        RLPS.verify(
          Noun.atom_integer_to_binary(resource.logicref),
          instance,
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
    t
    |> consumed()
    |> Enum.all?(fn nf ->
      with {:ok, resource} <- RLPS.match_resource(nf, true),
           {:ok, instance} <- to_instance(t, nf) do
        RLPS.verify(
          Noun.atom_integer_to_binary(resource.logicref),
          instance,
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
    with {:ok, %{created: created, consumed: consumed}} <- cu_precis(t),
         true <-
           t |> created() |> MapSet.new() == MapSet.new(created) and
             t |> consumed |> MapSet.new() == MapSet.new(consumed) do
      {:ok, true}
    else
      {:error, msg} -> {:error, msg}
      false -> {:error, "Resources at Action and Compliance level differ"}
    end
  end

  @doc """
  I provide a list of created resources by looking through the logic proofs
  field
  """
  @spec created(t()) :: list(integer())
  def created(action) do
    for {tag, {flag, _, _, _}} <- action.resource_logic_proofs, reduce: [] do
      acc ->
        if flag do
          acc
        else
          [tag | acc]
        end
    end
  end

  @doc """
  I provide a list of consumed resources by looking through the logic proofs
  field
  """
  @spec consumed(t()) :: list(integer())
  def consumed(action) do
    for {tag, {flag, _, _, _}} <- action.resource_logic_proofs, reduce: [] do
      acc ->
        unless flag do
          acc
        else
          [tag | acc]
        end
    end
  end

  @doc """
  I am the precis function for the compliance units.

  I gather all commitments and nullifiers from the appropriate compliance
  units in a provided action. Moreover, I check that they are indeed
  disjoint across, provind an error in the opposite case.
  """
  @spec cu_precis(t()) ::
          {:ok, %{consumed: [], created: []}}
          | {:error, String.t()}
  def cu_precis(t) do
    %{created: created, consumed: consumed} =
      Enum.reduce(t.compliance_units, %{created: [], consumed: []}, fn cu,
                                                                       acc ->
        %{
          created: acc.created ++ ComplianceUnit.created(cu),
          consumed: acc.consumed ++ ComplianceUnit.consumed(cu)
        }
      end)

    created_dup =
      created |> Enum.frequencies() |> Enum.reject(&(elem(&1, 1) == 1))

    consumed_dup =
      consumed |> Enum.frequencies() |> Enum.reject(&(elem(&1, 1) == 1))

    if Enum.empty?(created_dup ++ consumed_dup) do
      {:ok, %{created: created, consumed: consumed}}
    else
      {:error,
       "Not disjoint Compliance Units. Repeated created:\n" <>
         "#{inspect(Enum.map(created_dup, &elem(&1, 0)), pretty: true)}\n" <>
         "Repeated consumed:\n" <>
         "#{inspect(Enum.map(consumed_dup, &elem(&1, 0)), pretty: true)}"}
    end
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

  @spec generate_proofs(consume_data, create_data) :: %{
          integer() => {bool, integer(), appdata(), <<>>}
        }
  defp generate_proofs(to_nullify, to_commit) do
    map =
      for {nlf_key, res, _, _, _, appdata, _} <- to_nullify, reduce: %{} do
        acc ->
          Map.put(
            acc,
            Resource.nullifier_hash(nlf_key, res),
            {true, res.logicref, appdata, <<>>}
          )
      end

    for {res, _extra, appdata, _} <- to_commit, reduce: map do
      acc ->
        Map.put(
          acc,
          Resource.commitment_hash(res),
          {false, res.logicref, appdata, <<>>}
        )
    end
  end

  @spec from_noun(Noun.t()) :: {:ok, t()} | :error
  def from_noun([rl_proofs | cus]) do
    with {:ok, proof_map} <- Noun.Nounable.Map.from_noun(rl_proofs),
         {:ok, cus} <- Noun.Nounable.List.from_noun(cus),
         proof_map_elixir <-
           proof_map
           |> Enum.into(%{}, fn {tag, [bool, logic, appdata | prf]} ->
             {Noun.atom_binary_to_integer(tag),
              {Noun.equal?(0, bool), Noun.atom_binary_to_integer(logic),
               match_appdata(appdata), prf}}
           end),
         cus_list_elixir <-
           cus
           |> Enum.map(fn x ->
             {:ok, cu} = ComplianceUnit.from_noun(x)
             cu
           end) do
      {:ok,
       %__MODULE__{
         resource_logic_proofs: proof_map_elixir,
         compliance_units: cus_list_elixir
       }}
    else
      _ -> :error
    end
  end

  defimpl Noun.Nounable, for: Action do
    @impl true
    def to_noun(t = %Action{}) do
      [
        Noun.Nounable.to_noun(t.resource_logic_proofs)
        | Noun.Nounable.to_noun(t.compliance_units)
      ]
    end
  end

  @doc """
  I match a given nock appdata into an elixir appdata
  """
  @spec match_appdata(Noun.t()) :: appdata()
  def match_appdata(appdata) do
    {:ok, list_data} = Noun.Nounable.List.from_noun(appdata)

    Enum.map(list_data, fn [bin | bool] ->
      {Noun.atom_integer_to_binary(bin), Noun.equal?(bool, 0)}
    end)
  end
end
