defmodule Anoma.RM.Transparent.ProvingSystem.CPS.Instance do
  alias __MODULE__

  use TypedStruct

  typedstruct enforce: true do
    # lits of nullifiers with roots and logics
    field(:consumed, list({integer(), integer(), integer()}), default: <<>>)
    # list of commitments with logics
    field(:created, list({integer(), integer()}), default: <<>>)
    # computed unit delta hash
    field(:unit_delta, integer(), default: 2)
  end

  @spec from_noun(Noun.t()) :: t()
  def from_noun([_consumed, _created | _unit_delta]) do
    %__MODULE__{}
  end

  @spec to_noun(t()) :: Noun.t()
  def to_noun(_instance) do
    0
  end
end

defmodule Anoma.RM.Transparent.ProvingSystem.CPS do
  alias Anoma.RM.Transparent.Primitive.DeltaHash
  alias Anoma.RM.Transparent.Primitive.CommitmentAccumilator
  alias Anoma.RM.Transparent.Resource
  alias Anoma.RM.Transparent.ProvingSystem.CPS.Instance

  require Logger
  use TypedStruct

  @type cps_key :: <<>>
  @cps_key <<>>

  typedstruct enforce: true do
    # pk and vk is the jam of the gate calling CPS.verify_jet
    # empty for now
    field(:proving_key, cps_key, default: @cps_key)
    field(:verifying_key, cps_key, default: @cps_key)
    # structured data for running delta checks
    # inherently ordered by the key orders in definition
    field(:instance, Instance.t(), default: %Instance{})
    # witness and proofs are empty for transparent case
    field(:witness, <<>>, default: <<>>)
    field(:proof, <<>>, default: <<>>)
  end

  @spec key() :: binary()
  def key() do
    @cps_key
  end

  @spec prove(<<>>, Instance.t(), <<>>) :: <<>>
  def prove(_, _, _) do
    <<>>
  end

  @spec verify(<<>>, Instance.t(), <<>>) :: bool()
  def verify(jammed_predicate, instance, <<>>) do
    {:ok, predicate} = Noun.Jam.cue(jammed_predicate)

    with {:ok, res} <-
           Nock.nock(predicate, [
             9,
             2,
             10,
             [6, 1 | Instance.to_noun(instance)]
           ]) do
      Noun.equal?(res, 0)
    else
      _ -> false
    end
  end

  @spec verify_jet(
          list({integer(), integer(), integer()}),
          list({integer(), integer()}),
          integer()
        ) :: bool()
  def verify_jet(consumed, created, unitdelta) do
    consumed_resources_w_roots =
      consumed
      |> Enum.map(fn {nul, rt, _} ->
        {nul, rt}
      end)

    with {:ok, consumed} <-
           verify_nulfs_compliance(consumed_resources_w_roots),
         {:ok, created} <- verify_cms_compliance(created),
         true <- verify_delta(consumed, created, unitdelta) do
      true
    else
      false ->
        Logger.error("Delta verification failed for compliance unit")
        false

      {:eror, message} ->
        Logger.error(message)
        false
    end
  end

  @spec verify_delta(
          MapSet.t(Resource.t()),
          MapSet.t(Resource.t()),
          integer()
        ) :: bool()
  def verify_delta(consumed, created, unitdelta) do
    consumed_delta = Enum.reduce(consumed, 2, &DeltaHash.delta_add/2)
    created_delta = Enum.reduce(created, 2, &DeltaHash.delta_add/2)

    unitdelta == DeltaHash.delta_sub(consumed_delta, created_delta)
  end

  @spec verify_nulfs_compliance(list({integer(), integer()})) ::
          {:ok, MapSet.t(Resource.t())} | {:error, String.t()}
  def verify_nulfs_compliance(consumed) do
    Enum.reduce_while(consumed, MapSet.new(), fn {nul, rt}, {:ok, acc} ->
      with <<"NF_", rest::bitstring>> <- nul,
           true <- check_nullified_has_been_created(rest, rt) do
        {:cont, {:ok, MapSet.put(acc, rest)}}
      else
        {:error, log} ->
          {:halt, {:error, log}}

        _ ->
          {:halt,
           {:error,
            "Nullifier\n" <>
              "#{inspect(nul, pretty: true)}\n" <> "is noncompliant"}}
      end
    end)
  end

  @spec verify_cms_compliance(list({integer(), integer()})) ::
          {:ok, MapSet.t(Resource.t())} | {:error, String.t()}
  def verify_cms_compliance(consumed) do
    Enum.reduce_while(consumed, true, fn {cm, _}, {:ok, acc} ->
      with <<"CM_", rest::bitstring>> <- cm do
        {:cont, {:ok, MapSet.put(acc, rest)}}
      else
        _ ->
          {:halt,
           {:error,
            "Commitment\n" <>
              "#{inspect(cm, pretty: true)}\n" <> "is noncompliant"}}
      end
    end)
  end

  @spec check_nullified_has_been_created(Resource.t(), integer()) ::
          true | {:error, String.t()}
  def check_nullified_has_been_created(res, rt) do
    unless res.isephemeral do
      cm = res |> Resource.commitment_hash()

      with {:ok, acc} <-
             rt |> Noun.atom_integer_to_binary() |> Noun.Jam.cue(),
           true <- CommitmentAccumilator.verify(cm, acc, rt) do
        true
      else
        false ->
          {:error,
           "Resource commitment\n" <>
             "#{inspect(cm, pretty: true)}\n" <>
             "is not found at root" <>
             "#{inspect(rt, pretty: true)}" <>
             "during nullifier compliance check"}

        _ ->
          {:error,
           "Failed to cue root" <>
             "#{inspect(rt, pretty: true)}" <>
             "during nullifier compliance proof check"}
      end
    else
      true
    end
  end
end
