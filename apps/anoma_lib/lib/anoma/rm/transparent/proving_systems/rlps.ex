defmodule Anoma.RM.Transparent.ProvingSystem.RLPS.Instance do
  alias Anoma.RM.Transparent.Resource
  alias __MODULE__
  use TypedStruct

  typedstruct enforce: true do
    # the integer representing binary of commitment or nullifier
    field(:tag, integer(), default: %Resource{} |> Resource.commitment_hash())
    # the flag telling if above is a commitment or nullifier
    # i.e. consumed?
    field(:flag, bool(), default: false)
    # list of consumed resources in the action
    field(:consumed, list(integer()), default: [])
    # list of created resources in the action
    field(:created, list(integer()), default: [])
    # app data for the tag in appropriate action
    field(:appdata, list({binary(), bool()}), default: [])
  end

  @spec from_noun(Noun.t()) :: t()
  def from_noun([_tag, _flag, _consumed, _created | _app_data]) do
    %__MODULE__{}
  end

  @spec to_noun(t()) :: 0
  def to_noun(_instance) do
    0
  end
end

defmodule Anoma.RM.Transparent.ProvingSystem.RLPS do
  alias Anoma.RM.Transparent.Resource
  alias Anoma.RM.Transparent.ProvingSystem.RLPS.Instance

  require Logger
  use TypedStruct

  @type rlps_key :: <<>>
  @rlps_key <<>>

  typedstruct enforce: true do
    # pk and vk is the jam of the gate calling RLPS.verify_jet
    # empty for now
    field(:proving_key, rlps_key, default: @rlps_key)
    field(:verifying_key, rlps_key, default: @rlps_key)
    # structured data for running input logics
    # inherently ordered by the key orders in definition
    # assume to be fed into resource logics similarly
    field(:instance, Instance.t(), default: %Instance{})
    # witness and proofs are empty for transparent case
    field(:witness, <<>>, default: <<>>)
    field(:proof, <<>>, default: <<>>)
  end

  @spec key() :: binary()
  def key() do
    @rlps_key
  end

  @spec prove(<<>>, Instance.t(), <<>>) :: <<>>
  def prove(_, _, _) do
    <<>>
  end

  @spec verify(rlps_key, Instance.t(), <<>>) :: bool()
  def verify(jammed_predicate, instance, <<>>) do
    with {:ok, predicate} <- Noun.Jam.cue(jammed_predicate),
         {:ok, res} <-
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
          integer(),
          bool(),
          list(integer()),
          list(integer()),
          list({binary(), bool()})
        ) :: {:ok, bool()} | {:error, String.t()}
  def verify_jet(tag, flag, consumed, created, appdata) do
    with {:ok, resource} <- match_resource(tag, flag),
         {:ok, {logic, rest}} <- make_data(appdata),
         true <- resource.logicref == :crypto.hash(:sha256, logic),
         {:ok, predicate} <- Noun.Jam.cue(logic) do
      Nock.nock(predicate, [
        9,
        2,
        10,
        [6, 1 | [flag, consumed, created, [logic | rest]]]
      ])
    else
      {:error, string} ->
        Logger.error(string)
        false

      _ ->
        Logger.error("Logic verification failed")
    end
  end

  @spec match_resource(integer(), bool()) ::
          {:ok, Resource.t()} | {:error, String.t()}
  def match_resource(hash, tag) do
    string =
      if tag do
        "NF_"
      else
        "CM_"
      end

    with <<^string, rest::bitstring>> <- hash do
      {:ok, rest}
    else
      _ -> {:error, "Bad tag for hash\n" <> "#{inspect(hash, pretty: true)}"}
    end
  end

  @spec make_data(list({binary(), bool})) ::
          {:ok, {binary(), list(binary())}} | {:error, String.t()}
  defp make_data(appdata) do
    list = Enum.map(appdata, &elem(&1, 0))

    try do
      hd(list)
    rescue
      _ ->
        {:error,
         "No logic present in appdata\n" <>
           "#{inspect(appdata, pretty: true)}"}
    else
      res -> {:ok, {res, tl(list)}}
    end
  end
end
