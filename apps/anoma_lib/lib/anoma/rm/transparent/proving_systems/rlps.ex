defmodule Anoma.RM.Transparent.ProvingSystem.RLPS.Instance do
  @moduledoc """
  I am the instance module for the resource logic proving system of the TRM.

  My main purpose is to define a proper structure to represent the kind of
  arguments to be fed into a delta unit computation and the appropriate
  API to transform to and from Nockma nouns.

  ### Public API

  I have the following public functionality

  - `from_noun/1`
  - `to_noun/1`
  """
  alias Anoma.RM.Transparent.Resource
  alias __MODULE__
  use TypedStruct

  typedstruct enforce: true do
    # the integer representing binary of commitment or nullifier
    field(:tag, integer(), default: %Resource{} |> Resource.commitment_hash())
    # the flag telling if above is a commitment or nullifier
    # i.e. consumed?
    field(:flag, boolean(), default: false)
    # list of consumed resources in the action
    field(:consumed, list(integer()), default: [])
    # list of created resources in the action
    field(:created, list(integer()), default: [])
    # app data for the tag in appropriate action
    field(:app_data, list({binary(), boolean()}), default: [])
  end

  @spec from_noun(Noun.t()) :: {:ok, t()} | :error
  def from_noun([tag, flag, consumed, created | app_data]) do
    with {:ok, bool} <- Noun.Nounable.Bool.from_noun(flag),
         {:ok, list_consumed} <- Noun.Nounable.List.from_noun(consumed),
         {:ok, list_created} <- Noun.Nounable.List.from_noun(created),
         {:ok, list_app_data} <- Noun.Nounable.List.from_noun(app_data) do
      {:ok,
       %__MODULE__{
         tag: Noun.atom_binary_to_integer(tag),
         flag: bool,
         consumed: list_consumed |> Enum.map(&Noun.atom_binary_to_integer/1),
         created: list_created |> Enum.map(&Noun.atom_binary_to_integer/1),
         app_data:
           list_app_data
           |> Enum.map(fn [data | bool] ->
             {Noun.atom_integer_to_binary(data), Noun.equal?(bool, 0)}
           end)
       }}
    else
      _ -> :error
    end
  end

  defimpl Noun.Nounable, for: Instance do
    @impl true
    def to_noun(instance = %Instance{}) do
      [
        instance.tag,
        Noun.Nounable.Bool.to_noun(instance.flag),
        instance.consumed,
        instance.created
        | Noun.Nounable.to_noun(instance.app_data)
      ]
    end
  end
end

defmodule Anoma.RM.Transparent.ProvingSystem.RLPS do
  @moduledoc """
  I am the resource logic proving system module for the TRM.

  I provide the interface for everything that has to do with the TRM
  resource logic proving.

  ### Public API

  I have the following public functionality

  - `match_resource/2`
  - `prove/3`
  - `verify/3`
  """
  alias Anoma.RM.Transparent.Resource
  alias Anoma.RM.Transparent.ProvingSystem.RLPS.Instance
  alias __MODULE__

  require Logger
  use TypedStruct

  typedstruct enforce: true do
    # pk and vk is the jam of the gate calling RLPS.verify_jet
    # empty for now
    field(:proving_key, binary(), default: [[1 | 0], 0] |> Noun.Jam.jam())
    field(:verifying_key, binary(), default: [[1 | 0], 0] |> Noun.Jam.jam())
    # structured data for running input logics
    # inherently ordered by the key orders in definition
    # assume to be fed into resource logics similarly
    field(:instance, Instance.t(), default: %Instance{})
    # witness and proofs are empty for transparent case
    field(:witness, <<>>, default: <<>>)
    field(:proof, <<>>, default: <<>>)
  end

  @doc """
  I am the TRM resource logic proving system prove interface.

  As we are in the transparent case, this is always trivial.
  """
  @spec prove(<<>>, Instance.t(), <<>>) :: <<>>
  def prove(_, _, _) do
    <<>>
  end

  @doc """
  I am the verification function of the TRM resource logic proving system.

  I accept a jammed predicate as my verifying key. I first check that it can
  be cued, then make sure that the key corresponds to the resource, after which
  I run the jammed logic on the instance.
  """
  @spec verify(binary(), Instance.t(), <<>>) :: boolean()
  def verify(jammed_predicate, instance, <<>>) do
    with {:ok, predicate} <- Noun.Jam.cue(jammed_predicate),
         {:ok, resource} <- match_resource(instance.tag, instance.flag),
         true = Noun.equal?(jammed_predicate, resource.logicref),
         {:ok, res} <-
           Nock.nock(predicate, [
             9,
             2,
             10,
             [6, 1 | Noun.Nounable.to_noun(instance)],
             0 | 1
           ]) do
      Noun.equal?(res, 0)
    else
      {:error, msg} ->
        Logger.error(msg)
        false

      _ ->
        false
    end
  end

  @doc """
  I am a function to matching the resource form the tag provided.

  In particular, given a flag I try to un-match the jammed resource from
  the commitment or nullifier.
  """
  @spec match_resource(integer(), boolean()) ::
          {:ok, Resource.t()} | {:error, String.t()}
  def match_resource(tag, true) do
    with <<"NF_", rest::binary>> <- tag |> Noun.atom_integer_to_binary(),
         {:ok, noun_resource} <- rest |> Noun.Jam.cue(),
         {:ok, resource} <- noun_resource |> Resource.from_noun() do
      {:ok, resource}
    else
      _ -> {:error, "Bad flag for tag\n" <> "#{inspect(tag, pretty: true)}"}
    end
  end

  @spec match_resource(integer(), boolean()) ::
          {:ok, Resource.t()} | {:error, String.t()}
  def match_resource(tag, false) do
    with <<"CM_", rest::binary>> <- tag |> Noun.atom_integer_to_binary(),
         {:ok, noun_resource} <- rest |> Noun.Jam.cue(),
         {:ok, resource} <- noun_resource |> Resource.from_noun() do
      {:ok, resource}
    else
      _ -> {:error, "Bad flag for tag\n" <> "#{inspect(tag, pretty: true)}"}
    end
  end

  @spec from_noun(Noun.t()) :: {:ok, t()} | :error
  def from_noun([pk, vk, instance, witness | proof]) do
    with true <-
           Noun.atom_integer_to_binary(pk) == Noun.atom_integer_to_binary(vk),
         {:ok, instance} <- Instance.from_noun(instance),
         true <- Noun.equal?(witness, 0),
         true <- Noun.equal?(proof, 0) do
      {:ok, %__MODULE__{instance: instance}}
    else
      _ -> :error
    end
  end

  defimpl Noun.Nounable, for: RLPS do
    @impl true
    def to_noun(t = %RLPS{}) do
      [
        t.proving_key,
        t.verifying_key,
        Noun.Nounable.to_noun(t.instance),
        <<>> | <<>>
      ]
    end
  end
end
