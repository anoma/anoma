defmodule Anoma.RM.Transparent.ProvingSystem.CPS.Instance do
  @moduledoc """
  I am the instance module for the compliance proving system of the TRM.

  My main purpose is to define a proper structure to represent the kind of
  arguments to be fed into a compliance unit computation and the appropriate
  API to transform to and from Nockma nouns.

  ### Public API

  I have the following public functionality

  - `from_noun/1`
  - `to_noun/1`
  """
  alias __MODULE__

  use TypedStruct

  typedstruct enforce: true do
    # lits of nullifiers with roots and logics
    field(:consumed, list({integer(), integer(), integer()}), default: [])
    # list of commitments with logics
    field(:created, list({integer(), integer()}), default: [])
    # computed unit delta hash
    field(:unit_delta, integer(), default: 2)
  end

  @spec from_noun(Noun.t()) :: {:ok, t()} | :error
  def from_noun([consumed, created | unit_delta]) do
    with {:ok, list_consumed} <- Noun.Nounable.List.from_noun(consumed),
         {:ok, list_created} <- Noun.Nounable.List.from_noun(created) do
      consumed_proper =
        Enum.map(list_consumed, fn [nul, rt | log] ->
          {Noun.atom_binary_to_integer(nul), Noun.atom_binary_to_integer(rt),
           Noun.atom_binary_to_integer(log)}
        end)

      created_proper =
        Enum.map(list_created, fn [cm | log] ->
          {Noun.atom_binary_to_integer(cm), Noun.atom_binary_to_integer(log)}
        end)

      {:ok,
       %__MODULE__{
         consumed: consumed_proper,
         created: created_proper,
         unit_delta: unit_delta |> Noun.atom_binary_to_integer()
       }}
    else
      _ -> :error
    end
  end

  defimpl Noun.Nounable, for: Instance do
    @impl true
    def to_noun(instance = %Instance{}) do
      [
        Noun.Nounable.to_noun(instance.consumed),
        Noun.Nounable.to_noun(instance.created) | instance.unit_delta
      ]
    end
  end
end

defmodule Anoma.RM.Transparent.ProvingSystem.CPS do
  @moduledoc """
  I am the compliance proving system module for the TRM.

  I provide the main interface for everything that has to do with the TRM
  compliance.

  ### Public API

  I provide the following public functionality

  - `key/0`
  - `verify_cms_compliance/1`
  - `verify_nulfs_compliance/1`
  - `check_nullified_has_been_created/2`
  - `prove/3`
  - `verify/3`
  - `verify_jet/3`
  - `verify_delta/3``
  """
  alias Anoma.RM.Transparent.Primitive.DeltaHash
  alias Anoma.RM.Transparent.Primitive.CommitmentAccumulator
  alias Anoma.RM.Transparent.Resource
  alias Anoma.RM.Transparent.ProvingSystem.CPS.Instance
  alias __MODULE__

  require Logger
  use TypedStruct

  @type cps_key :: binary()
  @cps_key "[8 [9 382 0 7] 9 2 10 [6 7 [0 3] [0 12] [0 26] 0 27] 0 2]"
           |> Noun.Format.parse_always()
           |> Noun.Jam.jam()

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

  @doc """
  I am the TRM compliance proving system verifying and proving key.

  Namely, I am the identifier of the verification gate encoded into the Nockma
  standard library. In particular, I am the jam of the logic of the verify gate.

  Developer warning: this means that if the layer in which the gate is located
  changes, this value may change as well.
  """
  @spec key() :: binary()
  def key() do
    @cps_key
  end

  @doc """
  I am the TRM compliance proving system prove interface.

  As we are in the transparent case, this is always trivial.
  """
  @spec prove(<<>>, Instance.t(), <<>>) :: <<>>
  def prove(_, _, _) do
    <<>>
  end

  @doc """
  I am the verification function for the TRM compliance proving system.

  I cue the jammed predicate given, then evaluate it giving the instance as
  an argument.
  """
  @spec verify(cps_key(), Instance.t(), <<>>) :: boolean()
  def verify(jammed_predicate, instance, <<>>) do
    with {:ok, predicate} <- Noun.Jam.cue(jammed_predicate),
         {:ok, res} <-
           Nock.nock([predicate, 0 | Nock.Lib.rm_core()], [
             9,
             2,
             10,
             [6, 1 | Noun.Nounable.to_noun(instance)],
             0 | 1
           ]) do
      Noun.equal?(res, 0)
    else
      _ -> false
    end
  end

  @doc """
  I am the verify jet. I am to be used as the jet of the verification
  gate in the Nockma standard library.

  I check the constraints as specified by the RM specification.
  """
  @spec verify_jet(
          list({integer(), integer(), integer()}),
          list({integer(), integer()}),
          integer()
        ) :: boolean()
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

      {:error, message} ->
        Logger.error(message)
        false
    end
  end

  @doc """
  I am the delta verification function of the TRM implementation of the
  compliance proof system.

  I check that the delta of the unit is computed correctly.
  """
  @spec verify_delta(
          MapSet.t(Resource.t()),
          MapSet.t(Resource.t()),
          integer()
        ) :: boolean()
  def verify_delta(consumed, created, unitdelta) do
    sum = &DeltaHash.delta_add(Resource.delta(&1), &2)
    consumed_delta = Enum.reduce(consumed, 2, sum)
    created_delta = Enum.reduce(created, 2, sum)

    unitdelta == DeltaHash.delta_sub(consumed_delta, created_delta)
  end

  @doc """
  I am the nullifier verification function of the TRM

  I check that the nullifiers provided are indeed nullifiers of some
  resources and that, moreover, these are in specified accumulators.

  See `check_nullified_has_been_created/2` for more info.
  """
  @spec verify_nulfs_compliance(list({integer(), integer()})) ::
          {:ok, MapSet.t(Resource.t())} | {:error, String.t()}
  def verify_nulfs_compliance(consumed) do
    Enum.reduce_while(consumed, {:ok, MapSet.new()}, fn {nul, rt},
                                                        {:ok, acc} ->
      with <<"NF_", rest::bitstring>> <- nul |> Noun.atom_integer_to_binary(),
           {:ok, noun_resource} <- Noun.Jam.cue(rest),
           {:ok, resource} <- Resource.from_noun(noun_resource),
           true <-
             check_nullified_has_been_created(resource, rt) do
        {:cont, {:ok, MapSet.put(acc, resource)}}
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

  @doc """
  I am the commitment verification function for the TRM.

  I check that the commitments provided are indeed commitments of some
  resources.
  """
  @spec verify_cms_compliance(list({integer(), integer()})) ::
          {:ok, MapSet.t(Resource.t())} | {:error, String.t()}
  def verify_cms_compliance(consumed) do
    Enum.reduce_while(consumed, {:ok, MapSet.new()}, fn {cm, _}, {:ok, acc} ->
      with <<"CM_", rest::bitstring>> <- cm |> Noun.atom_integer_to_binary(),
           {:ok, noun_resource} <- Noun.Jam.cue(rest),
           {:ok, resource} <- Resource.from_noun(noun_resource) do
        {:cont, {:ok, MapSet.put(acc, resource)}}
      else
        _ ->
          {:halt,
           {:error,
            "Commitment\n" <>
              "#{inspect(cm, pretty: true)}\n" <> "is noncompliant"}}
      end
    end)
  end

  @doc """
  A am the root verification function for the TRM compliance prooving system.

  Given a resource and a root of the accumulator, if the resource is not
  ephemeral, I decode the root to provide access to the underlying set and
  verify that the resource is indeed contained in it.
  """
  @spec check_nullified_has_been_created(Resource.t(), integer()) ::
          true | {:error, String.t()}
  def check_nullified_has_been_created(res, rt) do
    unless res.isephemeral do
      cm = res |> Resource.commitment_hash()

      with {:ok, noun_acc} <-
             rt |> Noun.atom_integer_to_binary() |> Noun.Jam.cue(),
           {:ok, acc} <- noun_acc |> Noun.Nounable.MapSet.from_noun(),
           true <- CommitmentAccumulator.verify(cm, acc, rt) do
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

  @spec from_noun(Noun.t()) :: {:ok, t()} | :error
  def from_noun([pk, vk, instance, witness, proof]) do
    with true <- Noun.atom_integer_to_binary(pk) == key(),
         true <- Noun.atom_integer_to_binary(vk) == key(),
         {:ok, instance} <- Instance.from_noun(instance),
         true <- Noun.equal?(witness, 0),
         true <- Noun.equal?(proof, 0) do
      {:ok, %__MODULE__{instance: instance}}
    else
      _ -> :error
    end
  end

  defimpl Noun.Nounable, for: CPS do
    @impl true
    def to_noun(t = %CPS{}) do
      [
        t.proving_key,
        t.verifying_key,
        Noun.Nounable.to_noun(t.instance),
        <<>> | <<>>
      ]
    end
  end
end
