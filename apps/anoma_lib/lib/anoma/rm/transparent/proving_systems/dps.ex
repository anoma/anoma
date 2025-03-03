defmodule Anoma.RM.Transparent.ProvingSystem.DPS.Instance do
  @moduledoc """
  I am the instance module for the delta proving system of the TRM.

  My main purpose is to define a proper structure to represent the kind of
  arguments to be fed into a delta unit computation and the appropriate
  API to transform to and from Nockma nouns.

  ### Public API

  I have the following public functionality

  - `from_noun/1`
  - `to_noun/1`
  """
  alias __MODULE__

  use TypedStruct

  typedstruct enforce: true do
    # the jam of the delta map computed from compliance unit deltas
    field(:delta, integer(), default: 2)
    # 2 is the empty map
    # if we expect expected balance to be somehow changable, we should
    # store expected balance somewhere
    field(:expected_balance, 2, default: 2)
  end

  @spec(from_noun(Noun.t()) :: {:ok, t()}, :error)
  def from_noun([delta | expected_balance]) do
    with true <- expected_balance == 2 do
      {:ok,
       %__MODULE__{
         delta: Noun.atom_binary_to_integer(delta),
         expected_balance: 2
       }}
    else
      _ -> :error
    end
  end

  defimpl Noun.Nounable, for: Instance do
    @impl true
    def to_noun(instance = %Instance{}) do
      [instance.delta | instance.expected_balance]
    end
  end
end

defmodule Anoma.RM.Transparent.ProvingSystem.DPS do
  @moduledoc """
  I am the delta proving system module for the TRM.

  I provide the interface for everything that has to do with the TRM delta
  prooving.

  ### Public API

  I have the following public functionality

  - `key/0`
  - `aggregate/2`
  - `verify_jet/2`
  - `prove/3`
  - `verify/3`
  """
  alias Anoma.RM.Transparent.ProvingSystem.DPS.Instance
  alias __MODULE__
  use TypedStruct

  @type dps_key :: binary()
  @dps_key "[8 [9 374 0 7] 9 2 10 [6 7 [0 3] [0 12] 0 13] 0 2]"
           |> Noun.Format.parse_always()
           |> Noun.Jam.jam()

  typedstruct enforce: true do
    # pk and vk is the jam of the gate calling DPS.verify_jet
    # empty for now
    field(:proving_key, dps_key, default: @dps_key)
    field(:verifying_key, dps_key, default: @dps_key)
    # structured data for running delta checks
    # inherently ordered by the key orders in definition
    field(:instance, Instance.t(), default: %Instance{})
    # witness and proofs are empty for transparent case
    field(:witness, <<>>, default: <<>>)
    field(:proof, <<>>, default: <<>>)
  end

  @doc """
  I am the TRM delta proving system verifying and proving key.

  Namely, I the identifier of the verification gate encoded into the Nockma
  standard library. In particular, I the jam of the logic of the verify gate.

  Developer warning: this means that if the layer in which the gate is located
  changes, this value may change as well.
  """
  @spec key() :: binary()
  def key() do
    @dps_key
  end

  @doc """
  I am the TRM delta proving system prove interface.

  As we are in the transparent case, this is always trivial.
  """
  @spec prove(<<>>, Instance.t(), <<>>) :: <<>>
  def prove(_, _, _) do
    <<>>
  end

  @doc """
  I am the TRM delta proof aggregate.

  In the transparent case, this is always trivial.
  """
  @spec aggregate(<<>>, <<>>) :: <<>>
  def aggregate(_t1, _t2) do
    <<>>
  end

  @doc """
  I am the verification function of the TRM delta proving system.

  I cue the jammed predicate given, then evaluate it giving the instance as
  an argument.
  """
  @spec verify(dps_key, Instance.t(), <<>>) :: boolean()
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
  @spec verify_jet(integer(), integer()) :: boolean()
  def verify_jet(delta, expected_balance) do
    delta == expected_balance
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

  defimpl Noun.Nounable, for: DPS do
    @impl true
    def to_noun(t = %DPS{}) do
      [
        t.proving_key,
        t.verifying_key,
        Noun.Nounable.to_noun(t.instance),
        <<>> | <<>>
      ]
    end
  end
end
