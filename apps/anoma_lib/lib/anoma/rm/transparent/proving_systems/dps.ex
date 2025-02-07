defmodule Anoma.RM.Transparent.ProvingSystem.DPS.Instance do
  alias __MODULE__
  use TypedStruct

  typedstruct enforce: true do
    # the jam of the delta map computed from compliance unit deltas
    field(:delta, integer(), default: 2)
    # 2 is the empty map
    # if we expect expected balance to be somehow changable, we should
    # store expected balance somewhere
    field(:expectedbalance, 2, default: 2)
  end

  @spec from_noun(Noun.t()) :: t()
  def from_noun([_delta | _expected_balance]) do
    %__MODULE__{}
  end

  @spec to_noun(t()) :: Noun.t()
  def to_noun(_instance) do
    0
  end
end

defmodule Anoma.RM.Transparent.ProvingSystem.DPS do
  alias Anoma.RM.Transparent.ProvingSystem.DPS.Instance

  use TypedStruct

  @type dps_key :: <<>>
  @dps_key <<>>

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

  @spec key() :: binary()
  def key() do
    @dps_key
  end

  @spec prove(<<>>, Instance.t(), <<>>) :: <<>>
  def prove(_, _, _) do
    <<>>
  end

  @spec aggregate(<<>>, <<>>) :: <<>>
  def aggregate(_t1, _t2) do
    <<>>
  end

  @spec verify(dps_key, Instance.t(), <<>>) :: bool()
  def verify(jammed_predicate, instance, <<>>) do
    with {:ok, predicate} = Noun.Jam.cue(jammed_predicate),
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

  @spec verify_jet(integer(), integer()) :: bool()
  def verify_jet(delta, expected_balance) do
    delta == expected_balance
  end
end
