defmodule Examples.ETransparent.EResource do
  alias Anoma.Crypto.Randomness
  alias Anoma.RM.Transparent.Resource

  use Memoize
  use TestHelper.TestMacro

  @doc """
  I generate out unique trivial true resources.

  Use me when random resources are wanted.
  """
  @spec trivial_true_resource_generator() :: Resource.t()
  def trivial_true_resource_generator() do
    res = %Resource{nonce: Randomness.get_random(32)}

    {:ok, logic} =
      res.logicref |> Noun.atom_integer_to_binary() |> Noun.Jam.cue()

    {:ok, result} = Nock.nock(logic, [9, 2, 0 | 1])
    assert Noun.equal?(result, 0)
    res
  end

  @doc """
  I generate out unique trivial false resources.

  Use me when random resources are wanted.
  """
  @spec trivial_false_resource_generator() :: Resource.t()
  def trivial_false_resource_generator() do
    res = %Resource{
      logicref:
        [[<<1>> | <<1>>], <<>> | <<>>]
        |> Noun.Jam.jam()
        |> Noun.atom_binary_to_integer(),
      nonce: Randomness.get_random(32)
    }

    assert {:ok, uncued} =
             Noun.Nounable.to_noun(res)
             |> Noun.Jam.jam()
             |> Noun.Jam.cue!()
             |> Resource.from_noun()

    assert uncued == res

    {:ok, logic} =
      res.logicref |> Noun.atom_integer_to_binary() |> Noun.Jam.cue()

    {:ok, result} = Nock.nock(logic, [9, 2, 0 | 1])
    assert Noun.equal?(result, 1)
    res
  end

  @spec trivial_true_resource() :: Resource.t()
  defmemo trivial_true_resource() do
    %Resource{
      trivial_true_resource_generator()
      | nonce: Randomness.get_random(32)
    }
  end

  @spec trivial_true_resource_2() :: Resource.t()
  defmemo trivial_true_resource_2() do
    %Resource{
      trivial_true_resource_generator()
      | nonce: Randomness.get_random(32)
    }
  end

  @spec trivial_true_resource_eph() :: Resource.t()
  defmemo trivial_true_resource_eph() do
    %Resource{
      trivial_true_resource_generator()
      | nonce: Randomness.get_random(32),
        isephemeral: true
    }
  end

  @spec trivial_false_resource() :: Resource.t()
  defmemo trivial_false_resource() do
    %Resource{
      trivial_false_resource_generator()
      | nonce: Randomness.get_random(32)
    }
  end

  @spec trivial_true_commitment() :: integer()
  def trivial_true_commitment() do
    res = Resource.commitment_hash(trivial_true_resource())

    assert Resource.commits?(trivial_true_resource(), res)
    # The aura of the commitment does not matter
    assert Resource.commits?(
             trivial_true_resource(),
             res |> Noun.atom_binary_to_integer()
           )

    refute Resource.nullifies?(trivial_true_resource(), res)

    res
  end

  @spec trivial_true_nullifier() :: integer()
  def trivial_true_nullifier() do
    res = Resource.nullifier_hash(<<0::256>>, trivial_true_resource())

    assert Resource.nullifies?(trivial_true_resource(), res)
    # The aura of the commitment does not matter
    assert Resource.nullifies?(
             trivial_true_resource(),
             res |> Noun.atom_binary_to_integer()
           )

    refute Resource.commits?(trivial_true_resource(), res)

    res
  end

  @spec trivial_true_nullifier_2() :: integer()
  def trivial_true_nullifier_2() do
    Resource.nullifier_hash(<<0::256>>, trivial_true_resource_2())
  end

  @spec trivial_true_nullifier_eph() :: integer()
  def trivial_true_nullifier_eph() do
    Resource.nullifier_hash(<<0::256>>, trivial_true_resource_eph())
  end

  @spec trivial_false_commitment() :: integer()
  def trivial_false_commitment() do
    Resource.commitment_hash(trivial_false_resource())
  end
end
