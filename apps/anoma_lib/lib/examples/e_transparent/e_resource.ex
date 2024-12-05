defmodule Examples.ETransparent.EResource do
  use Memoize

  alias Anoma.TransparentResource.Resource
  alias Anoma.Crypto.Randomness

  use TestHelper.TestMacro

  @doc """
  I generate out unique trivial true resources.

  Use me when random resources are wanted.
  """
  @spec trivial_true_resource_generator() :: Resource.t()
  def trivial_true_resource_generator() do
    res = %Resource{nonce: Randomness.get_random(32)}

    {:ok, 0} = Nock.nock(res.logic, [9, 2, 0 | 1])
    res
  end

  @doc """
  I generate out unique trivial false resources.

  Use me when random resources are wanted.
  """
  @spec trivial_false_resource_generator() :: Resource.t()
  def trivial_false_resource_generator() do
    res = %Resource{
      logic: [[<<1>> | <<1>>], <<>> | <<>>],
      nonce: Randomness.get_random(32)
    }

    assert {:ok, uncued} =
             Resource.to_noun(res)
             |> Noun.Jam.jam()
             |> Noun.Jam.cue()
             |> elem(1)
             |> Resource.from_noun()

    assert uncued == res
    {:ok, <<1>>} = Nock.nock(res.logic, [9, 2, 0 | 1])
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
        ephemeral: true
    }
  end

  @spec trivial_false_resource() :: Resource.t()
  defmemo trivial_false_resource() do
    %Resource{
      trivial_false_resource_generator()
      | nonce: Randomness.get_random(32)
    }
  end

  @spec trivial_true_commitment() :: Resource.commitment()
  def trivial_true_commitment() do
    res = Resource.commitment(trivial_true_resource())

    assert Resource.commits?(trivial_true_resource(), res)
    # The aura of the commitment does not matter
    assert Resource.commits?(
             trivial_true_resource(),
             res |> Noun.atom_binary_to_integer()
           )

    refute Resource.nullifies?(trivial_true_resource(), res)

    res
  end

  @spec trivial_true_nullifier() :: Resource.nullifier()
  def trivial_true_nullifier() do
    res = Resource.nullifier(trivial_true_resource())

    assert Resource.nullifies?(trivial_true_resource(), res)
    # The aura of the commitment does not matter
    assert Resource.nullifies?(
             trivial_true_resource(),
             res |> Noun.atom_binary_to_integer()
           )

    refute Resource.commits?(trivial_true_resource(), res)

    res
  end

  @spec trivial_true_nullifier_2() :: Resource.nullifier()
  def trivial_true_nullifier_2() do
    Resource.nullifier(trivial_true_resource_2())
  end

  @spec trivial_true_nullifier_eph() :: Resource.nullifier()
  def trivial_true_nullifier_eph() do
    Resource.nullifier(trivial_true_resource_eph())
  end

  @spec trivial_false_commitment() :: Resource.commitment()
  def trivial_false_commitment() do
    Resource.commitment(trivial_false_resource())
  end
end
