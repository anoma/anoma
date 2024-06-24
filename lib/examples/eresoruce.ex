defmodule Examples.EResource do
  use Memoize

  require ExUnit.Assertions
  import ExUnit.Assertions

  alias Anoma.Resource
  alias Anoma.Crypto.Sign

  alias Examples.ENock

  # Memoize it as we want it to always be the same!
  defmemo(keypair_a(), do: Sign.new_keypair())
  defmemo(keypair_b(), do: Sign.new_keypair())

  ####################################################################
  ##                         Basic Creation                         ##
  ####################################################################

  # new_with_npk gives new resources, so memo again
  defmemo(a_resource(), do: Resource.new_with_npk(keypair_a().public))
  defmemo(b_resource(), do: Resource.new_with_npk(keypair_b().public))
  defmemo(a2_resource(), do: Resource.new_with_npk(keypair_a().public))

  defmemo a10_space_resource() do
    %Resource{
      Resource.new_with_npk(keypair_a().public)
      | label: "space bucks",
        quantity: 10
    }
  end

  defmemo a5_space_resource() do
    %Resource{a10_space_resource() | quantity: 5}
    |> Resource.unique()
  end

  defmemo b10_space_resource() do
    %Resource{
      Resource.new_with_npk(keypair_b().public)
      | label: "space bucks",
        quantity: 10
    }
  end

  defmemo a10_d0_resource() do
    %Resource{a10_space_resource() | logic: ENock.zero_delta_logic()}
    |> Resource.unique()
  end

  defmemo b10_d0_resource() do
    %Resource{b10_space_resource() | logic: ENock.zero_delta_logic()}
    |> Resource.unique()
  end

  defmemo a0_counter_resource() do
    %Resource{
      a_resource()
      | logic: ENock.counter_logic(),
        label: "counter",
        quantity: 0
    }
    |> Resource.unique()
  end

  defmemo a1_counter_resource() do
    %Resource{a0_counter_resource() | quantity: 1} |> Resource.unique()
  end

  ####################################################################
  ##                          Commitments                           ##
  ####################################################################

  # Now we get interesting
  @spec a_commit() :: binary()
  def a_commit() do
    commitment = Resource.commitment(a_resource())
    assert commitment |> Resource.commits_to(a_resource())
    refute commitment |> Resource.commits_to(b_resource())
    commitment
  end

  @spec a2_commit() :: binary()
  def a2_commit() do
    commitment = Resource.commitment(a2_resource())
    assert commitment |> Resource.commits_to(a2_resource())
    refute commitment |> Resource.commits_to(a_resource())
    assert commitment != a_commit()
    commitment
  end

  @spec b_commit() :: binary()
  def b_commit() do
    commitment = Resource.commitment(b_resource())
    assert commitment |> Resource.commits_to(b_resource())
    refute commitment |> Resource.commits_to(a_resource())
    commitment
  end

  def a10_space_commit(), do: Resource.commitment(a10_space_resource())
  def a5_space_commit(), do: Resource.commitment(a5_space_resource())
  def b10_space_commit(), do: Resource.commitment(b10_space_resource())
  def a10_d0_commit(), do: Resource.commitment(a10_d0_resource())
  def b10_d0_commit(), do: Resource.commitment(b10_d0_resource())
  def a0_counter_commit(), do: Resource.commitment(a0_counter_resource())
  def a1_counter_commit(), do: Resource.commitment(a1_counter_resource())

  ####################################################################
  ##                          Nullifiers                            ##
  ####################################################################

  @spec a_nullifier() :: binary()
  def a_nullifier() do
    nullifier = Resource.nullifier(a_resource(), keypair_a().secret)
    assert nullifier |> Resource.nullifies(a_resource())
    refute nullifier |> Resource.nullifies(b_resource())
    nullifier
  end

  @spec a2_nullifier() :: binary()
  def a2_nullifier() do
    nullifier = Resource.nullifier(a2_resource(), keypair_a().secret)
    assert nullifier |> Resource.nullifies(a2_resource())
    refute nullifier |> Resource.nullifies(a_resource())
    assert nullifier != a_nullifier()
    nullifier
  end

  @spec b_nullifier() :: binary()
  def b_nullifier() do
    nullifier = Resource.nullifier(b_resource(), keypair_b().secret)
    assert nullifier |> Resource.nullifies(b_resource())
    refute nullifier |> Resource.nullifies(a_resource())
    nullifier
  end

  @spec invalid_nullifier() :: binary()
  def invalid_nullifier() do
    nullifier = Resource.nullifier(a_resource(), keypair_b().secret)
    refute nullifier |> Resource.nullifies(a_resource())
    nullifier
  end

  def a10_space_nullifier(),
    do: Resource.nullifier(a10_space_resource(), keypair_a().secret)

  def a5_space_nullifier(),
    do: Resource.nullifier(a5_space_resource(), keypair_a().secret)

  def b10_space_nullifier(),
    do: Resource.nullifier(b10_space_resource(), keypair_b().secret)

  def a10_d0_nullifier(),
    do: Resource.nullifier(a10_d0_resource(), keypair_a().secret)

  def b10_d0_nullifier(),
    do: Resource.nullifier(b10_d0_resource(), keypair_b().secret)

  def a0_counter_nullifier(),
    do: Resource.nullifier(a0_counter_resource(), keypair_a().secret)

  def a1_counter_nullifier(),
    do: Resource.nullifier(a1_counter_resource(), keypair_a().secret)

  ####################################################################
  ##                             Kinds                              ##
  ####################################################################

  @spec a_kind() :: binary()
  def a_kind() do
    Resource.kind(a_resource())
  end

  @spec another_a_kind() :: binary()
  def another_a_kind() do
    kind = Resource.kind(a2_resource())
    assert a_kind() == kind, "same kind and label ought to be the same"
    kind
  end

  @spec d0_kind() :: binary()
  def d0_kind() do
    kind = Resource.kind(a10_d0_resource())
    assert a_kind() != kind, "different logic should change the kind"
    kind
  end

  def counter_kind(), do: Resource.kind(a0_counter_resource())
end
