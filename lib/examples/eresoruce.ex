defmodule Examples.EResource do
  use Memoize

  require ExUnit.Assertions
  import ExUnit.Assertions

  alias Anoma.Resource

  alias Examples.{ENock, ECrypto}

  ####################################################################
  ##                         Basic Creation                         ##
  ####################################################################

  # new_with_npk gives new resources, so memo again
  defmemo a_resource() do
    Resource.new_with_npk(ECrypto.alice().external.sign)
  end

  defmemo a2_resource() do
    Resource.new_with_npk(ECrypto.alice().external.sign)
  end

  defmemo b_resource() do
    Resource.new_with_npk(ECrypto.bertha().external.sign)
  end

  defmemo a10_space_resource() do
    %Resource{a_resource() | label: "space bucks", quantity: 10}
    |> Resource.unique()
  end

  defmemo a5_space_resource() do
    %Resource{a10_space_resource() | quantity: 5}
    |> Resource.unique()
  end

  defmemo b10_space_resource() do
    %Resource{a10_space_resource() | npk: ECrypto.bertha().external.sign}
    |> Resource.unique()
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

  defmemo ax_resource() do
    %Resource{a10_d0_resource() | quantity: 1, label: "x"}
    |> Resource.unique()
  end

  defmemo ay_resource() do
    %Resource{ax_resource() | label: "y"} |> Resource.unique()
  end

  defmemo bx_resource() do
    %Resource{b10_d0_resource() | quantity: 1, label: "x"}
    |> Resource.unique()
  end

  defmemo by_resource() do
    %Resource{bx_resource() | label: "y"} |> Resource.unique()
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

  def ax_commit(), do: Resource.commitment(ax_resource())
  def ay_commit(), do: Resource.commitment(ay_resource())
  def bx_commit(), do: Resource.commitment(bx_resource())
  def by_commit(), do: Resource.commitment(by_resource())

  ####################################################################
  ##                          Nullifiers                            ##
  ####################################################################

  @spec a_nullifier() :: binary()
  def a_nullifier() do
    nullifier =
      Resource.nullifier(a_resource(), ECrypto.alice().internal.sign)

    assert nullifier |> Resource.nullifies(a_resource())
    refute nullifier |> Resource.nullifies(b_resource())
    nullifier
  end

  @spec a2_nullifier() :: binary()
  def a2_nullifier() do
    nullifier =
      Resource.nullifier(a2_resource(), ECrypto.alice().internal.sign)

    assert nullifier |> Resource.nullifies(a2_resource())
    refute nullifier |> Resource.nullifies(a_resource())
    assert nullifier != a_nullifier()
    nullifier
  end

  @spec b_nullifier() :: binary()
  def b_nullifier() do
    nullifier =
      Resource.nullifier(b_resource(), ECrypto.bertha().internal.sign)

    assert nullifier |> Resource.nullifies(b_resource())
    refute nullifier |> Resource.nullifies(a_resource())
    nullifier
  end

  @spec invalid_nullifier() :: binary()
  def invalid_nullifier() do
    nullifier =
      Resource.nullifier(a_resource(), ECrypto.bertha().internal.sign)

    refute nullifier |> Resource.nullifies(a_resource())
    nullifier
  end

  def a10_space_nullifier(),
    do:
      Resource.nullifier(a10_space_resource(), ECrypto.alice().internal.sign)

  def a5_space_nullifier(),
    do: Resource.nullifier(a5_space_resource(), ECrypto.alice().internal.sign)

  def b10_space_nullifier(),
    do:
      Resource.nullifier(b10_space_resource(), ECrypto.bertha().internal.sign)

  def a10_d0_nullifier(),
    do: Resource.nullifier(a10_d0_resource(), ECrypto.alice().internal.sign)

  def b10_d0_nullifier(),
    do: Resource.nullifier(b10_d0_resource(), ECrypto.bertha().internal.sign)

  def a0_counter_nullifier(),
    do:
      Resource.nullifier(a0_counter_resource(), ECrypto.alice().internal.sign)

  def a1_counter_nullifier(),
    do:
      Resource.nullifier(a1_counter_resource(), ECrypto.alice().internal.sign)

  def ax_nullifier(),
    do: Resource.nullifier(ax_resource(), ECrypto.alice().internal.sign)

  def ay_nullifier(),
    do: Resource.nullifier(ay_resource(), ECrypto.alice().internal.sign)

  def bx_nullifier(),
    do: Resource.nullifier(bx_resource(), ECrypto.bertha().internal.sign)

  def by_nullifier(),
    do: Resource.nullifier(by_resource(), ECrypto.bertha().internal.sign)

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

  @spec x_kind() :: binary()
  def x_kind(), do: Resource.kind(ax_resource())

  @spec y_kind() :: binary()
  def y_kind(), do: Resource.kind(ay_resource())
end
