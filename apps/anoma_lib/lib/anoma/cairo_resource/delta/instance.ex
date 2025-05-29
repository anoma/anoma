defmodule Anoma.CairoResource.Delta.Instance do
  @moduledoc """
  I represent the delta's instance.
  """

  alias __MODULE__
  alias Anoma.CairoResource.Action
  use TypedStruct

  typedstruct enforce: true do
    # Transaction delta (computed from compliance unit deltas by adding them together)
    field(:delta, list(byte()), default: [])

    # The tx_digest (which includes nullifiers and commitments) is the message
    # used in the binding signature. This is a specific field for shielded RM.
    field(:tx_digest, list(binary()), default: [])

    # Only zero balance is allowed for delta in shielded RM. Expected_balance is
    # not currently used. If the expected_balance is concealed, it is a 256-bit
    # curve point type; otherwise, it is a list of kind-quantity pairs.
    field(:expected_balance, <<_::256>>, default: <<0::256>>)
  end

  @spec sum(MapSet.t(Action.t())) :: list(byte())
  def sum(actions) do
    actions
    |> Enum.flat_map(fn action ->
      Action.delta(action)
    end)
    |> Enum.reduce(
      [],
      &[:binary.bin_to_list(&1) | &2]
    )
  end

  @spec to_instance(MapSet.t(Action.t()), binary()) :: t()
  @default_balance <<0::256>>
  def to_instance(actions, expected_balance \\ @default_balance) do
    nullifiers =
      actions
      |> Enum.flat_map(&Action.nullifiers/1)

    commitments =
      actions
      |> Enum.flat_map(&Action.commitments/1)

    %Instance{
      delta: sum(actions),
      tx_digest: nullifiers ++ commitments,
      expected_balance: expected_balance
    }
  end
end
