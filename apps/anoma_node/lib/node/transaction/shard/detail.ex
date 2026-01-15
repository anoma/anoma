defmodule Anoma.Node.Transaction.Shard.Detail do
  @moduledoc """

  """
  use TypedStruct

  alias Anoma.Node.Transaction.Shard.Cell

  @typedoc "I represent the value of a Cell"
  @type slot :: %{value: any()} | :empty | :reserved

  typedstruct enforce: true do
    @typedoc """
    I represent the details of a Shard Cell
    """
    field(:pending, nil | list(GenServer.from()), default: nil)
    field(:reserved_reads, non_neg_integer(), default: 0)
    field(:cell, slot(), default: :empty)
  end

  @spec write(t(), any()) :: {:ok, t()} | {:error, atom()}
  def write(d = %__MODULE__{cell: :reserved}, val) do
    {:ok, %__MODULE__{d | cell: %{value: val}}}
  end

  def write(_, _), do: {:error, :can_not_write}

  @spec reserve(t(), Cell.cap()) :: t()
  def reserve(d = %__MODULE__{reserved_reads: c}, :read) do
    %__MODULE__{d | reserved_reads: c + 1}
  end

  def reserve(d = %__MODULE__{cell: :empty}, :write) do
    %__MODULE__{d | cell: :reserved}
  end

  def reserve(d = %__MODULE__{cell: %{value: _}}, :write), do: d
  def reserve(d = %__MODULE__{cell: :reserved}, :write), do: d
  def reserve(d, :read_write), do: d |> reserve(:read) |> reserve(:write)

  @spec unreserve(t(), Cell.cap()) :: t()
  def unreserve(d = %__MODULE__{reserved_reads: c}, :read) do
    # Remove this hack with a better model for unreserve data that
    # passes who unreserved
    new_c = max(0, c - 1)

    pending =
      if 0 == new_c do
        nil
      else
        d.pending
      end

    %__MODULE__{d | reserved_reads: new_c, pending: pending}
  end

  def unreserve(d = %__MODULE__{cell: :reserved}, :write) do
    %__MODULE__{d | cell: :empty}
  end

  def unreserve(d, :write), do: d

  @spec can_reserve?(t(), Cell.cap()) :: boolean()
  def can_reserve?(%__MODULE__{cell: %{value: _}}, c)
      when c in [:write, :read_write],
      do: false

  def can_reserve?(%__MODULE__{cell: :reserved}, c)
      when c in [:write, :read_write],
      do: false

  def can_reserve?(_, _), do: true
end
