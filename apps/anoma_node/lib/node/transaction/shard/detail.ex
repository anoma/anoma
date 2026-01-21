defmodule Anoma.Node.Transaction.Shard.Detail do
  @moduledoc """

  """
  use TypedStruct

  @typedoc "I represent the value of a Cell"
  @type slot :: %{value: any()} | :empty | :reserved

  typedstruct enforce: true do
    @typedoc """
    I represent the details of a Shard Cell
    """
    field(:pending, nil | list(GenServer.from()), default: nil)
    field(:cell, slot(), default: :empty)
  end

  @spec write(t(), any()) :: {:ok, t()} | {:error, atom()}
  def write(d = %__MODULE__{cell: :reserved}, val) do
    {:ok, %__MODULE__{d | cell: %{value: val}}}
  end

  def write(_, _), do: {:error, :can_not_write}

  @spec reserve(t()) :: t()
  def reserve(d = %__MODULE__{cell: %{value: _}}), do: d
  def reserve(d = %__MODULE__{cell: :reserved}), do: d
  def reserve(d = %__MODULE__{cell: :empty}), do: %{d | cell: :reserved}

  @spec retract(t(), pid()) :: t()
  def retract(d = %__MODULE__{pending: nil}, _), do: d

  def retract(d = %__MODULE__{pending: ps}, pid) do
    val =
      Enum.reject(ps, fn {pid_2, _ref} -> pid_2 == pid end)

    %{
      d
      | pending:
          if Enum.empty?(val) do
            nil
          else
            val
          end
    }
  end

  @spec unreserve(t()) :: t()
  def unreserve(d = %__MODULE__{cell: :reserved}) do
    %__MODULE__{d | cell: :empty}
  end

  def unreserve(d), do: d

  @spec can_reserve?(t()) :: boolean()
  def can_reserve?(%__MODULE__{cell: %{value: _}}), do: false
  def can_reserve?(%__MODULE__{cell: :reserved}), do: false
  def can_reserve?(_), do: true
end
