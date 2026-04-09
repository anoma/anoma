defmodule Anoma.Node.Transaction.Shard.Cell do
  @moduledoc """
  """
  use TypedStruct

  alias Anoma.Node.Transaction.Shard.Cell
  alias Anoma.Node.Transaction.Shard.Detail

  @typedoc "The height associated with an operation."
  @type height :: non_neg_integer()

  @typedoc "The watermark type"
  @type watermarks() :: %{write: height()}

  typedstruct enforce: true do
    @typedoc """
    I represent a cell within a shard
    """
    field(:details, %{height() => Detail.t()}, default: %{})
    field(:watermarks, watermarks(), default: %{write: 0})
  end

  ############################################################
  #                      Main Functions                      #
  ############################################################

  @spec reserve(t(), height()) :: {:ok, t()} | {:error, atom()}
  def reserve(c = %__MODULE__{}, height) do
    with :ok <- can_reserve(c, height),
         true <- detail_at(c, height) |> Detail.can_reserve?() do
      {:ok, update_detail(c, height, &Detail.reserve/1)}
    else
      false -> {:error, :occupied}
      err -> err
    end
  end

  @spec unreserve(t(), height()) :: t()
  def unreserve(c = %__MODULE__{}, height) do
    new_c = update_detail(c, height, &Detail.unreserve/1)

    case detail_at(c, height) do
      %Detail{cell: :reserved} -> resolve_pending(new_c)
      _ -> new_c
    end
  end

  @spec retract(t(), height(), pid()) :: t()
  def retract(c, height, pid) do
    update_detail(c, height, &Detail.retract(&1, pid))
  end

  @spec write(t(), height(), any()) :: t()
  def write(c = %__MODULE__{}, height, value) do
    case replace_detail(c, height, &Detail.write(&1, value)) do
      {:ok, new_c} -> resolve_pending(new_c)
      {:error, _e} -> c
    end
  end

  @spec read(t(), height()) :: {:ok, any()} | :absent | :blocked
  def read(%__MODULE__{watermarks: w}, h) when h > w.write + 1,
    do: :blocked

  def read(%__MODULE__{details: ds}, height) do
    # This represents the most recent operation relevant to the read.
    relevant_entries =
      Enum.filter(ds, fn
        {h, %Detail{cell: :reserved}} -> h < height
        {h, %Detail{cell: %{value: _}}} -> h < height
        {_, %Detail{cell: :empty}} -> false
      end)

    case Enum.max(relevant_entries, fn -> nil end) do
      nil -> :absent
      {_, %Detail{cell: :reserved}} -> :blocked
      {_, %Detail{cell: %{value: val}}} -> {:ok, val}
    end
  end

  @spec add_pending(t(), height(), GenServer.from()) :: t()
  def add_pending(c, height, from) do
    update = fn
      d = %Detail{pending: nil} -> %Detail{d | pending: [from]}
      d = %Detail{pending: p} -> %Detail{d | pending: [from | p]}
    end

    update_detail(c, height, update)
  end

  @spec run_advance_watermark(t(), height()) :: t()
  def run_advance_watermark(c = %Cell{watermarks: water}, height) do
    case advance_watermark(water, height) do
      ^water -> c
      new_water -> resolve_pending(%__MODULE__{c | watermarks: new_water})
    end
  end

  @spec resolve_pending(t()) :: t()
  def resolve_pending(c = %__MODULE__{details: ds}) do
    new_ds = Map.new(ds, fn {height, d} -> resolve_height(c, height, d) end)
    %__MODULE__{c | details: new_ds}
  end

  @spec resolve_height(t(), height(), Detail.t()) :: {height(), Detail.t()}
  defp resolve_height(_c, height, d = %Detail{pending: nil}),
    do: {height, d}

  defp resolve_height(c, height, d = %Detail{pending: ps}) do
    case read(c, height) do
      :blocked ->
        {height, d}

      value ->
        Enum.each(ps, &GenServer.reply(&1, value))
        {height, %Detail{d | pending: nil}}
    end
  end

  ############################################################
  #                     Helpers Details                      #
  ############################################################

  @spec detail_at(t(), height()) :: Detail.t()
  def detail_at(%__MODULE__{details: ds}, height) do
    Map.get(ds, height, %Detail{})
  end

  @spec replace_detail(t(), height(), (Detail.t() ->
                                         {:error, atom()} | {:ok, Detail.t()})) ::
          {:ok, t()} | {:error, atom()}
  def replace_detail(c = %__MODULE__{details: ds}, height, f) do
    with {:ok, val} <- Map.fetch(ds, height),
         {:ok, nv} <- f.(val) do
      {:ok, %__MODULE__{c | details: Map.put(ds, height, nv)}}
    else
      :error -> {:error, :empty}
      error -> error
    end
  end

  @spec update_detail(t(), height(), (Detail.t() -> Detail.t())) :: t()
  def update_detail(c = %__MODULE__{details: ds}, height, f) do
    %__MODULE__{c | details: Map.update(ds, height, f.(%Detail{}), f)}
  end

  ############################################################
  #                    Helpers Watermarks                    #
  ############################################################

  @doc "Checks if a reservation request conflicts with existing watermarks."
  @spec can_reserve(t(), height()) :: :ok | {:error, atom()}
  def can_reserve(%Cell{watermarks: %{write: mark}}, ht) when ht <= mark do
    {:error, :reserving_write_under_write_watermark}
  end

  def can_reserve(_, _), do: :ok

  @spec advance_watermark(watermarks(), height()) :: watermarks()
  def advance_watermark(w, height) do
    Map.replace_lazy(w, :write, &max(height, &1))
  end
end
