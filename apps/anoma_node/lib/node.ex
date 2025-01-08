defmodule Anoma.Node do
  use Application

  @impl true
  def start(_type, args) do
    Anoma.Supervisor.start_link(args)
  end

  @default_bytes_length 16
  @valid_prefix_pattern ~r/^[a-zA-Z0-9_-]+$/

  @type id_options :: [
    bytes_length: pos_integer(),
    validate_unique: boolean()
  ]

  @doc """
  I am the random ID generator for examples.

  I call `prefix_random_id/1` with argument `example_`
  """
  @spec example_random_id() :: {:ok, String.t()} | {:error, atom()}
  def example_random_id() do
    prefix_random_id("example_")
  end

  @doc """
  I am the random ID generator with prefixed string.

  I concatenate a given string with a random base 16 string.
  The prefix must match the pattern: ^[a-zA-Z0-9_-]+$

  ## Options
    * `:bytes_length` - Length of random bytes to generate (default: 16)
    * `:validate_unique` - Whether to validate ID uniqueness (default: false)

  ## Examples
      iex> prefix_random_id("test_")
      {:ok, "test_" <> <<random_string>>}

      iex> prefix_random_id("invalid prefix")
      {:error, :invalid_prefix}
  """
  @spec prefix_random_id(String.t(), id_options()) :: {:ok, String.t()} | {:error, atom()}
  def prefix_random_id(prefix, opts \\ []) when is_binary(prefix) do
    bytes_length = Keyword.get(opts, :bytes_length, @default_bytes_length)
    validate_unique = Keyword.get(opts, :validate_unique, false)

    with :ok <- validate_prefix(prefix),
         {:ok, id} <- generate_id(prefix, bytes_length),
         :ok <- maybe_validate_unique(id, validate_unique) do
      {:ok, id}
    end
  end

  @doc """
  Same as prefix_random_id/2 but raises on error
  """
  @spec prefix_random_id!(String.t(), id_options()) :: String.t()
  def prefix_random_id!(prefix, opts \\ []) do
    case prefix_random_id(prefix, opts) do
      {:ok, id} -> id
      {:error, reason} -> raise "Failed to generate ID: #{reason}"
    end
  end

  # Private functions

  defp validate_prefix(prefix) do
    if Regex.match?(@valid_prefix_pattern, prefix) do
      :ok
    else
      {:error, :invalid_prefix}
    end
  end

  defp generate_id(prefix, bytes_length) when bytes_length > 0 do
    random = :crypto.strong_rand_bytes(bytes_length) |> Base.encode16()
    {:ok, prefix <> random}
  end
  defp generate_id(_, _), do: {:error, :invalid_bytes_length}

  defp maybe_validate_unique(_id, false), do: :ok
  defp maybe_validate_unique(id, true) do
    # Here we could add actual uniqueness validation against a storage
    # For now we just return :ok as a placeholder
    :ok
  end
end
