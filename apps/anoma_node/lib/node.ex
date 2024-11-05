defmodule Anoma.Node do
  use Application

  @impl true
  def start(_type, args) do
    Anoma.Supervisor.start_link(args)
  end

  @doc """
  I am the random ID generator for examples.

  I call `prefix_random_id/1` with argument `example_`
  """

  @spec example_random_id() :: String.t()
  def example_random_id() do
    prefix_random_id("example_")
  end

  @doc """
  I am the random ID generator with prefixed string.

  I concatenate a given string with a random base 16 string.
  """

  @spec prefix_random_id(String.t()) :: String.t()
  def prefix_random_id(string) do
    string <> (:crypto.strong_rand_bytes(16) |> Base.encode16())
  end
end
