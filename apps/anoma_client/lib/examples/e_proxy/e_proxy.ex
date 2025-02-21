defmodule Anoma.Client.Examples.EProxy do
  @moduledoc """
  I contain examples for the GRPC proxy.

  The proxy is started, and if necessary, a node is started too.

  I then test each public API of the proxy to ensure it works as expected.
  """

  alias Anoma.Client.Examples.EClient

  @spec setup() :: EClient.t()
  def setup() do
    EClient.create_example_client()
  end
end
