defmodule Anoma.Client.Web.SubscribeController do
  use Anoma.Client.Web, :controller

  action_fallback(Anoma.Client.Web.FallbackController)

  alias Anoma.Client.Node.GRPCProxy

  @doc """
  I return a list of all intents from the remote node.
  The intents will be jammed nouns, base64 encoded.
  """
  def subscribe(conn, _params = %{"topic" => topic}) do
    with {:ok, :subscribed} <- GRPCProxy.subscribe(topic) do
      render(conn, "subscribed.json")
    end
  end
end
