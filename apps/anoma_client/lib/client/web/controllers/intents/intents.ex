defmodule Anoma.Client.Web.IntentsController do
  use Anoma.Client.Web, :controller

  action_fallback(Anoma.Client.Web.FallbackController)

  alias Anoma.Client.Node.GRPCProxy

  @doc """
  I return a list of all intents from the remote node.
  The intents will be jammed nouns, base64 encoded.
  """
  def index(conn, _params) do
    with {:ok, intents} <- GRPCProxy.list_intents() do
      render(conn, "index.json", intents: intents)
    end
  end

  @doc """
  I add an intent to the remote intent pool.

  I expect a jammed noun as paramter, base64 encoded.

  If anything goes wrong, I will return an error and this will be handled by the fallback controller.
  """
  def add_intent(conn, params = %{"intent" => _}) do
    with {:ok, intent} <- Base.decode64(params["intent"]),
         {:ok, :added} <- GRPCProxy.add_intent(intent) do
      render(conn, "add_intent.json")
    end
  end
end
