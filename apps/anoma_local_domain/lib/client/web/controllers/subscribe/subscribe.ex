defmodule Anoma.LocalDomain.Web.SubscribeController do
  use Anoma.LocalDomain.Web, :controller
  use OpenApiSpex.ControllerSpecs

  action_fallback(Anoma.LocalDomain.Web.FallbackController)

  alias Anoma.LocalDomain.Node.GRPCProxy
  alias OpenApiSpex.Schema
  alias Anoma.LocalDomain.Web.SubscribeController.Spec

  ############################################################
  #                           OpenAPI Spec                   #
  ############################################################

  tags(["Events"])

  operation(:subscribe,
    summary: "Subscribe to events from the node",
    parameters: [],
    request_body:
      {"Topic to subscribe to", "application/json", Spec.SubscribeRequest},
    responses: [
      ok:
        {"Result message of adding the transaction", "application/json",
         %Schema{
           type: :object,
           description: "Result of submitting a transaction to the mempool",
           properties: %{
             message: %Schema{
               type: :string,
               default: "subscribed",
               description: "The result message"
             }
           }
         }}
    ]
  )

  ############################################################
  #                          Actions                         #
  ############################################################

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
