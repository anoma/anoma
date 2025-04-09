defmodule Anoma.Client.Web.ExecutorController do
  use Anoma.Client.Web, :controller
  use OpenApiSpex.ControllerSpecs

  action_fallback(Anoma.Client.Web.FallbackController)

  alias Anoma.Client.Node.GRPCProxy
  alias Anoma.Client.Web.ExecutorController.Spec

  ############################################################
  #                           OpenAPI Spec                   #
  ############################################################

  tags(["Executor"])

  operation(:add_read_only_transaction,
    summary: "Execute a read-only transaction.",
    parameters: [],
    request_body:
      {"Transaction candidate to submit", "application/json",
       Spec.ReadOnlyTransaction},
    responses: [
      ok: {"Evaluation result", "application/json", Spec.Result}
    ]
  )

  ############################################################
  #                          Actions                         #
  ############################################################

  @doc """
  I add a read-only transaction to the remote executor.

  I expect a jammed noun as paramter, base64 encoded.

  If anything goes wrong, I will return an error and this will be handled by the fallback controller.
  """
  def add_read_only_transaction(conn, params = %{"transaction" => _}) do
    with {:ok, intent} <- Base.decode64(params["transaction"]),
         {:ok, result} <- GRPCProxy.add_read_only_transaction(intent) do
      render(conn, "add_read_only_transaction.json", result: result)
    end
  end
end
