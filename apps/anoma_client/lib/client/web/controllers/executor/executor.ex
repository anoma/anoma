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

  operation(:run_scry,
    summary: "Run a scry.",
    parameters: [],
    request_body: {"Key to scry", "application/json", Spec.ScryKey},
    responses: [
      ok: {"Evaluation result", "application/json", Spec.Result}
    ]
  )

  ############################################################
  #                          Actions                         #
  ############################################################

  @doc """
  I a scry on the remote executor.

  I expect a jammed noun as paramter, base64 encoded.

  If anything goes wrong, I will return an error and this will be handled by the fallback controller.
  """
  def run_scry(conn, params = %{"key" => _}) do
    with {:ok, key} <- Base.decode64(params["key"]),
         {:ok, result} <- GRPCProxy.run_scry(key) do
      render(conn, "run_scry.json", result: result)
    end
  end
end
