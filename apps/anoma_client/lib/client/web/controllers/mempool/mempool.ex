defmodule Anoma.Client.Web.MempoolController do
  use Anoma.Client.Web, :controller
  use OpenApiSpex.ControllerSpecs

  action_fallback(Anoma.Client.Web.FallbackController)

  alias Anoma.Client.Node.GRPCProxy
  alias Anoma.Client.Web.MempoolController.Spec

  ############################################################
  #                           OpenAPI Spec                   #
  ############################################################

  tags(["Mempool"])

  operation(:add_transaction,
    summary: "Add transaction to the mempool",
    parameters: [],
    request_body:
      {"The transaction to add to the mempool.", "application/json",
       Spec.AddTransaction},
    responses: [
      ok:
        {"Result message of adding the transaction", "application/json",
         Spec.AddResult}
    ]
  )

  ############################################################
  #                          Actions                         #
  ############################################################

  @doc """
  I add an intent to the remote intent pool.

  I expect a jammed noun as paramter, base64 encoded.

  If anything goes wrong, I will return an error and this will be handled by the fallback controller.
  """
  @spec add_transaction(any(), nil | maybe_improper_list() | map()) :: any()
  def add_transaction(conn, params) do
    with %{"transaction" => tx} <- params,
         wrap? <- Map.get(params, "wrap", false),
         type <- String.to_existing_atom(params["transaction_type"]),
         {:ok, transaction} <- Base.decode64(tx),
         {:ok, :added} <-
           GRPCProxy.add_transaction(transaction, type, wrap?) do
      render(conn, "add_transaction.json")
    end
  end
end
