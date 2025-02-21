defmodule Anoma.Client.Web.MempoolController do
  use Anoma.Client.Web, :controller

  action_fallback(Anoma.Client.Web.FallbackController)

  alias Anoma.Client.Node.GRPCProxy

  @spec add_transaction(any(), nil | maybe_improper_list() | map()) :: any()
  @doc """
  I add an intent to the remote intent pool.

  I expect a jammed noun as paramter, base64 encoded.

  If anything goes wrong, I will return an error and this will be handled by the fallback controller.
  """
  def add_transaction(conn, params) do
    with %{"transaction" => tx} <- params,
         {:ok, transaction} <- Base.decode64(tx),
         {:ok, :added} <- GRPCProxy.add_transaction(transaction) do
      render(conn, "add_transaction.json")
    end
  end
end
