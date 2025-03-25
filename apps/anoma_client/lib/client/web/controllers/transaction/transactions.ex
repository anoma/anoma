defmodule Anoma.Client.Web.TransactionController do
  use Anoma.Client.Web, :controller

  action_fallback(Anoma.Client.Web.FallbackController)

  alias Anoma.Client.Transactions

  @doc """
  I return a list of all intents from the remote node.
  The intents will be jammed nouns, base64 encoded.
  """
  def compose(conn, _params = %{"transactions" => transactions}) do
    with {:ok, transactions} <- decode_many(transactions),
         {:ok, composed} <- Transactions.compose(transactions) do
      render(conn, "composed.json", transaction: composed)
    else
      :error ->
        {:error, :invalid_transactions}

      e ->
        e
    end
  end

  def verify(conn, _params = %{"transaction" => transaction}) do
    with {:ok, transaction} <- Base.decode64(transaction),
         {:ok, valid?} <- Transactions.verify(transaction) do
      render(conn, "verified.json", valid?: valid?)
    else
      :error ->
        {:error, :invalid_transaction}
    end
  end

  ############################################################
  #                       Helpers                            #
  ############################################################
  # @doc """
  # Decodes a list of binaries, and returns :error if one of them does not decode properly.
  # This is the same api as Base.decode64/1, but for many binaries.
  # """
  @spec decode_many([binary()]) ::
          {:ok, [binary()]} | :error
  defp decode_many(inputs) do
    Enum.reduce_while(inputs, [], fn input, acc ->
      case Base.decode64(input) do
        {:ok, decoded} ->
          {:cont, [decoded | acc]}

        :error ->
          {:halt, :error}
      end
    end)
    |> case do
      xs when is_list(xs) ->
        {:ok, xs}

      _ ->
        :error
    end
  end
end
