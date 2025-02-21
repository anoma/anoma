defmodule Anoma.Client.Web.FallbackController do
  use Phoenix.Controller

  def call(conn, {:error, :failed_to_fetch_intents}) do
    conn
    |> put_status(503)
    |> put_view(json: Anoma.Client.Web.ErrorJSON)
    |> json(%{error: "failed to fetch intents"})
  end

  def call(conn, {:error, :add_intent_failed, err}) do
    conn
    |> put_status(503)
    |> put_view(json: Anoma.Client.Web.ErrorJSON)
    |> json(%{error: "failed to add intent", reason: err})
  end

  def call(conn, {:error, :add_transaction_failed, err}) do
    conn
    |> put_status(503)
    |> put_view(json: Anoma.Client.Web.ErrorJSON)
    |> json(%{error: "failed to add transaction", reason: err})
  end

  # happens during a call to /transactions/verify
  # typically invalid binary encoding data
  def call(conn, {:error, :invalid_transaction}) do
    conn
    |> put_status(503)
    |> put_view(json: Anoma.Client.Web.ErrorJSON)
    |> json(%{error: "failed to verify transaction. is the binary valid?"})
  end

  # happens during a call to /transactions/compose
  # typically invalid binary encoding data
  def call(conn, {:error, :invalid_transactions}) do
    conn
    |> put_status(503)
    |> put_view(json: Anoma.Client.Web.ErrorJSON)
    |> json(%{
      error: "failed to compose transactions. are the binaries valid?"
    })
  end

  # happens during a call to /transactions/compose
  # when there are not enough transactions to comose (i.e., 0 or 1)
  def call(conn, {:error, :not_enough_transactions}) do
    conn
    |> put_status(503)
    |> put_view(json: Anoma.Client.Web.ErrorJSON)
    |> json(%{
      error: "failed to compose transactions. not enough transactions"
    })
  end

  # happens during a call to /transactions/compose
  # one of the inputs was not a valid transaction or noun
  def call(conn, {:error, :invalid_input, err}) do
    conn
    |> put_status(503)
    |> put_view(json: Anoma.Client.Web.ErrorJSON)
    |> json(%{
      error:
        "failed to compose the transactions. are all transactions valid?",
      reason: err
    })
  end

  def call(conn, _err) do
    conn
    |> put_status(503)
    |> put_view(json: Anoma.Client.Web.ErrorJSON)
    |> json(%{error: "unknown error"})
  end
end
