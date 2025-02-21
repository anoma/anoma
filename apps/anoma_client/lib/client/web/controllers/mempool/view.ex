defmodule Anoma.Client.Web.MempoolJSON do
  def render("add_transaction.json", _assigns) do
    %{message: "transaction added"}
  end

  def render("500.json", %{error: err}) do
    %{error: err}
  end
end
