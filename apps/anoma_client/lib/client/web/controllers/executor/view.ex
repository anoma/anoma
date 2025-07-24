defmodule Anoma.Client.Web.ExecutorJSON do
  def render("run_scry.json", %{result: result}) do
    result = result |> Noun.Jam.jam() |> Base.encode64()
    %{result: result}
  end

  def render("500.json", %{error: err}) do
    %{error: err}
  end
end
