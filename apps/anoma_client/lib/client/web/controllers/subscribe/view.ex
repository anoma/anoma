defmodule Anoma.Client.Web.SubscribeJSON do
  def render("subscribed.json", _params) do
    %{result: "subscribed"}
  end
end
