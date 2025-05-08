defmodule Anoma.LocalDomain.Web.SubscribeJSON do
  def render("subscribed.json", _params) do
    %{message: "subscribed"}
  end
end
