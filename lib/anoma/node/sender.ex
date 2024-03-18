defmodule Anoma.Node.Proxy do
  use TypedStruct
  alias Anoma.Node.Router
  require Logger

  def init(router) do
    {:ok, router}
  end

  def handle_call({:call, addr, msg}, _, router) do {:reply, Router.call(addr, msg), router} end
  def handle_cast({:cast, addr, msg}, _, router) do
    Router.cast(addr, msg)
    {:noreply, router}
  end
end
