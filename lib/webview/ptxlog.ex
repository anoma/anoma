defmodule Webview.PtxLog do
  use Phoenix.LiveView
  def render(assigns) do
    ~H"""
    <ul>
      <li :for={ptx <- @ptxes}><%= ptx %></li>
    </ul>
    """
  end

  def handle_params(_params, _uri, socket) do
    Webview.Endpoint.subscribe("ptxes")
    {:noreply, socket}
  end
  def handle_info(%{event: "new_ptx", payload: %{name: name}}, socket) do
    {:noreply, update(socket, :ptxes, &([name | &1]))}
  end
  def mount(_params, _session, socket) do
    {:ok, assign(socket, :ptxes, [])}
  end
end
