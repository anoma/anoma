defmodule AnomaDashboard.AnomaLive do
  use AnomaDashboard, :live_view

  alias Anoma.Node.Registry
  alias Anoma.Node.Events.IntentAddSuccess
  alias Anoma.Node.Transaction.Mempool

  def test() do
    alias Anoma.CairoResource.{LogicInstance, Transaction}
    alias Examples.ECairo.EAction

    priv_keys = <<3::256>>

    shielded_tx_1 =
      %Transaction{
        actions: [EAction.an_action()],
        delta: priv_keys
      }


    shielded_tx_2 =
      %Transaction{
        actions: [EAction.an_action_with_intents()],
        delta: priv_keys
      }


      {shielded_tx_1, shielded_tx_2}
    # composed =
    #   Noun.Jam.cue!(composed)
    #   |> Anoma.TransparentResource.Transaction.from_noun()
  end

  def mount(_params, _session, socket) do
    EventBroker.subscribe_me([])
    {:ok, assign(socket, nodes: list_nodes(), events: %{}, active_node: nil)}
  end

  defp list_nodes() do
    Registry.dump_register()
    |> Enum.filter(fn {addr, _, _} -> addr.engine == Mempool end)
    |> Enum.map(&elem(&1, 0))
  end

  def handle_event("inspect", %{"node" => node_id}, socket) do
    {:noreply,
     assign(
       socket,
       :active_node,
       inspect_node(node_id, socket.assigns.events)
     )}
  end

  def handle_info(e, socket) do
    events =
      socket.assigns.events
      |> Map.update(e.body.node_id, [e], &(&1 ++ [e]))

    if socket.assigns.active_node do

    {:noreply, assign(socket, [nodes: list_nodes(), events: events, active_node: inspect_node(socket.assigns.active_node.node_id, events)])}
  else
    {:noreply, assign(socket, [nodes: list_nodes(), events: events])}

    end
  end

  defp inspect_node(node_id, events) do
    mempool_size =
      Anoma.Node.Transaction.Mempool.tx_dump(node_id)
      |> Enum.count()

    intentpool_size =
      Anoma.Node.Intents.IntentPool.intents(node_id)
      |> Enum.count()

    events = Map.get(events, node_id, [])

    %{
      node_id: node_id,
      mempool_length: mempool_size,
      events: events,
      intentpool_length: intentpool_size
    }
  end
end
