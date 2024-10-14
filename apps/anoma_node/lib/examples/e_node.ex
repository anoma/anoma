defmodule Anoma.Node.Examples.ENode do
  def start_node(node_id) do
    case Anoma.Supervisor.start_node(node_id: node_id) do
      {:ok, pid} ->
        pid

      {:error, {:already_started, pid}} ->
        pid
    end
  end

  def stop_node(node_id) do
    Supervisor.stop(node_id)
  end
end
