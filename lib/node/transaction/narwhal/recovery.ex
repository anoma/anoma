defmodule Anoma.Node.Transaction.Narwhal.Recovery do
  @moduledoc """
  I am the network tier of Bullshark's recovery: I pull a block or batch
  this validator is missing from a peer over the transport. A fetch runs
  in its own spawned process so it never blocks the Bullshark GenServer,
  and on completion (success or failure) I cast `{:fetch_done, digest}`
  back to Bullshark, which clears the in-flight marker and retries.
  """

  alias Anoma.Node.Registry
  alias Anoma.Node.Transport.Proxy.TransportProtocol
  alias Anoma.Node.Transaction.Narwhal.{Block, Bullshark, Worker}
  alias Anoma.Node.Transaction.Narwhal.Supervisor, as: NarwhalSup

  @doc """
  I spawn an async task that fetches `digest` (a `:block` or `:batch`)
  from the given `candidates` (node_ids, most-likely holder first),
  trying each in turn until one returns the data. I store it locally on
  success and notify `my_node_id`'s Bullshark when done.
  """
  @spec spawn_fetch(String.t(), [String.t()], :block | :batch, binary()) ::
          pid()
  def spawn_fetch(my_node_id, candidates, kind, digest) do
    spawn(fn ->
      try do
        do_fetch(my_node_id, candidates, kind, digest)
      after
        # Always notify Bullshark so the digest leaves `in_flight` even
        # if the fetch crashed; otherwise its recovery would hang.
        Bullshark.fetch_done(my_node_id, digest)
      end
    end)
  end

  # Walk the candidate list, stopping at the first holder. On miss,
  # `:absent`, or transport error, try the next candidate.
  @spec do_fetch(String.t(), [String.t()], :block | :batch, binary()) :: :ok
  defp do_fetch(_my_node_id, [], _kind, _digest), do: :ok

  defp do_fetch(my_node_id, [peer | rest], :block, digest) do
    case call_peer(my_node_id, peer, {:fetch_block, digest}) do
      %Block{} = block ->
        NarwhalSup.store_block(my_node_id, digest, block)

      _ ->
        do_fetch(my_node_id, rest, :block, digest)
    end
  end

  defp do_fetch(my_node_id, [peer | rest], :batch, digest) do
    case call_peer(my_node_id, peer, {:fetch_batch, digest}) do
      {:ok, tx_ids, tx_data} ->
        Worker.store_fetched_batch(my_node_id, digest, tx_ids, tx_data)

      _ ->
        do_fetch(my_node_id, rest, :batch, digest)
    end
  end

  # I send a targeted request to `peer_node_id`'s Worker and return the
  # decoded reply, or `:error` on any transport failure.
  @spec call_peer(String.t(), String.t(), term()) :: term() | :error
  defp call_peer(my_node_id, peer_node_id, request) do
    case Registry.match(peer_node_id, TransportProtocol) do
      [tp | _] ->
        message = %{
          to: peer_node_id,
          from: my_node_id,
          engine: Worker,
          message: request
        }

        TransportProtocol.call(Registry.via(tp), message)

      [] ->
        :error
    end
  end
end
