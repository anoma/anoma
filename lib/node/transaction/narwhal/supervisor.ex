defmodule Anoma.Node.Transaction.Narwhal.Supervisor do
  @moduledoc """
  I am the supervisor for the Narwhal consensus subsystem.

  I supervise Worker, Primary, and Bullshark (in that order --
  Worker must start first so it can receive batches before
  Primary proposes).

  I own the mnesia tables for block and batch storage.
  Blocks are written by Primary and read by Bullshark.
  Batches are written by Worker and read by Primary/Bullshark.

  Strategy is `:one_for_all` -- if any child crashes, restart
  all, because DAG state must be consistent across processes.
  """

  use Supervisor

  alias Anoma.Node.Transaction.Narwhal.Block
  alias Anoma.Node.Transaction.Narwhal.Bullshark
  alias Anoma.Node.Transaction.Narwhal.Config
  alias Anoma.Node.Transaction.Narwhal.Primary
  alias Anoma.Node.Transaction.Narwhal.Worker

  @type startup_options ::
          {:node_id, String.t()}
          | {:config, Config.t()}
          | {:batch_size, pos_integer()}

  ############################################################
  #                    Supervisor Lifecycle                  #
  ############################################################

  @spec start_link(list(startup_options())) :: Supervisor.on_start()
  def start_link(args) do
    Supervisor.start_link(__MODULE__, args)
  end

  @impl true
  def init(args) do
    Process.set_label(__MODULE__)

    node_id = Keyword.fetch!(args, :node_id)
    config = Keyword.fetch!(args, :config)
    batch_size = Keyword.get(args, :batch_size, 10)

    Anoma.Tables.create_table(
      blocks_table(node_id),
      [:block_digest, :block]
    )

    Anoma.Tables.create_table(
      batches_table(node_id),
      [:batch_digest, :batch]
    )

    children = [
      {Worker, node_id: node_id, batch_size: batch_size, config: config},
      {Primary, node_id: node_id, config: config},
      {Bullshark, node_id: node_id, config: config}
    ]

    Supervisor.init(children, strategy: :one_for_all)
  end

  ############################################################
  #                    Block Store API                      #
  ############################################################

  @spec blocks_table(String.t()) :: atom()
  def blocks_table(node_id) do
    Anoma.Tables.node_table_name(node_id, NarwhalBlocks)
  end

  @spec store_block(String.t(), binary(), Block.t()) :: :ok | :error
  def store_block(node_id, block_digest, block) do
    case :mnesia.transaction(fn ->
           :mnesia.write({blocks_table(node_id), block_digest, block})
         end) do
      {:atomic, :ok} -> :ok
      {:aborted, _} -> :error
    end
  end

  @spec get_block(String.t(), binary()) :: Block.t() | nil
  def get_block(node_id, block_digest) do
    case :mnesia.transaction(fn ->
           case :mnesia.read({blocks_table(node_id), block_digest}) do
             [{_, _, block}] -> block
             [] -> nil
           end
         end) do
      {:atomic, result} -> result
      {:aborted, _} -> nil
    end
  end

  ############################################################
  #                    Batch Store API                      #
  ############################################################

  @spec batches_table(String.t()) :: atom()
  def batches_table(node_id) do
    Anoma.Tables.node_table_name(node_id, NarwhalBatches)
  end

  @spec store_batch(String.t(), binary(), [binary()], %{binary() => term()}) ::
          :ok | :error
  def store_batch(node_id, digest, tx_ids, tx_data) do
    case :mnesia.transaction(fn ->
           table = batches_table(node_id)

           case :mnesia.read({table, digest}) do
             [_] -> :ok
             [] -> :mnesia.write({table, digest, {tx_ids, tx_data}})
           end
         end) do
      {:atomic, :ok} -> :ok
      {:aborted, _} -> :error
    end
  end

  @spec get_batch(String.t(), binary()) :: {:ok, [binary()]} | :absent
  def get_batch(node_id, digest) do
    case :mnesia.transaction(fn ->
           case :mnesia.read({batches_table(node_id), digest}) do
             [{_, _, {tx_ids, _}}] -> {:ok, tx_ids}
             [] -> :absent
           end
         end) do
      {:atomic, result} -> result
      {:aborted, _} -> :absent
    end
  end

  @spec get_full_batch(String.t(), binary()) ::
          {:ok, [binary()], %{binary() => term()}} | :absent
  def get_full_batch(node_id, digest) do
    case :mnesia.transaction(fn ->
           case :mnesia.read({batches_table(node_id), digest}) do
             [{_, _, {tx_ids, tx_data}}] -> {:ok, tx_ids, tx_data}
             [] -> :absent
           end
         end) do
      {:atomic, result} -> result
      {:aborted, _} -> :absent
    end
  end

  @spec has_batches?(String.t(), [binary()]) :: boolean()
  def has_batches?(node_id, digests) do
    case :mnesia.transaction(fn ->
           table = batches_table(node_id)

           Enum.all?(digests, fn d ->
             :mnesia.read({table, d}) != []
           end)
         end) do
      {:atomic, result} -> result
      {:aborted, _} -> false
    end
  end
end
