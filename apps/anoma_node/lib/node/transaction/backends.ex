defmodule Anoma.Node.Transaction.Backends do
  @moduledoc """
  Backend module.
  Support :kv, :ro, :rm, :cairo execution.
  """

  alias Anoma.Node.Transaction.Ordering
  alias Anoma.TransparentResource

  import Nock
  require Noun
  require EventBroker.Event
  use TypedStruct

  @type backend() ::
          :debug_term_storage
          | {:debug_read_term, pid}
          | :debug_bloblike
          | :transparent_resource

  @type transaction() :: {backend(), Noun.t() | binary()}

  typedstruct enforce: true, module: ResultEvent do
    field(:tx_id, integer())
    field(:vm_result, {:ok, Noun.t()} | :error)
  end

  typedstruct enforce: true, module: CompleteEvent do
    field(:tx_id, integer())
    field(:tx_result, {:ok, any()} | :error)
  end

  @spec execute(String.t(), {backend(), Noun.t()}, binary()) :: :ok
  def execute(node_id, {backend, tx}, id)
      when Noun.is_noun_atom(tx) do
    case Nock.Cue.cue(tx) do
      {:ok, tx} ->
        execute(node_id, {backend, tx}, id)

      :error ->
        error_handle(node_id, id)
    end
  end

  def execute(node_id, {{:debug_read_term, pid}, tx}, id) do
    execute_candidate(node_id, tx, id, fn x, y ->
      send_value(node_id, x, y, pid)
    end)
  end

  def execute(node_id, {:debug_term_storage, tx}, id) do
    execute_candidate(node_id, tx, id, &store_value(node_id, &1, &2))
  end

  def execute(node_id, {:debug_bloblike, tx}, id) do
    execute_candidate(node_id, tx, id, &store_value(node_id, &1, &2))
  end

  def execute(node_id, {:transparent_resource, tx}, id) do
    execute_candidate(
      node_id,
      tx,
      id,
      &transparent_resource_tx(node_id, &1, &2)
    )
  end

  @spec gate_call(Noun.t(), Nock.t(), binary()) :: {:ok, Noun.t()} | :vm_error
  defp gate_call(tx_code, env, id) do
    with {:ok, stage_2_tx} <- nock(tx_code, [9, 2, 0 | 1], env),
         {:ok, ordered_tx} <- nock(stage_2_tx, [10, [6, 1 | id], 0 | 1], env),
         {:ok, result} <- nock(ordered_tx, [9, 2, 0 | 1], env) do
      res = {:ok, result}
      result_event(id, res, env.node_id)
      res
    else
      _e -> :vm_error
    end
  end

  @spec execute_candidate(node_id, Noun.t(), id, process) :: :ok
        when id: binary(),
             node_id: String.t(),
             process: (id, Noun.t() -> :ok | :error)
  defp execute_candidate(node_id, tx_code, id, process) do
    env = %Nock{node_id: node_id}

    with {:ok, result} <- gate_call(tx_code, env, id),
         :ok <- process.(id, result) do
      :ok
    else
      :vm_error ->
        error_handle(node_id, id)

      _e ->
        Ordering.write(node_id, {id, []})
        complete_event(id, :error)
    end
  end

  @spec transparent_resource_tx(String.t(), binary(), Noun.t()) ::
          :ok | :error
  defp transparent_resource_tx(node_id, id, result) do
    with {:ok, tx} <- TransparentResource.Transaction.from_noun(result),
         true <- TransparentResource.Transaction.verify(tx) do
      for action <- tx.actions do
        cms = action.commitments
        nfs = action.nullifiers

        Ordering.append(
          node_id,
          {id, [{:nullifiers, nfs}, {:commitments, cms}]}
        )
      end
    else
      _e -> :error
    end
  end

  @spec send_value(String.t(), binary(), Noun.t(), pid()) :: :ok | :error
  defp send_value(node_id, id, result, reply_to) do
    # send the value to reply-to address and the topic
    reply_msg = {:read_value, result}
    send(reply_to, reply_msg)
    Ordering.write(node_id, {id, []})
    complete_event(id, {:ok, reply_msg})
  end

  @spec blob_store(String.t(), binary(), Noun.t()) :: :ok | :error
  def blob_store(node_id, id, result) do
    key = :crypto.hash(:sha256, :erlang.term_to_binary(result))
    Ordering.write(node_id, {id, [{key, result}]})
    complete_event(id, {:ok, key})
  end

  @spec store_value(String.t(), binary(), Noun.t()) :: :ok | :error
  def store_value(node_id, id, result) do
    with {:ok, list} <- result |> Noun.list_nock_to_erlang_safe(),
         true <-
           Enum.all?(list, fn
             [_ | _] -> true
             _ -> false
           end) do
      Ordering.write(
        node_id,
        {id, list |> Enum.map(fn [k | v] -> {k, v} end)}
      )

      complete_event(id, {:ok, list})
    else
      _ -> :error
    end
  end

  @spec error_handle(String.t(), binary()) :: :ok
  defp error_handle(node_id, id) do
    result_event(id, :error, node_id)
    Ordering.write(node_id, {id, []})
    complete_event(id, :error)
  end

  @spec complete_event(String.t(), :error | {:ok, any()}) :: :ok
  defp complete_event(id, result) do
    event =
      EventBroker.Event.new_with_body(%__MODULE__.CompleteEvent{
        tx_id: id,
        tx_result: result
      })

    EventBroker.event(event)
  end

  @spec result_event(String.t(), any(), binary()) :: :ok
  defp result_event(id, result, _node_id) do
    event =
      EventBroker.Event.new_with_body(%__MODULE__.ResultEvent{
        tx_id: id,
        vm_result: result
      })

    EventBroker.event(event)
  end
end
