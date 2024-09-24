defmodule Anoma.Node.Transaction.Backends do
  @moduledoc """
  Backend module.
  Support :kv, :ro, :rm, :cairo execution.
  """

  alias Anoma.Node.Transaction.Ordering

  import Nock
  require Noun
  require EventBroker.Event
  use TypedStruct

  @type backend() ::
          :debug_term_storage | {:debug_read_term, pid} | :debug_bloblike

  @type transaction() :: {backend(), Noun.t() | binary()}

  typedstruct enforce: true, module: CompleteEvent do
    field(:tx_id, integer())
    field(:result, :ok | :error)
  end

  def execute({backend, tx}, id)
      when Noun.is_noun_atom(tx) do
    case Nock.Cue.cue(tx) do
      {:ok, tx} ->
        execute({backend, tx}, id)

      :error ->
        worker_event(id, :error)
        Ordering.write({id, [{nil, nil}]})
    end
  end

  def execute({{:debug_read_term, pid}, tx}, id) do
    execute_kv_ro(tx, id, pid, &send_value/3)
  end

  def execute({:debug_term_storage, tx}, id) do
    execute_kv_ro(tx, id, nil, &store_value/3)
  end

  def execute({:debug_bloblike, tx}, id) do
    execute_kv_ro(tx, id, nil, &blob_store/3)
  end

  defp gate_call(tx_code, env, id) do
    with {:ok, ordered_tx} <- nock(tx_code, [10, [6, 1 | id], 0 | 1], env),
         {:ok, result} <- nock(ordered_tx, [9, 2, 0 | 1], env) do
      {:ok, result}
    else
      e -> e
    end
  end

  defp execute_kv_ro(tx_code, id, reply_to, process) do
    env = %Nock{}

    with {:ok, stage_2_tx} <- nock(tx_code, [9, 2, 0 | 1], env),
         {:ok, result} <- gate_call(stage_2_tx, env, id),
         :ok <- process.(id, result, reply_to) do
      worker_event(id, {:ok, result})
    else
      _e ->
        worker_event(id, :error)
        Ordering.write({id, [{nil, nil}]})
    end
  end

  defp send_value(id, result, reply_to) do
    # send the value to reply-to address and the topic
    reply_msg = {:read_value, result}
    send(reply_to, reply_msg)
    Ordering.write({id, [{nil, nil}]})
  end

  def blob_store(id, result, _reply_to) do
    key = :crypto.hash(:sha256, :erlang.term_to_binary(result))
    Ordering.write({id, [{key, result}]})
  end

  def store_value(id, result, _reply_to) do
    with {:ok, list} <- result |> Noun.list_nock_to_erlang_safe(),
         true <-
           Enum.all?(list, fn
             [_ | _] -> true
             _ -> false
           end) do
      Ordering.write({id, list |> Enum.map(fn [k | v] -> {k, v} end)})

      :ok
    else
      _ -> :error
    end
  end

  defp worker_event(id, result) do
    event =
      EventBroker.Event.new_with_body(%__MODULE__.CompleteEvent{
        tx_id: id,
        result: result
      })

    EventBroker.event(event)
  end
end
