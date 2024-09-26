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
        :error
    end
  end

  def execute({{:debug_read_term, pid}, tx}, id) do
    execute_candidate(tx, id, fn x, y -> send_value(x, y, pid) end)
  end

  def execute({:debug_term_storage, tx}, id) do
    execute_candidate(tx, id, &store_value/2)
  end

  def execute({:debug_bloblike, tx}, id) do
    execute_candidate(tx, id, &blob_store/2)
  end

  defp gate_call(tx_code, env, id) do
    with {:ok, stage_2_tx} <- nock(tx_code, [9, 2, 0 | 1], env),
         {:ok, ordered_tx} <- nock(stage_2_tx, [10, [6, 1 | id], 0 | 1], env),
         {:ok, result} <- nock(ordered_tx, [9, 2, 0 | 1], env) do
      {:ok, result}
    else
      e -> e
    end
  end

  defp execute_candidate(tx_code, id, process) do
    env = %Nock{}

    with {:ok, result} <- gate_call(tx_code, env, id),
         :ok <- process.(id, result) do
      ok_event =
        EventBroker.Event.new_with_body(%__MODULE__.CompleteEvent{
          tx_id: id,
          result: {:ok, result}
        })

      EventBroker.event(ok_event)
    else
      _e ->
        error_event =
          EventBroker.Event.new_with_body(%__MODULE__.CompleteEvent{
            tx_id: id,
            result: :error
          })

        EventBroker.event(error_event)
    end
  end

  defp resource_tx(id, result) do
    with {:ok, tx} <- TransparentResource.Resource.from_noun(result),
         true <- TransparentResource.verify(tx) do
      for action <- tx.actions do
        cms = action.commitments
        nlfs = action.nullifiers
        Ordering.append({id, [{:nullifiers, nlfs}, {:commitments, cms}]})
      end
    else
      _e -> :error
    end
  end

  defp send_value(id, result, reply_to) do
    # send the value to reply-to address and the topic
    reply_msg = {:read_value, result}
    Ordering.write({id, [{nil, nil}]})
    send(reply_to, reply_msg)
  end

  def blob_store(id, result) do
    key = :crypto.hash(:sha256, :erlang.term_to_binary(result))
    Ordering.write({id, [{key, result}]})
  end

  def store_value(id, result) do
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
end
