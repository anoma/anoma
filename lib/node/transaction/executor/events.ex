defmodule Anoma.Node.Transaction.Executor.Events do
  @moduledoc """
  I define all events that are being sent by the mempool.

  I also define the filters that can be used to subscribe to these events.
  """
  alias Anoma.Node.Transaction.Mempool

  use EventBroker.DefFilter
  use TypedStruct

  ############################################################
  #                           Events                         #
  ############################################################

  typedstruct enforce: true, module: ExecutionEvent do
    @typedoc """
    I am the type of an execution event.

    I am launched when transactions for a specific block have been
    succesfully processed by their respective workers.

    I hence signal the results with a message containing the result list.
    The order of the results should coincide with the ordering of the
    corresponding transactions.
    """

    field(:result, [Mempool.vm_result()], default: [])
  end

  typedstruct enforce: true, module: TaskCrash do
    @derive Jason.Encoder
    @typedoc """
    I am a crash event for a task that failed.
    """
    field(:task, pid())
  end

  ############################################################
  #                           Json Encoding                  #
  ############################################################

  defimpl Jason.Encoder, for: ExecutionEvent do
    @spec encode(ExecutionEvent.t(), Jason.Encode.opts()) :: iodata()
    def encode(event = %ExecutionEvent{}, opts) do
      event
      |> Map.update!(
        :result,
        &Enum.map(&1, fn result ->
          case result do
            {:error, id} ->
              %{result: "error", id: id}

            {{:ok, %Anoma.RM.Transparent.Transaction{} = tx}, id} ->
              %{result: tx, id: id}
          end
        end)
      )
      |> Jason.Encode.map(opts)
    end
  end
end
