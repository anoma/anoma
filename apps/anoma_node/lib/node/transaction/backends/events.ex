defmodule Anoma.Node.Transaction.Backends.Events do
  @moduledoc """
  I define all events that are being sent by the backend.

  I also define the filters that can be used to subscribe to these events.
  """
  alias Anoma.Node.Event
  alias Anoma.Node.Transaction.Executor
  alias Anoma.Node.Transaction.Mempool

  use EventBroker.DefFilter
  use TypedStruct
  ############################################################
  #                           Events                         #
  ############################################################

  typedstruct enforce: true, module: ResultEvent do
    @typedoc """
    I hold the content of the Result Event, which conveys the result of
    the transaction candidate code execution on the Anoma VM to
    the Mempool engine.

    ### Fields
    - `:tx_id`              - The transaction id.
    - `:vm_result`          - VM execution result; either :error or an
                              {:ok, noun} tuple.
    """
    field(:tx_id, binary())
    field(:vm_result, Mempool.vm_result())
  end

  typedstruct enforce: true, module: CompleteEvent do
    @typedoc """
    I hold the content of the Complete Event, which communicates the result
    of the transaction candidate execution to the Executor engine.

    ### Fields
    - `:tx_id`              - The transaction id.
    - `:tx_result`          - Execution result; either :error or an
                              {:ok, value} tuple.
    """
    field(:tx_id, binary())
    field(:tx_result, Mempool.tx_result())
  end

  typedstruct enforce: true, module: TRMEvent do
    @derive Jason.Encoder
    @typedoc """
    I hold the content of the The Resource Machine Event, which
    communicates a set of nullifiers/commitments defined by the actions of the
    transaction candidate to the Intent Pool.

    ### Fields

    - `:commitments`        - The set of commitments.
    - `:nullifiers`         - The set of nullifiers.
    - `:commitments`        - The set of commitments.
    """
    field(:commitments, MapSet.t(binary()), default: MapSet.new())
    field(:nullifiers, MapSet.t(binary()), default: MapSet.new())
  end

  # todo: where to put this?
  defimpl Jason.Encoder, for: MapSet do
    def encode(mapset, opts) do
      Jason.Encode.list(Enum.into(mapset, []), opts)
    end
  end

  typedstruct enforce: true, module: SRMEvent do
    @derive Jason.Encoder
    @typedoc """
    I hold the content of the The Shielded Resource Machine Event, which
    communicates a set of nullifiers/commitments defined by the actions of the
    transaction candidate to the Intent Pool.

    ### Fields

    - `:commitments`        - The set of commitments.
    - `:nullifiers`         - The set of nullifiers.
    """
    field(:commitments, MapSet.t(binary()), default: MapSet.new())
    field(:nullifiers, MapSet.t(binary()), default: MapSet.new())
  end

  ############################################################
  #                           Filters                        #
  ############################################################

  deffilter CompleteFilter do
    %EventBroker.Event{body: %Event{body: %CompleteEvent{}}} ->
      true

    _ ->
      false
  end

  deffilter ForMempoolFilter do
    %EventBroker.Event{body: %Event{body: %ResultEvent{}}} ->
      true

    _ ->
      false
  end

  deffilter ForMempoolExecutionFilter do
    %EventBroker.Event{body: %Event{body: %Executor.Events.ExecutionEvent{}}} ->
      true

    _ ->
      false
  end

  ############################################################
  #                           Json Encoding                  #
  ############################################################

  defimpl Jason.Encoder, for: CompleteEvent do
    def encode(%CompleteEvent{} = event, opts) do
      event
      |> Map.update!(:tx_result, fn
        :error ->
          "error"

        {:ok, %Anoma.RM.Transparent.Transaction{} = tx} ->
          tx

        {:ok, noun} ->
          noun
          |> Noun.Jam.jam()
          |> Base.encode64()
      end)
      |> Map.drop([:__struct__])
      |> Jason.Encode.map(opts)
    end
  end

  defimpl Jason.Encoder, for: ResultEvent do
    def encode(%ResultEvent{} = event, opts) do
      event
      |> Map.update!(:vm_result, fn
        :vm_error ->
          "error"

        {:ok, %Anoma.RM.Transparent.Transaction{} = tx} ->
          tx

        {:ok, noun} ->
          noun
          |> Noun.Jam.jam()
          |> Base.encode64()
      end)
      |> Map.drop([:__struct__])
      |> Jason.Encode.map(opts)
    end
  end
end
