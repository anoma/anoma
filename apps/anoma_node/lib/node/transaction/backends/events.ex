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
    - `:tx_result`          - VM execution result; either :error or an
                              {:ok, noun} tuple.
    """
    field(:tx_id, binary())
    field(:vm_result, Mempool.vm_result())
  end

  defimpl Jason.Encoder, for: ResultEvent do
    defp encode_maybe_noun(noun) when is_atom(noun) do
      noun
    end

    defp encode_maybe_noun({:ok, noun}) do
      encode_maybe_noun(noun)
    end

    defp encode_maybe_noun(noun) do
      with jammed <- Noun.Jam.jam(noun),
           encoded <- Base.encode64(jammed) do
        encoded
      end
    end

    def encode(%ResultEvent{} = event, opts) do
      with vm_result <- encode_maybe_noun(event.vm_result) do
        Jason.Encode.map(
          %{
            tx_id: event.tx_id,
            vm_result: vm_result
          },
          opts
        )
      end
    end
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

  defimpl Jason.Encoder, for: CompleteEvent do
    defp encode_maybe_noun(noun) when is_atom(noun) do
      noun
    end

    defp encode_maybe_noun({:ok, noun}) do
      encode_maybe_noun(noun)
    end

    defp encode_maybe_noun(noun) do
      with jammed <- Noun.Jam.jam(noun),
           encoded <- Base.encode64(jammed) do
        encoded
      end
    end

    def encode(%CompleteEvent{} = event, opts) do
      with tx_result <- encode_maybe_noun(event.tx_result) do
        Jason.Encode.map(
          %{
            tx_id: event.tx_id,
            tx_result: tx_result
          },
          opts
        )
      end
    end
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
end
