defmodule Anoma.Node.Events do
  alias Anoma.Node.Event
  alias Anoma.Node.Logging
  alias Anoma.Node.Transaction.Backends
  alias Anoma.Node.Transaction.Mempool
  alias Anoma.Node.Transaction.Storage
  alias Anoma.RM.Intent
  alias Anoma.RM.Transparent.Transaction

  use TypedStruct

  require Anoma.Node.Event

  @type transaction_result :: {{:ok, any()}, binary()} | {:error, binary()}

  ############################################################
  #                       Events                             #
  ############################################################

  typedstruct enforce: true, module: ExecutionEvent do
    @typedoc """
    I am the type of an execution event.

    I am launched when transactions for a specific block have been succesfully
    processed by their respective workers.

    I hence signal the results with a message containing the result list. The
    order of the results should coincide with the ordering of the corresponding
    transactions.
    """

    field(:result, [Anoma.Node.Events.transaction_result()])
  end

  typedstruct module: TxEvent do
    @typedoc """
    I am the type of a transaction event.

    I am sent upon a launch of a transaction, signaling that a specific
    transaction has been launched.

    ### Fileds

    - `:id` - The ID of a launched transaction.
    - `:tx` - The transaction info as stored in Mempool state.
    """

    field(:id, binary())
    field(:tx, Mempool.Tx.t())
  end

  typedstruct enforce: true, module: IntentAddSuccess do
    @typedoc """
    I am an event specifying that an intent has been submitted succesfully.

    ### Fields
    - `:intent` - The intent added.
    """
    field(:intent, Intent.t())
  end

  typedstruct enforce: true, module: IntentAddError do
    @typedoc """
    I am an event specifying that an intent submission has failed alongside with
    a reason.

    ### Fields
    - `:intent` - The intent submitted.
    - `:reason` - The reason why it was rejected from the pool.
    """
    field(:intent, Intent.t())
    field(:reason, String.t())
  end

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
    field(:vm_result, {:ok, Noun.t()} | :error)
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
    field(:tx_result, {:ok, any()} | :error)
  end

  typedstruct enforce: true, module: TRMEvent do
    @typedoc """
    I hold the content of the The Resource Machine Event, which
    communicates a set of nullifiers/commitments defined by the actions of the
    transaction candidate to the Intent Pool.

    ### Fields

    - `:commitments`        - The set of commitments.
    - `:nullifiers`         - The set of nullifiers.
    - `:commitments`        - The set of commitments.
    """
    field(:commitments, MapSet.t(binary()))
    field(:nullifiers, MapSet.t(binary()))
  end

  typedstruct enforce: true, module: SRMEvent do
    @typedoc """
    I hold the content of the The Shielded Resource Machine Event, which
    communicates a set of nullifiers/commitments defined by the actions of the
    transaction candidate to the Intent Pool.

    ### Fields

    - `:commitments`        - The set of commitments.
    - `:nullifiers`         - The set of nullifiers.
    """
    field(:commitments, MapSet.t(binary()))
    field(:nullifiers, MapSet.t(binary()))
  end

  typedstruct module: LoggingEvent do
    @typedoc """
    I am the type of a logging event.

    I specify the format of any logging message sent.

    ### Fields

    - `:flag` - The level at which the event ought to be logged.
    - `:msg` - A logging message.
    """

    field(:flag, Logging.flag())
    field(:msg, binary())
  end

  typedstruct module: ConsensusEvent do
    @typedoc """
    I am the type of a consensus event.

    I am sent upon receiving a consensus, signaling that ordering has been
    assigned to a specific subset of pending transactions.

    ### Fileds

    - `:order` - The list of transaction IDs in apporpriate consensus
                 specified order.
    """

    field(:order, list(binary()))
  end

  typedstruct module: BlockEvent do
    @typedoc """
    I am the type of a block execition event.

    I am sent upon a completion of all transactions submitted by consensus
    and subsequent creation of a table-backed block.

    ### Fileds

    - `:order` - The consensus info executed, a list of transaction IDs.
    - `:round` - The block number committed.
    """

    field(:order, list(binary()))
    field(:round, non_neg_integer())
  end

  typedstruct enforce: true, module: OrderEvent do
    @typedoc """
    I am the type of an ordering Event.

    I am sent whenever the transaction with which I am associated gets a
    global timestamp.

    ### Fields

    - `tx_id` - The ID of the transaction which was ordered.
    """

    field(:tx_id, binary())
  end

  typedstruct enforce: true, module: WriteEvent do
    @typedoc """
    I am the type of a write event.

    I am sent whenever something has been written at a particular height.

    ### Fields

    - `:height` - The height at which something was just written.
    - `:writes` - A list of tuples {key, value}
    """

    field(:height, non_neg_integer())
    field(:writes, list({Anoma.Node.Transaction.Storage.bare_key(), term()}))
  end

  ############################################################
  #                       Event Fire Functions               #
  ############################################################

  @doc """
  I launch an execution event. See `ExecutionEvent` for more details.
  """
  @spec execution_event([transaction_result], String.t(), atom()) :: :ok
  def execution_event(results, node_id, module \\ __MODULE__) do
    %EventBroker.Event{
      source_module: module,
      body: %Event{
        node_id: node_id,
        body: %ExecutionEvent{
          result: results
        }
      }
    }
    |> EventBroker.event()
  end

  @doc """
  I launch a transaction event. See `TxEvent` for more details.
  """
  @spec transaction_event(
          Backends.transaction(),
          binary(),
          String.t(),
          atom()
        ) :: :ok
  def transaction_event(tx_tuple, tx_id, node_id, module \\ __MODULE__) do
    {backend, code} = tx_tuple

    %EventBroker.Event{
      source_module: module,
      body: %Event{
        node_id: node_id,
        body: %TxEvent{
          id: tx_id,
          tx: %Mempool.Tx{backend: backend, code: code}
        }
      }
    }
    |> EventBroker.event()
  end

  @doc """
  I fire an `IntentAddSuccess` event. See `IntentAddSuccess` for more
  information.
  """
  @spec intent_add_success(Transaction.t(), String.t(), atom()) :: :ok
  def intent_add_success(intent, node_id, module \\ __MODULE__) do
    %EventBroker.Event{
      source_module: module,
      body: %Event{
        node_id: node_id,
        body: %IntentAddSuccess{
          intent: intent
        }
      }
    }
    |> EventBroker.event()
  end

  @doc """
  I fire an `IntentAddError` event. See `IntentAddError` for more
  information.
  """
  @spec intent_add_error(Transaction.t(), any(), String.t(), atom()) :: :ok
  def intent_add_error(intent, reason, node_id, module \\ __MODULE__) do
    %EventBroker.Event{
      source_module: module,
      body: %Event{
        node_id: node_id,
        body: %IntentAddError{
          reason: reason,
          intent: intent
        }
      }
    }
    |> EventBroker.event()
  end

  @doc """
  I fire a `CompleteEvent`. See `CompleteEvent` for more information.
  """
  @spec transaction_complete(
          binary(),
          Mempool.tx_result(),
          String.t(),
          atom()
        ) :: :ok
  def transaction_complete(tx_id, tx_result, node_id, module \\ __MODULE__) do
    %EventBroker.Event{
      source_module: module,
      body: %Event{
        node_id: node_id,
        body: %CompleteEvent{
          tx_id: tx_id,
          tx_result: tx_result
        }
      }
    }
    |> EventBroker.event()
  end

  @doc """
  I fire a `ResultEvent`. See `ResultEvent` for more information.
  """
  @spec transaction_result(
          binary(),
          Mempool.tx_result(),
          String.t(),
          atom()
        ) :: :ok
  def transaction_result(tx_id, vm_result, node_id, module \\ __MODULE__) do
    %EventBroker.Event{
      source_module: module,
      body: %Event{
        node_id: node_id,
        body: %ResultEvent{
          tx_id: tx_id,
          vm_result: vm_result
        }
      }
    }
    |> EventBroker.event()
  end

  @doc """
  I fire a `TRMEvent`. See `TRMEvent` for more information.
  """
  @spec trm_event(MapSet.t(binary()), MapSet.t(binary()), String.t(), atom()) ::
          :ok
  def trm_event(commitments, nullifiers, node_id, module \\ __MODULE__) do
    %EventBroker.Event{
      source_module: module,
      body: %Event{
        node_id: node_id,
        body: %TRMEvent{
          commitments: commitments,
          nullifiers: nullifiers
        }
      }
    }
    |> EventBroker.event()
  end

  @doc """
  I fire a `SRMEvent`. See `SRMEvent` for more information.
  """
  @spec srm_event(MapSet.t(binary()), MapSet.t(binary()), String.t(), atom()) ::
          :ok
  def srm_event(commitments, nullifiers, node_id, module \\ __MODULE__) do
    %EventBroker.Event{
      source_module: module,
      body: %Event{
        node_id: node_id,
        body: %SRMEvent{
          commitments: commitments,
          nullifiers: nullifiers
        }
      }
    }
    |> EventBroker.event()
  end

  @doc """
  I fire a `BlockEvent`. See `BlockEvent` for more information.
  """
  @spec block_event([binary()], non_neg_integer(), String.t(), atom()) :: :ok
  def block_event(transaction_ids, round, node_id, module \\ __MODULE__) do
    %EventBroker.Event{
      source_module: module,
      body: %Event{
        node_id: node_id,
        body: %BlockEvent{
          order: transaction_ids,
          round: round
        }
      }
    }
    |> EventBroker.event()
  end

  @doc """
  I fire a `ConsensusEvent`. See `ConsensusEvent` for more information.
  """
  @spec consensus_event([binary()], String.t()) :: :ok
  def consensus_event(transaction_ids, node_id, module \\ __MODULE__) do
    %EventBroker.Event{
      source_module: module,
      body: %Event{
        node_id: node_id,
        body: %ConsensusEvent{
          order: transaction_ids
        }
      }
    }
    |> EventBroker.event()
  end

  @doc """
  I fire a `OrderEvent`. See `OrderEvent` for more information.
  """
  @spec order_event(binary(), String.t(), atom()) :: :ok
  def order_event(transaction_id, node_id, module \\ __MODULE__) do
    %EventBroker.Event{
      source_module: module,
      body: %Event{
        node_id: node_id,
        body: %OrderEvent{
          tx_id: transaction_id
        }
      }
    }
    |> EventBroker.event()
  end

  @doc """
  I fire a `LoggingEvent`. See `LoggingEvent` for more information.
  """
  @spec logging_event(Logging.flag(), binary(), String.t(), atom()) :: :ok
  def logging_event(flag, message, node_id, module \\ __MODULE__) do
    %EventBroker.Event{
      source_module: module,
      body: %Event{
        node_id: node_id,
        body: %LoggingEvent{
          flag: flag,
          msg: message
        }
      }
    }
    |> EventBroker.event()
  end

  @doc """
  I fire a `WriteEvent`. See `WriteEvent` for more information.
  """
  @spec write_event(non_neg_integer(), [Storage.event_write()], String.t()) ::
          :ok
  def write_event(height, event_writes, node_id, module \\ __MODULE__) do
    %EventBroker.Event{
      source_module: module,
      body: %Event{
        node_id: node_id,
        body: %WriteEvent{
          height: height,
          writes: event_writes
        }
      }
    }
    |> EventBroker.event()
  end

  @doc """
  I send a generic event to the event broker. My message can be any erlang term.
  """
  @spec generic_event(any(), String.t()) :: :ok
  def generic_event(payload, node_id, module \\ __MODULE__) do
    %EventBroker.Event{
      source_module: module,
      body: %Event{
        node_id: node_id,
        body: payload
      }
    }
    |> EventBroker.event()
  end
end
