defmodule Anoma.Node.Transport.Messages do
  alias Anoma.Crypto.Id
  alias Anoma.Node.Indexer
  alias Anoma.Node.Registry
  alias Anoma.Node.Transaction.Intentpool
  alias Anoma.Node.Transaction.Mempool

  alias Protobufs.Announcement
  alias Protobufs.Enveloppe
  alias Protobufs.Indexer.Nullifiers
  alias Protobufs.Indexer.UnrevealedCommits
  alias Protobufs.Indexer.UnspentResources
  alias Protobufs.IntentPool.AddIntent
  alias Protobufs.IntentPool.ListIntents
  alias Protobufs.MemPool.Dump
  alias Protobufs.NodeInfo

  @doc """
  I create an announcement message for the given node id.
  """
  @spec announcement(Id.t()) :: Enveloppe.t()
  def announcement(node_id) do
    # get a list of all the local engines for this node.
    engines =
      Registry.engines_for(node_id)
      |> Enum.map(&Atom.to_string/1)

    announcement = %Announcement{
      node_info: node_info(node_id),
      engines: engines
    }

    %Enveloppe{inner_message: {:announcement, announcement}}
  end

  #############################################################################
  # Incoming Messages

  @doc """
  I translate a protobuf message into a request to the engine.

  I return the result of the call, encoded as a protobuf message.
  """

  # ----------------------------------------------------------------------------
  # Dump Mempool

  def proto_to_call(%Dump.Request{}, ref, local_node_id) do
    # call the engine
    result = ["dump1", "dump2"]

    # construct the reply message
    response = %Dump.Response{dumps: result}

    {:reply, wrap(response, :mempool_dump_response, local_node_id, ref)}
  end

  def proto_to_call(%Dump.Response{} = response, ref, _local_node_id) do
    # construct the reply message
    result = response.dumps
    {:is_reply, {:ok, result}, ref}
  end

  # ----------------------------------------------------------------------------
  # Add Intent

  def proto_to_call(%AddIntent.Request{}, ref, local_node_id) do
    # call the engine
    result = "intent added"

    # construct the reply message
    response = %AddIntent.Response{result: result}

    {:reply, wrap(response, :add_intent_response, local_node_id, ref)}
  end

  def proto_to_call(%AddIntent.Response{} = response, ref, _local_node_id) do
    # construct the reply message
    result = response.result
    {:is_reply, {:ok, result}, ref}
  end

  # ----------------------------------------------------------------------------
  # List Intents

  def proto_to_call(%ListIntents.Request{}, ref, local_node_id) do
    # call the engine
    result = ["intent1", "intent2"]

    # do the call to the intent pool here
    response = %ListIntents.Response{intents: result}

    {:reply, wrap(response, :list_intents_response, local_node_id, ref)}
  end

  def proto_to_call(%ListIntents.Response{} = response, ref, _local_node_id) do
    # construct the reply message
    {:is_reply, {:ok, response.intents}, ref}
  end

  # ----------------------------------------------------------------------------
  # Nullifiers

  def proto_to_call(%Nullifiers.Request{}, ref, local_node_id) do
    # call the engine
    result = ["nullifier1", "nullifier2"]

    # do the call to the intent pool here
    response = %Nullifiers.Response{nullifiers: result}

    {:reply, wrap(response, :nullifiers_response, local_node_id, ref)}
  end

  def proto_to_call(%Nullifiers.Response{} = response, ref, _local_node_id) do
    # construct the reply message
    {:is_reply, {:ok, response.nullifiers}, ref}
  end

  # ----------------------------------------------------------------------------
  # Unrevealed Commitments

  def proto_to_call(%UnrevealedCommits.Request{}, ref, local_node_id) do
    # call the engine
    result = ["commit1", "commit2"]

    # do the call to the intent pool here
    # call to engine for result
    response = %UnrevealedCommits.Response{commits: result}

    {:reply, wrap(response, :unrevealed_commits_response, local_node_id, ref)}
  end

  def proto_to_call(%UnrevealedCommits.Response{} = response, ref, _) do
    # construct the reply message
    {:is_reply, {:ok, response.commits}, ref}
  end

  # ----------------------------------------------------------------------------
  # Unspent Resources

  def proto_to_call(%UnspentResources.Request{}, ref, local_node_id) do
    # call the engine
    result = ["unspent resource 1", "unspent resource 2"]

    # do the call to the intent pool here
    # call to engine for result
    response = %UnspentResources.Response{unspent_resources: result}

    {:reply, wrap(response, :unspent_resources_response, local_node_id, ref)}
  end

  def proto_to_call(%UnspentResources.Response{} = response, ref, _) do
    # construct the reply message
    {:is_reply, {:ok, response.unspent_resources}, ref}
  end

  #############################################################################
  # Outgoing Messages

  @doc """
  I translate a request to an engine into a protobuf message.
  """

  # ----------------------------------------------------------------------------
  # Dump Mempool

  def call_to_proto(:dump, Mempool, local_node_id) do
    request = %Dump.Request{}

    wrap(request, :mempool_dump_request, local_node_id)
  end

  # ----------------------------------------------------------------------------
  # Add Intent

  def call_to_proto({:add_intent, _intent}, Intentpool, local_node_id) do
    request = %AddIntent.Request{}

    wrap(request, :add_intent_request, local_node_id)
  end

  # ----------------------------------------------------------------------------
  # List Intents

  def call_to_proto(:list_intents, Intentpool, local_node_id) do
    request = %ListIntents.Request{}

    wrap(request, :list_intents_request, local_node_id)
  end

  def call_to_proto(:list_nullifiers, Indexer, local_node_id) do
    request = %Nullifiers.Request{}

    wrap(request, :nullifiers_request, local_node_id)
  end

  def call_to_proto(:list_unrevealed_commitments, Indexer, local_node_id) do
    request = %UnrevealedCommits.Request{}

    wrap(request, :unrevealed_commits_request, local_node_id)
  end

  def call_to_proto(:list_unspent_resources, Indexer, local_node_id) do
    request = %UnspentResources.Request{}

    wrap(request, :unspent_resources_request, local_node_id)
  end

  ############################################################
  #                           Helpers                        #
  ############################################################
  # @doc """
  # I create a NodeInfo from a node id.
  # """
  defp node_info(node_id) do
    %NodeInfo{
      sign: node_id.external.sign,
      encrypt: node_id.external.encrypt
    }
  end

  # @doc """
  # Given a protobuf struct and a label, I wrap it into an Enveloppe.
  # If a ref is not provided, I generate one.
  # """
  defp wrap(message, label, node_id) do
    wrap(message, label, node_id, :erlang.term_to_binary(make_ref()))
  end

  defp wrap(message, label, node_id, ref) do
    %Enveloppe{
      message_id: ref,
      inner_message: {label, message},
      sender_info: node_info(node_id)
    }
  end
end
