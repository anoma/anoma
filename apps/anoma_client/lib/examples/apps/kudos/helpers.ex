defmodule Anoma.Client.Examples.Apps.Kudos.Helpers do
  use Anoma.Client.Web.ConnCase
  use EventBroker.WithSubscription

  alias Anoma.Client.Examples.Apps.Kudos
  alias Anoma.Node.Examples.EEvent
  alias Anoma.Node.Transaction.Mempool
  alias Anoma.RM.Transparent.Resource

  alias Noun.Jam

  require Anoma.Node.Examples.EEvent

  @doc """
  I expect a based64 encoded intent, submit it to the client.
  """
  @spec submit_intent(Kudos.t(), String.t()) :: :ok
  def submit_intent(kudos, intent) do
    payload = %{
      intent: intent
    }

    data =
      kudos.client.conn
      |> post(~p"/intents", payload)
      |> json_response(200)

    assert data == %{"message" => "intent added"}
    :ok
  end

  @doc """
  I return the list of intents in the intentpool in base64 encoded form.
  """
  @spec list_intents(Kudos.t()) :: [String.t()]
  def list_intents(kudos) do
    data =
      kudos.client.conn
      |> get(~p"/intents")
      |> json_response(200)

    assert Kernel.match?(%{"intents" => intents}, data)

    assert is_list(data["intents"])

    data["intents"]
  end

  @doc """
  I expect a based64 encoded transaction, submit it to the client, and then wait for the next block to be created.
  """
  @spec submit_transaction_and_confirm(Kudos.t(), String.t()) :: :ok
  def submit_transaction_and_confirm(kudos, transaction) do
    with_subscription [[]] do
      node_id = kudos.client.node.node_id

      payload = %{
        transaction: transaction,
        transaction_type: "transparent_resource"
      }

      # wait for a new block before submitting the new transaction. This
      # way I can make sure that the block event I see next is at least a few blocks after the one
      # that contains my transaction.
      %{body: %{body: %{round: current_block}}} =
        EEvent.wait_for_pattern(%Anoma.Node.Event{
          node_id: ^node_id,
          body: %Mempool.Events.BlockEvent{}
        })

      data =
        kudos.client.conn
        |> post(~p"/mempool/add", payload)
        |> json_response(200)

      assert data == %{"message" => "transaction added"}

      # wait for 3 new blocks to make sure the transaction is in.
      receive do
        %{body: %{body: %{round: round}, node_id: ^node_id}}
        when round > current_block + 1 ->
          :ok
      end
    end

    :ok
  end

  @doc """
  I wait for `n` blocks to be created before returning.
  """
  @spec wait_n_blocks(Kudos.t(), number()) :: :ok
  def wait_n_blocks(kudos, n) do
    with_subscription [[]] do
      node_id = kudos.client.node.node_id

      # wait for a new block before submitting the new transaction. This
      # way I can make sure that the block event I see next is at least a few blocks after the one
      # that contains my transaction.
      %{body: %{body: %{round: current_block}}} =
        EEvent.wait_for_pattern(%Anoma.Node.Event{
          node_id: ^node_id,
          body: %Mempool.Events.BlockEvent{}
        })

      # wait for 3 new blocks to make sure the transaction is in.
      receive do
        %{body: %{body: %{round: round}, node_id: ^node_id}}
        when round > current_block + n ->
          :ok
      end
    end

    :ok
  end

  @doc """
  I return a list of resources for the given user. A resource is a Resource
  struct as defined in apps/anoma_lib/lib/anoma/rm/transparent/resource.ex.
  """
  @spec resources_for(Kudos.t(), atom()) :: [Resource.t()]
  def resources_for(kudos, user) do
    # A resource can be identified by the public key of its owner. This key is
    # present in the resource under its nullifiercommitment. See
    # apps/anoma_lib/lib/anoma/rm/transparent/resource.ex
    #
    # To filter resources based on a user, we thus need that user their public key.
    #
    # The list of resources that I get back is a list of base64 encoded, jammed noun.s
    payload = %{
      filters: [
        %{owner: Base.encode64(Map.get(kudos.users, user).public_key)}
      ]
    }

    data =
      kudos.client.conn
      |> post(~p"/indexer/filter-resources", payload)
      |> json_response(200)

    # we expect a single resource to be present in the output
    resources = data["resources"]

    resources =
      Enum.map(resources, fn resource ->
        {:ok, resource} =
          resource
          |> Base.decode64!()
          |> Jam.cue!()
          |> Resource.from_noun()

        # assert the public key is the nullifiercommitment from the user
        assert resource.nullifierkeycommitment ==
                 Map.get(kudos.users, user).public_key

        resource
      end)

    resources
  end

  @doc """
  I return the latest root binary.
  """
  @spec get_latest_root(Kudos.t()) :: binary()
  def get_latest_root(kudos) do
    kudos.client.conn
    |> get(~p"/indexer/root")
    |> json_response(200)
    |> Map.get("root")
    |> Base.decode64!()
  end
end
