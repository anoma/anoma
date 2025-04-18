defmodule Anoma.Client.Examples.EClient.Indexer do
  @moduledoc """
  I contain functions to test the public interface of the client.

  I show how to use the intents endpoint.
  """
  use Anoma.Client.Web.ConnCase

  use TypedStruct

  alias Anoma.Client.Examples.EClient
  alias Anoma.Node.Examples.EIndexer
  alias Anoma.RM.Transparent.Resource
  alias Anoma.Node.Utility.Indexer

  import ExUnit.Assertions
  import Anoma.Client.Examples.EClient

  @doc """
  List nullifiers on a blank node is the empty list.
  """
  @spec list_nullifiers_empty(EClient.t()) :: EClient.t()
  def list_nullifiers_empty(client \\ setup()) do
    data =
      client.conn
      |> get(~p"/indexer/nullifiers")
      |> json_response(200)

    assert data == %{"nullifiers" => []}

    client
  end

  @doc """
  List nullifiers returns a list of nullifiers.
  """
  @spec list_nullifiers(EClient.t()) :: EClient.t()
  def list_nullifiers(client \\ setup()) do
    # Create a nullifier in the indexer
    EIndexer.indexer_reads_nullifier(client.node.node_id)

    # expected nullifier
    expected_nullifier =
      Resource.nullifier_hash(<<0::256>>, %Resource{})
      |> Noun.atom_integer_to_binary()
      |> Base.encode64()

    data =
      client.conn
      |> get(~p"/indexer/nullifiers")
      |> json_response(200)

    assert data == %{"nullifiers" => [expected_nullifier]}

    client
  end

  @doc """
  List nullifiers on a blank node is the empty list.
  """
  @spec list_unrevealed_commits_empty(EClient.t()) :: EClient.t()
  def list_unrevealed_commits_empty(client \\ setup()) do
    data =
      client.conn
      |> get(~p"/indexer/unrevealed-commits")
      |> json_response(200)

    assert data == %{"commits" => []}

    client
  end

  @doc """
  List nullifiers on a blank node is the empty list.
  """
  @spec list_unrevealed_commits(EClient.t()) :: EClient.t()
  def list_unrevealed_commits(client \\ setup()) do
    # Create an unrevealed commit using another example
    EIndexer.indexer_reads_unrevealed(client.node.node_id)

    # expected commits
    expected_commits =
      Indexer.get(client.node.node_id, :unrevealed)
      |> Enum.to_list()
      |> Enum.map(&Base.encode64/1)

    data =
      client.conn
      |> get(~p"/indexer/unrevealed-commits")
      |> json_response(200)

    assert data == %{"commits" => expected_commits}

    client
  end

  @doc """
  List nullifiers on a blank node is the empty list.
  """
  @spec list_commits_empty(EClient.t()) :: EClient.t()
  def list_commits_empty(client \\ setup()) do
    data =
      client.conn
      |> get(~p"/indexer/commits")
      |> json_response(200)

    assert data == %{"commits" => []}

    client
  end

  @doc """
  List nullifiers on a blank node is the empty list.
  """
  @spec list_commits(EClient.t()) :: EClient.t()
  def list_commits(client \\ setup()) do
    # Create an unrevealed commit using another example
    EIndexer.indexer_reads_unrevealed(client.node.node_id)

    # expected commits
    expected_commits = Indexer.get(client.node.node_id, :cms) |> Enum.into([])

    data =
      client.conn
      |> get(~p"/indexer/commits")
      |> json_response(200)

    assert data == %{"commits" => expected_commits}

    client
  end

  @doc """
  List nullifiers on a blank node is the empty list.
  """
  @spec list_unspent_resources_empty(EClient.t()) :: EClient.t()
  def list_unspent_resources_empty(client \\ setup()) do
    data =
      client.conn
      |> get(~p"/indexer/unspent-resources")
      |> json_response(200)

    assert data == %{"unspent_resources" => []}

    client
  end

  @doc """
  List nullifiers on a blank node is the empty list.
  """
  @spec list_unspent_resources(EClient.t()) :: EClient.t()
  def list_unspent_resources(client \\ setup()) do
    # Create an unrevealed commit using another example
    EIndexer.indexer_reads_unrevealed(client.node.node_id)
    # expected unspent resources
    expected_unspent_resources =
      Indexer.get(client.node.node_id, :resources)
      |> Enum.map(&Noun.Jam.jam/1)
      |> Enum.map(&Base.encode64/1)

    data =
      client.conn
      |> get(~p"/indexer/unspent-resources")
      |> json_response(200)

    %{
      "unspent_resources" => [
        "AaHYNGTIOA6AAOG3T2NX290OA60mr6qwQHGWTvHJoPCr8Q7bBg4GyJCgKQ=="
      ]
    }

    assert data == %{"unspent_resources" => expected_unspent_resources}

    client
  end

  @doc """
  List nullifiers on a blank node is the empty list.
  """
  @spec get_blocks_empty(EClient.t()) :: EClient.t()
  def get_blocks_empty(client \\ setup()) do
    payload = %{direction: :before, offset: 0}

    data =
      client.conn
      |> get(~p"/indexer/blocks", payload)
      |> json_response(200)

    assert data == %{"blocks" => []}

    client
  end

  @doc """
  List nullifiers on a blank node is the empty list.
  """
  @spec get_blocks(EClient.t()) :: EClient.t()
  def get_blocks(client \\ setup()) do
    # Create multiple blocks by calling the indexer example.
    # After this call, there should be two blocks present.
    EIndexer.indexer_reads_after(client.node.node_id)

    payload = %{direction: :before, offset: 100}

    data =
      client.conn
      |> get(~p"/indexer/blocks", payload)
      |> json_response(200)

    %{"blocks" => blocks} = data
    assert Enum.count(blocks) == 2

    client
  end

  @doc """
  List nullifiers on a blank node is the empty list.
  """
  @spec get_latest_empty(EClient.t()) :: EClient.t()
  def get_latest_empty(client \\ setup()) do
    data =
      client.conn
      |> get(~p"/indexer/latest-block")
      |> json_response(200)

    assert data == %{"block" => nil}

    client
  end

  @doc """
  List nullifiers on a blank node is the empty list.
  """
  @spec get_latest(EClient.t()) :: EClient.t()
  def get_latest(client \\ setup()) do
    # Create multiple blocks by calling the indexer example.
    # After this call, there should be two blocks present.
    EIndexer.indexer_reads_after(client.node.node_id)

    data =
      client.conn
      |> get(~p"/indexer/latest-block")
      |> json_response(200)

    %{"block" => block} = data
    assert block
    assert block["height"] == 2

    client
  end

  @doc """
  List nullifiers on a blank node is the empty list.
  """
  @spec get_root_empty(EClient.t()) :: EClient.t()
  def get_root_empty(client \\ setup()) do
    data =
      client.conn
      |> get(~p"/indexer/root")
      |> json_response(200)

    assert data == %{"root" => ""}

    client
  end

  @doc """
  List nullifiers on a blank node is the empty list.
  """
  @spec get_root(EClient.t()) :: EClient.t()
  def get_root(client \\ setup()) do
    EIndexer.indexer_reads_anchor(client.node.node_id)

    data =
      client.conn
      |> get(~p"/indexer/root")
      |> json_response(200)

    assert data == %{"root" => "I am a root at height 1"}

    client
  end

  @doc """
  List nullifiers on a blank node is the empty list.
  """
  @spec filter_resource_empty_no_filter(EClient.t()) :: EClient.t()
  def filter_resource_empty_no_filter(client \\ setup()) do
    payload = %{filters: []}

    data =
      client.conn
      |> post(~p"/indexer/filter-resources", payload)
      |> json_response(200)

    assert data == %{"resources" => []}

    client
  end

  @doc """
  List nullifiers on a blank node is the empty list.
  """
  @spec filter_resource_empty_owner(EClient.t()) :: EClient.t()
  def filter_resource_empty_owner(client \\ setup()) do
    payload = %{filters: [example_owner_filter()]}

    data =
      client.conn
      |> post(~p"/indexer/filter-resources", payload)
      |> json_response(200)

    assert data == %{"resources" => []}

    client
  end

  @doc """
  List nullifiers on a blank node is the empty list.
  """
  @spec filter_resource_owner(EClient.t()) :: EClient.t()
  def filter_resource_owner(client \\ setup()) do
    EIndexer.indexer_filters_owner(client.node.node_id)

    payload = %{filters: [example_owner_filter()]}

    data =
      client.conn
      |> post(~p"/indexer/filter-resources", payload)
      |> json_response(200)

    %{"resources" => resources} = data
    assert Enum.count(resources) == 1

    client
  end

  @doc """
  List nullifiers on a blank node is the empty list.
  """
  @spec filter_resource_empty_kind(EClient.t()) :: EClient.t()
  def filter_resource_empty_kind(client \\ setup()) do
    payload = %{filters: [example_kind_filter()]}

    data =
      client.conn
      |> post(~p"/indexer/filter-resources", payload)
      |> json_response(200)

    assert data == %{"resources" => []}

    client
  end

  @doc """
  List nullifiers on a blank node is the empty list.
  """
  @spec filter_resource_kind(EClient.t()) :: EClient.t()
  def filter_resource_kind(client \\ setup()) do
    EIndexer.indexer_filters_owner(client.node.node_id)

    payload = %{filters: [example_kind_filter()]}

    data =
      client.conn
      |> post(~p"/indexer/filter-resources", payload)
      |> json_response(200)

    %{"resources" => resources} = data
    assert Enum.count(resources) == 2
    client
  end

  @doc """
  List resources based on multiple filters.
  """
  @spec filter_resource_multiple_filters(EClient.t()) :: EClient.t()
  def filter_resource_multiple_filters(client \\ setup()) do
    EIndexer.indexer_filters_owner(client.node.node_id)

    payload = %{filters: [example_kind_filter(), example_kind_filter()]}

    data =
      client.conn
      |> post(~p"/indexer/filter-resources", payload)
      |> json_response(200)

    %{"resources" => resources} = data
    assert Enum.count(resources) == 2
    client
  end

  ############################################################
  #                           Helpers                        #
  ############################################################

  # @doc """
  # An example filter for a resource kind.
  # The filter is always the base64 encoded representation of the kind.
  # """
  @spec example_kind_filter :: %{kind: String.t()}
  defp example_kind_filter do
    kind = Base.encode64(Resource.kind(%Resource{}))
    %{kind: kind}
  end

  # @doc """
  # An example filter for an owner.
  # The filter is always the base64 encoded representation of the owner,
  # padded to 32 bytes.
  # """
  @spec example_owner_filter :: %{owner: String.t()}
  defp example_owner_filter do
    owner = Base.encode64(Noun.pad_trailing("jeremy", 32))
    %{owner: owner}
  end
end
