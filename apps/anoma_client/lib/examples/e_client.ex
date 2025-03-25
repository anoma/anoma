defmodule Anoma.Client.Examples.EClient do
  @moduledoc """
  I contain functions to test the public interface of the client.

  I start a new client and if necessary a node, and then connect to that node.

  I test the public GRPC interface of the client to ensure it works as expected.
  """

  alias Anoma.Client
  alias Anoma.Client.Examples.EClient
  alias Anoma.Client.Storage
  alias Anoma.Node.Examples.ENode
  alias Anoma.Proto.Indexer.Commits
  alias Anoma.Proto.Indexer.Filter
  alias Anoma.Proto.Indexer.FilterResource
  alias Anoma.Proto.Indexer.GetBlock
  alias Anoma.Proto.Indexer.LatestBlock
  alias Anoma.Proto.Indexer.Nullifiers
  alias Anoma.Proto.Indexer.RootBlock
  alias Anoma.Proto.Indexer.UnrevealedCommits
  alias Anoma.Proto.Indexer.UnspentResources
  alias Anoma.Proto.IndexerService
  alias Anoma.Proto.Intentpool.Add
  alias Anoma.Proto.Intentpool.Intent
  alias Anoma.Proto.Intentpool.List
  alias Anoma.Proto.IntentpoolService
  alias Anoma.Proto.Nock.Input
  alias Anoma.Proto.Nock.Prove
  alias Anoma.Proto.NockService
  alias Anoma.Proto.Node
  alias Anoma.TransparentResource.Action
  alias Anoma.TransparentResource.Resource
  alias Anoma.TransparentResource.Transaction
  alias Examples.ETransparent.ETransaction
  alias Anoma.Node.Examples.EIndexer
  alias Noun.Nounable
  alias Anoma.Node.Utility.Indexer

  import ExUnit.Assertions

  use TypedStruct

  ############################################################
  #                    Context                               #
  ############################################################

  typedstruct do
    @typedoc """
    I am the state of a TCP listener.

    My fields contain information to listen for TCP connection with a remote node.

    ### Fields
    - `:channel`    - The channel for making grpc requests.
    - `:supervisor` - the pid of the supervision tree.
    - `:node`       - The node to which the client is connected.
    - `:client`     - The client that is connected to the node.
    - `:channel`    - The channel for making grpc requests.
    """
    field(:supervisor, pid())
    field(:node, ENode.t())
    field(:client, Client.t())
    field(:channel, any())
  end

  typedstruct module: EConnection do
    @typedoc """
    I am an example GRPC stub connection to a client.
    I represent an outside client (e.g., a typescript application).

    ### Fields
    - `:channel`    - The channel for making grpc requests.
    """
    field(:channel, any())
    field(:client, EClient.t())
  end

  ############################################################
  #                    Helpers                               #
  ############################################################

  @doc """
  I create a new node in the system, and ensure that that is the only node that is running
  by killing all other nodes.
  """
  @spec create_single_example_node() :: ENode.t()
  def create_single_example_node() do
    ENode.kill_all_nodes()
    ENode.start_node()
  end

  @doc """
  I kill the existing client.
  """
  @spec kill_existing_client() :: :ok
  def kill_existing_client() do
    Client.ConnectionSupervisor
    |> DynamicSupervisor.which_children()
    |> Enum.each(fn {_, pid, _, _} -> Supervisor.stop(pid) end)
  end

  @doc """
  I create an instance of the client and connect it to the given node.

  If there is already a client started, I kill it and start a new one.
  """
  @spec create_example_client(ENode.t() | nil) :: EClient.t()
  def create_example_client(enode \\ create_single_example_node()) do
    kill_existing_client()

    grpc_port = Application.get_env(:anoma_node, :grpc_port)

    {:ok, client} = Client.connect("localhost", grpc_port, 0, enode.node_id)

    %EClient{supervisor: nil, client: client, node: enode}
  end

  @doc """
  I create an example stub to a given clients GRPC endpoint.
  """
  @spec create_example_connection(t()) :: EConnection.t()
  def create_example_connection(eclient \\ create_example_client()) do
    case GRPC.Stub.connect("localhost:#{eclient.client.grpc_port}") do
      {:ok, channel} ->
        %EConnection{channel: channel, client: eclient}

      {:error, reason} ->
        {:error, reason}
    end
  end

  @doc """
  I create the setup necessary to run each example below without arguments.
  """
  @spec setup() :: EConnection.t()
  def setup() do
    create_example_connection()
  end

  ############################################################
  #                    Examples                              #
  ############################################################

  @doc """
  I list the intents over grpc on the client.
  """
  @spec list_intents(EConnection.t()) :: EConnection.t()
  def list_intents(conn \\ setup()) do
    node_id = %Node{id: conn.client.node.node_id}
    request = %List.Request{node: node_id}

    {:ok, _reply} = IntentpoolService.Stub.list(conn.channel, request)
    conn
  end

  @doc """
  I add an intent to the client.
  """
  @spec add_intent(EConnection.t()) :: EConnection.t()
  def add_intent(conn \\ setup()) do
    # create an arbitrary intent and jam it
    intent_jammed =
      ETransaction.nullify_intent()
      |> Nounable.to_noun()
      |> Noun.Jam.jam()

    node_id = %Node{id: conn.client.node.node_id}

    request = %Add.Request{
      node: node_id,
      intent: %Intent{intent: intent_jammed}
    }

    {:ok, _reply} = IntentpoolService.Stub.add(conn.channel, request)

    # fetch the intents to ensure it was added
    request = %List.Request{}

    {:ok, reply} = IntentpoolService.Stub.list(conn.channel, request)

    assert reply.intents == [%Intent{intent: intent_jammed}]

    conn
  end

  @doc """
  I list all nullifiers.
  """
  @spec list_nullifiers(EConnection.t()) :: EConnection.t()
  def list_nullifiers(conn \\ setup()) do
    # Create a nullifier in the indexer
    EIndexer.indexer_reads_nullifier(conn.client.node.node_id)

    # expected nullifier
    expected_nullifier = Resource.nullifier(%Resource{})

    # request the nullifiers from the client
    node_id = %Node{id: conn.client.node.node_id}
    request = %Nullifiers.Request{node: node_id}

    {:ok, response} =
      IndexerService.Stub.list_nullifiers(conn.channel, request)

    assert response.nullifiers == [expected_nullifier]

    conn
  end

  @doc """
  I list all unrevealed commits.
  """
  @spec list_unrevealed_commits(EConnection.t()) :: EConnection.t()
  def list_unrevealed_commits(conn \\ setup()) do
    # Create an unrevealed commit using another example
    EIndexer.indexer_reads_unrevealed(conn.client.node.node_id)

    # expected commits
    expected_commits =
      conn.client.node.node_id |> Indexer.get(:unrevealed) |> Enum.to_list()

    # create the request to obtain the commits
    node_id = %Node{id: conn.client.node.node_id}
    request = %UnrevealedCommits.Request{node: node_id}

    {:ok, response} =
      IndexerService.Stub.list_unrevealed_commits(conn.channel, request)

    # assert the right commits are returned
    assert response.commits ==
             expected_commits

    conn
  end

  @doc """
  I list all commits.
  """
  @spec list_all_commits(EConnection.t()) :: EConnection.t()
  def list_all_commits(conn \\ setup()) do
    # Create two commit using another example
    EIndexer.indexer_reads_commitments(conn.client.node.node_id)

    # expected commits
    expected_commits = conn.client.node.node_id |> Indexer.get(:cms)

    # create the request to obtain the commits
    node_id = %Node{id: conn.client.node.node_id}
    request = %Commits.Request{node: node_id}

    {:ok, response} =
      IndexerService.Stub.list_commits(conn.channel, request)

    # assert the right commits are returned
    assert response.commits |> MapSet.new() == expected_commits
    conn
  end

  @doc """
  I list all unspent resources.
  """
  @spec list_unspent_resources(EConnection.t()) :: EConnection.t()
  def list_unspent_resources(conn \\ setup()) do
    # Create an unrevealed commit using another example
    EIndexer.indexer_reads_unrevealed(conn.client.node.node_id)

    # expected unspent resources
    expected_unspent_resources =
      Indexer.get(conn.client.node.node_id, :resources)
      |> Enum.map(&Noun.Jam.jam/1)

    # create the request to obtain the unspent resources
    node_id = %Node{id: conn.client.node.node_id}
    request = %UnspentResources.Request{node: node_id}

    {:ok, reply} =
      IndexerService.Stub.list_unspent_resources(conn.channel, request)

    assert reply.unspent_resources == expected_unspent_resources

    conn
  end

  @doc """
  I return blocks from the indexer.
  """
  @spec list_blocks(EConnection.t()) :: EConnection.t()
  def list_blocks(conn \\ setup()) do
    # Create multiple blocks by calling the indexer example.
    # After this call, there should be two blocks present.
    Anoma.Node.Examples.EIndexer.indexer_reads_after(conn.client.node.node_id)

    node_id = %Node{id: conn.client.node.node_id}

    # check for all blocks.
    request = %GetBlock.Request{node: node_id, index: {:before, 100}}
    {:ok, response} = IndexerService.Stub.get_block(conn.channel, request)
    assert Enum.count(response.blocks) == 2

    # check the first block
    request = %GetBlock.Request{node: node_id, index: {:before, 2}}
    {:ok, response} = IndexerService.Stub.get_block(conn.channel, request)
    assert Enum.count(response.blocks) == 1
    [block] = response.blocks
    assert block.height == 1
    conn
  end

  @doc """
  I return the latest block from the indexer when there are no blocks.
  """
  @spec get_latest_block_empty_index(EConnection.t()) :: EConnection.t()
  def get_latest_block_empty_index(conn \\ setup()) do
    node_id = %Node{id: conn.client.node.node_id}

    # check for all blocks.
    request = %LatestBlock.Request{node: node_id}

    {:ok, response} = IndexerService.Stub.latest_block(conn.channel, request)
    assert response.block == nil

    conn
  end

  @doc """
  I return the latest block from the indexer when there are two blocks.
  """
  @spec get_latest_block_populated_index(EConnection.t()) :: EConnection.t()
  def get_latest_block_populated_index(conn \\ setup()) do
    # Create multiple blocks by calling the indexer example.
    # After this call, there should be two blocks present.
    Anoma.Node.Examples.EIndexer.indexer_reads_after(conn.client.node.node_id)

    node_id = %Node{id: conn.client.node.node_id}

    # check for all blocks.
    request = %LatestBlock.Request{node: node_id}

    {:ok, response} = IndexerService.Stub.latest_block(conn.channel, request)
    assert response.block != nil

    conn
  end

  @doc """
  I return nil when trying to obtain the root of an empty index.
  """
  @spec get_root_unpopulated_index(EConnection.t()) :: EConnection.t()
  def get_root_unpopulated_index(conn \\ setup()) do
    node_id = %Node{id: conn.client.node.node_id}

    # check for all blocks.
    request = %RootBlock.Request{node: node_id}

    {:ok, response} = IndexerService.Stub.root_block(conn.channel, request)
    assert response.root == <<>>

    conn
  end

  @doc """
  I get the root from the index.
  """
  @spec get_root_populated_index(EConnection.t()) :: EConnection.t()
  def get_root_populated_index(conn \\ setup()) do
    # Ensures that there is a root in the indexer.
    Anoma.Node.Examples.EIndexer.indexer_reads_anchor(
      conn.client.node.node_id
    )

    node_id = %Node{id: conn.client.node.node_id}

    # check for all blocks.
    request = %RootBlock.Request{node: node_id}

    {:ok, response} = IndexerService.Stub.root_block(conn.channel, request)
    assert response.root == "I am a root at height 1"

    conn
  end

  @doc """
  I return the filtered resources when I filter on owner.
  I return all blocks if no filters are provided.
  """
  @spec get_filtered(EConnection.t()) :: EConnection.t()
  def get_filtered(conn \\ setup()) do
    # Ensures that there is a root in the indexer.
    Anoma.Node.Examples.EIndexer.indexer_filters_owner(
      conn.client.node.node_id
    )

    node_id = %Node{id: conn.client.node.node_id}

    # check for all blocks.
    request = %FilterResource.Request{node: node_id, filters: []}

    {:ok, response} =
      IndexerService.Stub.filter_resource(conn.channel, request)

    assert Enum.count(response.resources) == 2

    # filter out where the owner is jeremy
    # see apps/anoma_node/lib/examples/e_indexer.ex:231 for the example
    jeremy = "jeremy" |> Noun.pad_trailing(32)

    request = %FilterResource.Request{
      node: node_id,
      filters: [%Filter{filter: {:owner, jeremy}}]
    }

    {:ok, response} =
      IndexerService.Stub.filter_resource(conn.channel, request)

    assert Enum.count(response.resources) == 1

    conn
  end

  @doc """
  I return an empty list when I try to filter resources if there are none.
  """
  @spec get_filtered_no_resources_exist(EConnection.t()) :: EConnection.t()
  def get_filtered_no_resources_exist(conn \\ setup()) do
    node_id = %Node{id: conn.client.node.node_id}

    # check for all blocks.
    request = %FilterResource.Request{node: node_id, filters: []}

    {:ok, response} =
      IndexerService.Stub.filter_resource(conn.channel, request)

    assert Enum.count(response.resources) == 0

    conn
  end

  @doc """
  I filter on kind and expect the result to be one block.
  """
  @spec get_filtered_with_kind(EConnection.t()) :: EConnection.t()
  def get_filtered_with_kind(conn \\ setup()) do
    # Ensures that there are some resources.
    Anoma.Node.Examples.EIndexer.indexer_filters_owner(
      conn.client.node.node_id
    )

    node_id = %Node{id: conn.client.node.node_id}

    # filter that matches two resources
    request = %FilterResource.Request{
      node: node_id,
      filters: [
        %Filter{
          filter: {:kind, %Resource{} |> Resource.kind()}
        }
      ]
    }

    {:ok, response} =
      IndexerService.Stub.filter_resource(conn.channel, request)

    assert Enum.count(response.resources) == 2

    # filter that doesnt match any
    request = %FilterResource.Request{
      node: node_id,
      filters: [
        %Filter{filter: {:kind, <<>>}}
      ]
    }

    {:ok, response} =
      IndexerService.Stub.filter_resource(conn.channel, request)

    assert Enum.count(response.resources) == 0

    conn
  end

  @doc """
  I run a plaintext nock program using the client.
  """
  @spec prove_something_text(EConnection.t()) :: Prove.Response.t()
  def prove_something_text(conn \\ setup()) do
    program = text_program_example()
    input1 = text_input("1")
    input2 = text_input("2")
    input3 = text_input("3")
    input4 = text_input("4")

    request = %Prove.Request{
      program: {:text_program, program},
      public_inputs: [input1, input2],
      private_inputs: [input3, input4]
    }

    {:ok, response} = NockService.Stub.prove(conn.channel, request)
    {:success, success} = response.result

    assert {:ok, [<<1>>, <<2>>, <<3>> | <<4>>]} ==
             Noun.Jam.cue(success.result)

    success.result
  end

  @doc """
  I run a jammed nock program using the client.
  """
  @spec prove_something_jammed(EConnection.t()) :: Prove.Response.t()
  def prove_something_jammed(conn \\ setup()) do
    program = jammed_program_example()
    input1 = jammed_input(Noun.Jam.jam(1))
    input2 = jammed_input(Noun.Jam.jam(2))
    input3 = jammed_input(Noun.Jam.jam(3))
    input4 = jammed_input(Noun.Jam.jam(4))

    request = %Prove.Request{
      program: {:jammed_program, program},
      public_inputs: [input1, input2],
      private_inputs: [input3, input4]
    }

    {:ok, response} = NockService.Stub.prove(conn.channel, request)

    {:success, success} = response.result

    assert {:ok, [<<1>>, <<2>>, <<3>> | <<4>>]} ==
             Noun.Jam.cue(success.result)

    success.result
  end

  @doc """
  I run a Juvix program that squares its inputs.
  """
  @spec run_juvix_factorial(EConnection.t()) :: Prove.Response.t()
  def run_juvix_factorial(conn \\ setup()) do
    # assume the program and inputs are jammed
    program = jammed_program_juvix_squared()
    input = jammed_input(Noun.Jam.jam(3))

    request = %Prove.Request{
      program: {:jammed_program, program},
      public_inputs: [input]
    }

    {:ok, response} = NockService.Stub.run(conn.channel, request)

    {:success, success} = response.result

    assert {:ok, <<9>>} == Noun.Jam.cue(success.result)

    success.result
  end

  @doc """
  I run a Juvix program that squares its inputs without an argument.

  I expect the result to be 0.
  """
  @spec prove_juvix_factorial_no_arguments(EConnection.t()) ::
          Prove.Response.t()
  def prove_juvix_factorial_no_arguments(conn \\ setup()) do
    # assume the program and inputs are jammed
    program = jammed_program_juvix_squared()

    request = %Prove.Request{
      program: {:jammed_program, program},
      public_inputs: []
    }

    {:ok, response} = NockService.Stub.prove(conn.channel, request)

    {:success, success} = response.result

    assert {:ok, <<>>} == Noun.Jam.cue(success.result)

    success.result
  end

  @doc """
  I run a Juvix program that squares its inputs without an argument.

  I expect the result to be 0.
  """
  @spec run_juvix_factorial_no_arguments(EConnection.t()) ::
          Prove.Response.t()
  def run_juvix_factorial_no_arguments(conn \\ setup()) do
    # assume the program and inputs are jammed
    program = jammed_program_juvix_squared()

    request = %Prove.Request{
      program: {:jammed_program, program},
      public_inputs: []
    }

    {:ok, response} = NockService.Stub.run(conn.channel, request)

    {:success, success} = response.result

    assert {:ok, <<>>} == Noun.Jam.cue(success.result)

    success.result
  end

  @spec run_juvix_with_hints(EConnection.t()) :: Prove.Response.t()
  def run_juvix_with_hints(conn \\ setup()) do
    # assume the program and inputs are jammed
    program = jammed_program_tracing()

    request = %Prove.Request{
      program: {:jammed_program, program},
      public_inputs: []
    }

    {:ok, response} = NockService.Stub.run(conn.channel, request)

    {:success, success} = response.result

    assert [<<1>>, <<4>>, <<2>>, <<4>>] ==
             Enum.map(success.output, &Noun.Jam.cue!/1)

    assert {:ok, <<>>} == Noun.Jam.cue(success.result)

    success.result
  end

  @spec prove_with_internal_scry_call(EConnection.t()) :: Prove.Response.t()
  def prove_with_internal_scry_call(conn \\ setup()) do
    Anoma.Client.Examples.EStorage.setup()

    blob_key = ["anoma", "blob", <<123>>]
    blob_value = "i am scried"

    action =
      %Action{app_data: %{blob_key => {blob_value, true}}}

    tx =
      %Transaction{actions: MapSet.new([action])} |> Noun.Nounable.to_noun()

    key = ["anoma", "blob", "key"]
    Storage.write({key, tx})
    program = [[12, [1], 1 | ["id" | key]]] |> Noun.Jam.jam()

    request = %Prove.Request{
      program: {:jammed_program, program},
      public_inputs: []
    }

    {:ok, response} = NockService.Stub.prove(conn.channel, request)

    res = response.result |> elem(1) |> Map.get(:result)

    assert res == Noun.Jam.jam(tx)

    assert {:ok, ^blob_value} = Storage.read({System.os_time(), blob_key})

    res
  end

  @spec prove_with_external_scry_call(EConnection.t()) :: Prove.Response.t()
  def prove_with_external_scry_call(conn \\ setup()) do
    Anoma.Client.Examples.EStorage.setup()
    key = ["anoma", "blob", "key"]

    Anoma.Node.Transaction.Storage.write(
      conn.client.node.node_id,
      {1, [{key, 123}]}
    )

    program = [[12, [1], 1 | ["id" | key]]] |> Noun.Jam.jam()

    request = %Prove.Request{
      program: {:jammed_program, program},
      public_inputs: []
    }

    {:ok, response} = NockService.Stub.prove(conn.channel, request)

    res = response.result |> elem(1) |> Map.get(:result)

    assert 123 |> Noun.Jam.jam() == res

    assert Storage.read({System.os_time(), key})
           |> elem(1)
           |> Noun.equal?(123)

    res
  end

  ############################################################
  #                           Helpers                        #
  ############################################################

  @spec jammed_program_tracing() :: binary()
  def jammed_program_tracing() do
    :code.priv_dir(:anoma_client)
    |> Path.join("test_juvix/Tracing.nockma")
    |> File.read!()
  end

  @spec noun_program_tracing() :: Noun.t()
  def noun_program_tracing() do
    jammed_program_tracing()
    |> Noun.Jam.cue()
    |> elem(1)
  end

  @spec text_program_tracing() :: String.t()
  def text_program_tracing() do
    jammed_program_tracing()
    |> Noun.Jam.cue()
    |> elem(1)
  end

  @spec jammed_program_juvix_squared() :: binary()
  def jammed_program_juvix_squared() do
    :code.priv_dir(:anoma_client)
    |> Path.join("test_juvix/Squared.nockma")
    |> File.read!()
  end

  @spec noun_program_juvix_squared() :: Noun.t()
  def noun_program_juvix_squared() do
    jammed_program_juvix_squared()
    |> Noun.Jam.cue()
    |> elem(1)
  end

  @spec text_program_example() :: binary()
  def text_program_example() do
    "[[0 6] 0 0]"
  end

  @spec jammed_program_example() :: binary()
  def jammed_program_example() do
    noun_program_example()
    |> Noun.Jam.jam()
  end

  @spec noun_program_example() :: Noun.t()
  def noun_program_example() do
    text_program_example()
    |> Noun.Format.parse_always()
  end

  @spec text_program_minisquare() :: binary()
  def text_program_minisquare() do
    "[[1 123] 0 0]"
  end

  @spec jammed_program_minisquare() :: binary()
  def jammed_program_minisquare() do
    noun_program_minisquare()
    |> Noun.Jam.jam()
  end

  @spec noun_program_minisquare() :: Noun.t()
  def noun_program_minisquare() do
    text_program_minisquare()
    |> Noun.Format.parse_always()
  end

  @spec jammed_input(any()) :: Input.t()
  def jammed_input(value \\ <<>>) do
    %Input{input: {:jammed, value}}
  end

  @spec text_input(any()) :: Input.t()
  def text_input(value \\ "") do
    %Input{input: {:text, value}}
  end
end
