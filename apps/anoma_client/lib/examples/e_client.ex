defmodule Anoma.Client.Examples.EClient do
  @moduledoc """
  I contain functions to test the public interface of the client.

  I start a new client and if necessary a node, and then connect to that node.

  I test the public GRPC interface of the client to ensure it works as expected.
  """
  use TypedStruct

  alias Anoma.Client
  alias Anoma.Client.Examples.EClient
  alias Anoma.Node.Examples.EIndexer
  alias Anoma.Node.Examples.ENode
  alias Anoma.Node.Transaction.Mempool
  alias Anoma.Protobuf.BlockService
  alias Anoma.Protobuf.Indexer.Blocks
  alias Anoma.Protobuf.Indexer.Nullifiers
  alias Anoma.Protobuf.Indexer.UnrevealedCommits
  alias Anoma.Protobuf.Indexer.UnspentResources
  alias Anoma.Protobuf.IndexerService
  alias Anoma.Protobuf.Intents.Add
  alias Anoma.Protobuf.Intents.Compose
  alias Anoma.Protobuf.Intents.Intent
  alias Anoma.Protobuf.Intents.List
  alias Anoma.Protobuf.Intents.Verify
  alias Anoma.Protobuf.IntentsService
  alias Anoma.Protobuf.Mempool.Dump
  alias Anoma.Protobuf.MempoolService
  alias Anoma.Protobuf.Nock.Input
  alias Anoma.Protobuf.Nock.Prove
  alias Anoma.Protobuf.NockService
  alias Anoma.Protobuf.NodeInfo
  alias Anoma.TransparentResource.Transaction
  alias Anoma.TransparentResource.Resource
  alias Examples.ETransparent.ETransaction
  alias Anoma.Node.Utility.Indexer
  alias Noun.Nounable

  import ExUnit.Assertions

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
    ENode.start_node(grpc_port: 0)
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

    {:ok, client} =
      Client.connect("localhost", enode.grpc_port, 0, enode.node_id)

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
    node_id = %NodeInfo{node_id: conn.client.node.node_id}
    request = %List.Request{node_info: node_id}

    {:ok, _reply} = IntentsService.Stub.list_intents(conn.channel, request)
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

    node_id = %NodeInfo{node_id: conn.client.node.node_id}

    request = %Add.Request{
      node_info: node_id,
      intent: %Intent{intent: intent_jammed}
    }

    {:ok, _reply} = IntentsService.Stub.add_intent(conn.channel, request)

    # fetch the intents to ensure it was added
    request = %List.Request{}

    {:ok, reply} = IntentsService.Stub.list_intents(conn.channel, request)

    assert reply.intents == [intent_jammed]

    conn
  end

  def compose_intents(conn \\ setup()) do
    # create two arbitrary intents
    jammed_intents =
      [ETransaction.single_swap(), ETransaction.single_swap()]
      |> Enum.map(&(Nounable.to_noun(&1) |> Noun.Jam.jam()))
      |> Enum.map(&%Intent{intent: &1})

    node_id = %NodeInfo{node_id: conn.client.node.node_id}

    request = %Compose.Request{
      node_info: node_id,
      intents: jammed_intents
    }

    {:ok, reply} =
      IntentsService.Stub.compose(conn.channel, request)

    # unjam the intent to check if its the same
    {:ok, composed_intent} =
      reply.intent.intent
      |> Noun.Jam.cue!()
      |> Transaction.from_noun()

    # jam and unjam the single swap to make them equivalent
    assert Noun.equal(
             Nounable.to_noun(ETransaction.single_swap()),
             composed_intent |> Nounable.to_noun()
           )

    composed_intent
  end

  def verify_intent(conn \\ setup()) do
    # create arbitrary intent
    jammed_intent =
      ETransaction.single_swap()
      |> Noun.Nounable.to_noun()
      |> Noun.Jam.jam()

    # create request for grpc endpoint
    intent = %Intent{intent: jammed_intent}

    node_id = %NodeInfo{node_id: conn.client.node.node_id}

    request = %Verify.Request{
      node_info: node_id,
      intent: intent
    }

    {:ok, reply} = IntentsService.Stub.verify(conn.channel, request)

    assert reply.valid

    reply.valid
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
    node_id = %NodeInfo{node_id: conn.client.node.node_id}
    request = %Nullifiers.Request{node_info: node_id}

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
      %Resource{rseed: "random2"}
      |> Resource.commitment()
      |> Elixir.List.wrap()

    # create the request to obtain the commits
    node_id = %NodeInfo{node_id: conn.client.node.node_id}
    request = %UnrevealedCommits.Request{node_info: node_id}

    {:ok, response} =
      IndexerService.Stub.list_unrevealed_commits(conn.channel, request)

    # assert the right commits are returned
    assert response.commits == expected_commits
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
    node_id = %NodeInfo{node_id: conn.client.node.node_id}
    request = %UnspentResources.Request{node_info: node_id}

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
    EIndexer.indexer_reads_after(conn.client.node.node_id)

    node_id = %NodeInfo{node_id: conn.client.node.node_id}

    # check for all blocks.
    request = %Blocks.Get.Request{node_info: node_id, index: {:before, 100}}
    {:ok, response} = BlockService.Stub.get(conn.channel, request)
    assert Enum.count(response.blocks) == 2

    # check the first block
    request = %Blocks.Get.Request{node_info: node_id, index: {:before, 1}}
    {:ok, response} = BlockService.Stub.get(conn.channel, request)
    assert Enum.count(response.blocks) == 1
    [block] = response.blocks
    assert block.height == 0
    conn
  end

  @doc """
  I return the latest block from the indexer when there are no blocks.
  """
  @spec get_latest_block_empty_index(EConnection.t()) :: EConnection.t()
  def get_latest_block_empty_index(conn \\ setup()) do
    node_id = %NodeInfo{node_id: conn.client.node.node_id}

    # check for all blocks.
    request = %Blocks.Latest.Request{node_info: node_id}

    {:ok, response} = BlockService.Stub.latest(conn.channel, request)
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
    EIndexer.indexer_reads_after(conn.client.node.node_id)

    node_id = %NodeInfo{node_id: conn.client.node.node_id}

    # check for all blocks.
    request = %Blocks.Latest.Request{node_info: node_id}

    {:ok, response} = BlockService.Stub.latest(conn.channel, request)
    assert response.block != nil

    conn
  end

  @doc """
  I return nil when trying to obtain the root of an empty index.
  """
  @spec get_root_unpopulated_index(EConnection.t()) :: EConnection.t()
  def get_root_unpopulated_index(conn \\ setup()) do
    node_id = %NodeInfo{node_id: conn.client.node.node_id}

    # check for all blocks.
    request = %Blocks.Root.Request{node_info: node_id}

    {:ok, response} = BlockService.Stub.root(conn.channel, request)
    assert response.root == <<>>

    conn
  end

  @doc """
  I get the root from the index.
  """
  @spec get_root_populated_index(EConnection.t()) :: EConnection.t()
  def get_root_populated_index(conn \\ setup()) do
    # Ensures that there is a root in the indexer.
    EIndexer.indexer_reads_anchor(conn.client.node.node_id)

    node_id = %NodeInfo{node_id: conn.client.node.node_id}

    # check for all blocks.
    request = %Blocks.Root.Request{node_info: node_id}

    {:ok, response} = BlockService.Stub.root(conn.channel, request)
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
    EIndexer.indexer_filters_owner(conn.client.node.node_id)

    node_id = %NodeInfo{node_id: conn.client.node.node_id}

    # check for all blocks.
    request = %Blocks.Filtered.Request{node_info: node_id, filters: []}

    {:ok, response} = BlockService.Stub.filter(conn.channel, request)
    assert Enum.count(response.resources) == 2

    # filter out where the owner is jeremy
    # see apps/anoma_node/lib/examples/e_indexer.ex:231 for the example
    jeremy = "jeremy" |> Noun.pad_trailing(32)

    request = %Blocks.Filtered.Request{
      node_info: node_id,
      filters: [%Blocks.Filtered.Filter{filter: {:owner, jeremy}}]
    }

    {:ok, response} = BlockService.Stub.filter(conn.channel, request)
    assert Enum.count(response.resources) == 1

    conn
  end

  @doc """
  I return an empty list when I try to filter resources if there are none.
  """
  @spec get_filtered_no_resources_exist(EConnection.t()) :: EConnection.t()
  def get_filtered_no_resources_exist(conn \\ setup()) do
    node_id = %NodeInfo{node_id: conn.client.node.node_id}

    # check for all blocks.
    request = %Blocks.Filtered.Request{node_info: node_id, filters: []}

    {:ok, response} = BlockService.Stub.filter(conn.channel, request)
    assert Enum.empty?(response.resources)

    conn
  end

  @doc """
  I filter on kind and expect the result to be one block.
  """
  @spec get_filtered_with_kind(EConnection.t()) :: EConnection.t()
  def get_filtered_with_kind(conn \\ setup()) do
    # Ensures that there are some resources.
    EIndexer.indexer_filters_owner(conn.client.node.node_id)

    # the example above inserted two resources, take one of the nullifier keys
    resource_kind =
      EIndexer.jeremy_resource()
      |> Resource.kind()

    # request all the resources that are owned by the nullifier key above.
    node_id = %NodeInfo{node_id: conn.client.node.node_id}

    filters = [
      %Blocks.Filtered.Filter{filter: {:kind, resource_kind}}
    ]

    request = %Blocks.Filtered.Request{node_info: node_id, filters: filters}

    {:ok, response} = BlockService.Stub.filter(conn.channel, request)

    # Expect two resources
    assert Enum.count(response.resources) == 2

    conn
  end

  @spec get_filtered_no_matches(EConnection.t()) :: EConnection.t()
  def get_filtered_no_matches(conn \\ setup()) do
    # Ensures that there are some resources.
    EIndexer.indexer_filters_owner(conn.client.node.node_id)

    # request all the resources that are owned by a non-existant nullifier key
    node_id = %NodeInfo{node_id: conn.client.node.node_id}

    filters = [%Blocks.Filtered.Filter{filter: {:kind, <<>>}}]

    request = %Blocks.Filtered.Request{node_info: node_id, filters: filters}

    {:ok, response} = BlockService.Stub.filter(conn.channel, request)
    assert Enum.empty?(response.resources)

    conn
  end

  @doc """
  I dump all transaction candidates from the mempool.

  The mempool is empty, so I expect an empty list.
  """
  @spec dump_mempool_example_empty_mempool(EConnection.t()) :: EConnection.t()
  def dump_mempool_example_empty_mempool(conn \\ setup()) do
    # request a dump of all the transactions
    node_id = %NodeInfo{node_id: conn.client.node.node_id}
    request = %Dump.Request{node_info: node_id}
    {:ok, response} = MempoolService.Stub.dump(conn.channel, request)

    # there are no transactions
    assert Enum.empty?(response.transaction_candidates)

    conn
  end

  @doc """
  I dump all transaction candidates from the mempool.
  """
  @spec dump_mempool_example(EConnection.t()) :: EConnection.t()
  def dump_mempool_example(conn \\ setup()) do
    # add one transaction to the mempool, but dont execute it.
    code = Anoma.Node.Examples.ETransaction.trivial_transparent_transaction()
    Mempool.tx(conn.client.node.node_id, code, "id 1")

    # request a dump from the mempool
    node_id = %NodeInfo{node_id: conn.client.node.node_id}
    request = %Dump.Request{node_info: node_id}
    {:ok, response} = MempoolService.Stub.dump(conn.channel, request)

    # assert that there is one candidate in the mempool
    assert Enum.count(response.transaction_candidates) == 1

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
