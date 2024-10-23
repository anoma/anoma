defmodule Anoma.Client.Examples.EClient do
  @moduledoc """
  I contain functions to test the public interface of the client.

  I start a new client and if necessary a node, and then connect to that node.

  I test the public GRPC interface of the client to ensure it works as expected.
  """
  use TypedStruct

  alias __MODULE__
  alias Anoma.Client
  alias Anoma.Node.Examples.ENode
  alias Anoma.Protobuf.Indexer.Nullifiers
  alias Anoma.Protobuf.Indexer.UnrevealedCommits
  alias Anoma.Protobuf.Indexer.UnspentResources
  alias Anoma.Protobuf.Intent
  alias Anoma.Protobuf.IntentPool.AddIntent
  alias Anoma.Protobuf.IntentPool.ListIntents
  alias Anoma.Protobuf.Intents
  alias Anoma.Protobuf.Prove
  alias Anoma.Protobuf.RunNock
  alias Anoma.Protobuf.Input

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

    {:ok, client} = Client.connect("localhost", enode.grpc_port, 0)

    %EClient{supervisor: nil, client: client, node: enode}
  end

  @doc """
  I create an example stub to a given clients GRPC endpoint.
  """
  @spec create_example_connection() :: EConnection.t()
  def create_example_connection(eclient \\ create_example_client()) do
    case GRPC.Stub.connect("localhost:#{eclient.client.grpc_port}") do
      {:ok, channel} ->
        %EConnection{channel: channel}

      {:error, reason} ->
        {:error, reason}
    end
  end

  @doc """
  I create the setup necessary to run each example below without arguments.
  """
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
    request = %ListIntents.Request{}
    {:ok, _reply} = Intents.Stub.list_intents(conn.channel, request)
    conn
  end

  @doc """
  I add an intent to the client.
  """
  @spec add_intent(EConnection.t()) :: EConnection.t()
  def add_intent(conn \\ setup()) do
    request = %AddIntent.Request{intent: %Intent{value: 1}}

    {:ok, _reply} = Intents.Stub.add_intent(conn.channel, request)

    # fetch the intents to ensure it was added
    request = %ListIntents.Request{}

    {:ok, reply} = Intents.Stub.list_intents(conn.channel, request)

    assert reply.intents == ["1"]

    conn
  end

  @doc """
  I list all nullifiers.
  """
  @spec list_nullifiers(EConnection.t()) :: EConnection.t()
  def list_nullifiers(conn \\ setup()) do
    request = %Nullifiers.Request{}
    {:ok, _reply} = Intents.Stub.list_nullifiers(conn.channel, request)

    conn
  end

  @doc """
  I list all unrevealed commits.
  """
  @spec list_unrevealed_commits(EConnection.t()) :: EConnection.t()
  def list_unrevealed_commits(conn \\ setup()) do
    request = %UnrevealedCommits.Request{}

    {:ok, _reply} =
      Intents.Stub.list_unrevealed_commits(conn.channel, request)

    conn
  end

  @doc """
  I list all unspent resources.
  """
  @spec list_unspent_resources(EConnection.t()) :: EConnection.t()
  def list_unspent_resources(conn \\ setup()) do
    request = %UnspentResources.Request{}

    {:ok, _reply} =
      Intents.Stub.list_unspent_resources(conn.channel, request)

    conn
  end

  @doc """
  I prove something using the client.
  """
  def prove_something_jammed(conn \\ setup()) do
    nock_str = "[[1 123] 0 0]"
    pub_inputs = [<<>>, <<>>, <<>>]
    priv_inputs = [<<>>, <<>>, <<>>]

    # the client sends a knock program that is jammed.
    nock_program = nock_str |> Noun.Format.parse_always() |> Nock.Jam.jam()

    # each input is jammed individually
    inputs_pub =
      Enum.map(pub_inputs, &%Input{input: {:jammed, Nock.Jam.jam(&1)}})

    inputs_priv =
      Enum.map(priv_inputs, &%Input{input: {:jammed, Nock.Jam.jam(&1)}})

    request = %Prove.Request{
      program: {:jammed_program, nock_program},
      public_inputs: inputs_pub,
      private_inputs: inputs_priv
    }

    {:ok, _reply} = Intents.Stub.prove(conn.channel, request)
  end

  @doc """
  I prove something using the client.
  """
  def prove_something_plain_text(conn \\ setup()) do
    nock_str = "[[1 123] 0 0]"
    pub_inputs = ["1", "2", "3"]
    priv_inputs = ["4", "5", "6"]

    # build input structs
    pub_inputs = Enum.map(pub_inputs, &%Input{input: {:text, "#{&1}"}})
    priv_inputs = Enum.map(priv_inputs, &%Input{input: {:text, "#{&1}"}})

    request = %Prove.Request{
      program: {:text_program, nock_str},
      public_inputs: pub_inputs,
      private_inputs: priv_inputs
    }

    {:ok, _reply} = Intents.Stub.prove(conn.channel, request)
  end

  @doc """
  I run a plaintext nock program using the client.
  """
  def run_something_plain_text(conn \\ setup()) do
    nock_str = "[[1 123] 0 0]"
    inputs = ["1", "2", "3"]

    # build input structs
    inputs = Enum.map(inputs, &%Input{input: {:text, "#{&1}"}})

    request = %RunNock.Request{
      program: {:text_program, nock_str},
      inputs: inputs
    }

    {:ok, _reply} = Intents.Stub.prove(conn.channel, request)
  end

  @doc """
  I run a jammed nock program using the client.
  """
  def run_something_jammed(conn \\ setup()) do
    nock_str = "[[1 123] 0 0]"
    inputs = ["1", "2", "3"]

    nock_program = nock_str |> Noun.Format.parse_always() |> Nock.Jam.jam()

    # build input structs
    inputs =
      Enum.map(
        inputs,
        &%Input{
          input: {:jammed, &1 |> Noun.Format.parse_always() |> Nock.Jam.jam()}
        }
      )

    request = %RunNock.Request{
      program: {:jammed_program, nock_program},
      inputs: inputs
    }

    {:ok, _reply} = Intents.Stub.prove(conn.channel, request)
  end

  def run_juvix_factorial(conn \\ setup()) do
    # assume the program and inputs are jammed
    program_jammed =
      File.read!("apps/anoma_client/priv/test_juvix/Squared.nockma")

    inputs_jammed =
      ["3"]
      |> Enum.map(&Noun.Format.parse_always/1)
      |> Enum.map(&Nock.Jam.jam/1)

    # cue the program and inputs into nouns
    {:ok, program_noun} = Nock.Cue.cue(program_jammed)

    input_nouns =
      inputs_jammed
      |> Enum.map(fn i ->
        {:ok, input} = Nock.Cue.cue(i)
        input
      end)

    # eval with nouns
    nock =
      (Noun.list_nock_to_erlang(program_noun) ++ [Nock.rm_core()])
      |> Anoma.Client.Api.Server.to_improper_list()

    stuff = Anoma.Client.Api.Server.to_improper_list([6, 1] ++ input_nouns)

    Nock.nock(nock, [9, 2, 10, stuff, 0 | 1])
    # # read in the juvix binary program and encode it
    # {:ok, squared} =
    #   File.read!("apps/anoma_client/priv/test_juvix/Squared.nockma")
    #   |> Nock.Cue.cue()

    # inputs = "3" |> Noun.Format.parse_always()
    # the list of inputs (in this case, just the three)
    # inputs =
    #   ["3"]
    #   |> Enum.map(
    #     &%Input{
    #       input: {:jammed, &1 |> Noun.Format.parse_always() |> Nock.Jam.jam() |> Base.encode64()}
    #     }
    #   )

    #   nock =
    #     ((squared |> Noun.list_nock_to_erlang()) ++ [Nock.rm_core()])
    #     |> Anoma.Client.Api.Server.to_improper_list()
    #     |> tap(fn x -> IO.inspect(x) end)
  end
end
