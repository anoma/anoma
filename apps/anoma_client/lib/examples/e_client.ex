defmodule Anoma.Client.Examples.EClient do
  @moduledoc """
  I contain functions to test the public interface of the client.

  I start a new client and if necessary a node, and then connect to that node.

  I test the public GRPC interface of the client to ensure it works as expected.
  """

  alias Anoma.Client
  alias Anoma.Client.Storage
  alias Anoma.Client.Examples.EClient
  alias Anoma.Node.Examples.ENode
  alias Anoma.Protobuf.Intents.Add
  alias Anoma.Protobuf.Intents.Intent
  alias Anoma.Protobuf.Intents.List
  alias Anoma.Protobuf.IntentsService
  alias Anoma.Protobuf.Executor.AddROTransaction
  alias Anoma.Protobuf.ExecutorService
  alias Anoma.Protobuf.Nock.Input
  alias Anoma.Protobuf.Nock.Prove
  alias Anoma.Protobuf.NockService
  alias Anoma.Protobuf.NodeInfo
  alias Examples.ETransparent.ETransaction
  alias Anoma.RM.Transparent.Action
  alias Anoma.RM.Transparent.Transaction
  alias Noun.Nounable

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

  @doc """
  I submit a read-only transaction
  """
  @spec submit_read_only_tx(EConnection.t()) :: EConnection.t()
  def submit_read_only_tx(conn \\ setup()) do
    node_id = %NodeInfo{node_id: conn.client.node.node_id}

    code =
      Anoma.Node.Examples.ETransaction.zero() |> elem(1) |> Noun.Jam.jam()

    request = %AddROTransaction.Request{node_info: node_id, transaction: code}

    {:ok, reply} =
      ExecutorService.Stub.add(conn.channel, request)

    result = [[["key"] | 0] | 0] |> Noun.Jam.jam()

    {:success, res} = reply.result

    assert res.result == result

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

    action =
      %Action{app_data: %{<<123>> => {"i am scried", true}}}

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

    assert {:ok, "i am scried"} =
             Storage.read(
               {System.os_time(), :crypto.hash(:sha256, "i am scried")}
             )

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

  @spec prove_with_external_scry_call(EConnection.t()) :: Prove.Response.t()
  def prove_with_external_scry_call_nounify(conn \\ setup()) do
    Anoma.Client.Examples.EStorage.setup()

    val = MapSet.new(["i am a set"])
    key = ["anoma", "blob", "key"]

    Anoma.Node.Transaction.Storage.write(
      conn.client.node.node_id,
      {1, [{key, val}]}
    )

    program = [[12, [1], 1 | ["id" | key]]] |> Noun.Jam.jam()

    request = %Prove.Request{
      program: {:jammed_program, program},
      public_inputs: []
    }

    {:ok, response} = NockService.Stub.prove(conn.channel, request)

    {:success, int_res} = response.result

    res = int_res.result

    noun_val = val |> Noun.Nounable.to_noun()

    assert Noun.Jam.jam(noun_val) == res

    {:ok, read_res} = Storage.read({System.os_time(), key})

    assert Noun.equal?(read_res, noun_val)

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
