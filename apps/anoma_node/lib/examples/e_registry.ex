defmodule Anoma.Node.Examples.ERegistry do
  import ExUnit.Assertions

  alias __MODULE__
  alias Anoma.Node.Examples.ENode
  alias Anoma.Node.Registry
  alias Anoma.Node.Registry.Address

  use TypedStruct

  ############################################################
  #                    Context                               #
  ############################################################

  typedstruct do
    @typedoc """
    I am the state of a registry created in the examples.

    ### Fields
    - `:pid`      - the pid of the supervision tree.
    """
    field(:pid, pid())
  end

  @doc """
  Given a node id and an engine, I check if this process is registered and alive.
  """
  @spec process_registered?(String.t(), atom()) :: boolean()
  def process_registered?(node_id, engine) do
    case Registry.whereis(node_id, engine) do
      nil ->
        false

      pid ->
        Process.alive?(pid)
    end
  end

  @doc """
  I start a new registry, or return the address of the one that is already running.
  """
  @spec start_registry() :: ERegistry.t() | :error
  def start_registry() do
    case Elixir.Registry.start_link(keys: :unique, name: Registry) do
      {:error, {:already_started, pid}} ->
        %ERegistry{pid: pid}

      {:ok, pid} ->
        %ERegistry{pid: pid}

      _ ->
        :error
    end
  end

  @doc """
  I create an address for a given node id and module.

  I assert that the address is created correctly.
  """
  @spec create_address() :: Registry.Address.t()
  def create_address() do
    node_id =
      "londo_mollari" <>
        (:crypto.strong_rand_bytes(16)
         |> Base.url_encode64())

    address = Registry.address(node_id, :module)

    expected_address = %Address{
      label: nil,
      engine: :module,
      node_id: node_id
    }

    assert expected_address == address

    address
  end

  @doc """
  I create an address for a given node id, module, and label.
  """
  @spec create_address_with_label() :: Registry.Address.t()
  def create_address_with_label() do
    node_id =
      "londo_mollari" <>
        (:crypto.strong_rand_bytes(16)
         |> Base.url_encode64())

    address = Registry.address(node_id, :module, :label)

    expected_address = %Address{
      label: :label,
      engine: :module,
      node_id: node_id
    }

    assert expected_address == address

    address
  end

  @doc """
  Given a node id and an engine, I generate a name that can be used to register processes.
  """
  def generate_name() do
    node_id =
      "londo_mollari" <>
        (:crypto.strong_rand_bytes(16)
         |> Base.url_encode64())

    name = Registry.via(node_id, :module)

    expected_name =
      {:via, Elixir.Registry,
       {Registry, %Address{node_id: node_id, engine: :module}}}

    assert name == expected_name

    name
  end

  @doc """
  I generate a name from a module, node id and label.
  """
  @spec generate_name_with_label() ::
          {:via, atom(), {atom(), Address.t()}}
  def generate_name_with_label() do
    node_id =
      "londo_mollari" <>
        (:crypto.strong_rand_bytes(16)
         |> Base.url_encode64())

    name = Registry.via(node_id, :module, :label)

    expected_name =
      {:via, Elixir.Registry,
       {Registry, %Address{node_id: node_id, engine: :module, label: :label}}}

    assert name == expected_name

    name
  end

  @doc """
  I register a process in the registry and then lookup its pid.
  """
  @spec find_pid_of_process(String.t()) :: :ok
  def find_pid_of_process(node_id \\ "") do
    start_registry()

    # create a process that registers itself and then waits to be shut down.
    this = self()

    process =
      spawn(fn ->
        Registry.register(node_id, :module)

        send(this, :registered)

        receive do
          :stop -> :ok
        end
      end)

    assert_receive(:registered)

    # the registry can induce a short delay before the process is registered.
    assert Registry.whereis(node_id, :module) == process

    # terminate the process
    send(process, :stop)
  end

  @doc """
  I register a few engines for a specific node, and then ask the registry for the list.
  """
  @spec list_engines_for_node(String.t()) :: :ok
  def list_engines_for_node(node_id \\ "") do
    start_registry()

    # let the engine processes send a message to the current process.
    this = self()

    # names of all the engines that are registered.
    engines =
      [
        Anoma.Node.Transaction.Mempool,
        Anoma.Node.Transaction.IntentPool
      ]
      |> Enum.sort()

    engine_pids =
      for engine <- engines do
        engine_pid =
          spawn(fn ->
            Registry.register(node_id, engine)

            send(this, :registered)

            receive do
              :stop -> :ok
            end
          end)

        # wait for the process to be registered
        assert_receive(:registered)

        engine_pid
      end

    # the registry can induce a short delay before the process is registered.
    assert Enum.sort(Registry.engines_for(node_id)) == engines

    for engine_pid <- engine_pids do
      send(engine_pid, :stop)
    end

    :ok
  end

  @doc """
  Given a node, I check for a local engine of a specific type.

  If there are multiple engines present of the same type, I expect an error.
  """
  @spec single_node_running() :: :ok
  def single_node_running() do
    # for this test no nodes can be present.
    # all nodes are killed.
    ENode.kill_all_nodes()

    # assert no node running error
    # give the registry some time to clean up
    Process.sleep(100)
    assert Kernel.match?({:error, :no_node_running}, Registry.local_node_id())

    # start one node and assert the node is returned
    node_id =
      "londo_mollari" <>
        (:crypto.strong_rand_bytes(16)
         |> Base.url_encode64())

    %ENode{} = ENode.start_node(node_id: node_id)

    assert {:ok, node_id} == Registry.local_node_id()

    # start a second node, check for the exepcted error
    %ENode{} = ENode.start_node()
    assert {:error, :multiple_nodes} == Registry.local_node_id()

    :ok
  end
end
