defmodule Anoma.Node.Examples.ERegistry do
  import ExUnit.Assertions

  alias Anoma.Node.Registry
  alias Anoma.Node.Registry.Address
  alias Anoma.Crypto.Id

  @doc """
  I start a new registry, or return the address of the one that is already running.
  """
  @spec start_registry() :: {:ok, pid()}
  def start_registry() do
    case Elixir.Registry.start_link(keys: :unique, name: Anoma.Node.Registry) do
      {:error, {:already_started, pid}} -> {:ok, pid}
      {:ok, pid} -> {:ok, pid}
    end
  end

  @doc """
  I create an address for a given node id and module.

  I assert that the address is created correctly.
  """
  @spec create_address() :: Registry.Address.t()
  def create_address() do
    node_id = Id.new_keypair()
    address = Registry.address(node_id, :module)

    expected_address = %Anoma.Node.Registry.Address{
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
    node_id = Id.new_keypair()
    address = Registry.address(node_id, :module, :label)

    expected_address = %Anoma.Node.Registry.Address{
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
    node_id = Id.new_keypair()
    name = Registry.name(node_id, :module)

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
    node_id = Id.new_keypair()
    name = Registry.name(node_id, :module, :label)

    expected_name =
      {:via, Elixir.Registry,
       {Registry, %Address{node_id: node_id, engine: :module, label: :label}}}

    assert name == expected_name

    name
  end

  @doc """
  I register a process in the registry and then lookup its pid.
  """
  def find_pid_of_process() do
    start_registry()
    node_id = Id.new_keypair()

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
  def list_engines_for_node() do
    start_registry()

    # start a process, and register some dummy engines.
    node_id = Id.new_keypair()

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
  end
end
