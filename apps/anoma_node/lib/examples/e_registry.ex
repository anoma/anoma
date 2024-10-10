defmodule Anoma.Node.Examples.ERegistry do
  import ExUnit.Assertions

  alias __MODULE__
  alias Anoma.Crypto.Id
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
  @spec process_registered?(Id.t(), atom()) :: boolean()
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
  @spec create_address(Id.t()) :: Address.t()
  def create_address(node_id \\ Id.new_keypair()) do
    address = Registry.address(node_id, :module)

    expected_address = %Address{
      label: nil,
      engine: :module,
      node_id: node_id.external
    }

    assert expected_address == address

    address
  end

  @doc """
  I create an address for a given node id, module, and label.
  """
  @spec create_address_with_label(Id.t()) :: Address.t()
  def create_address_with_label(node_id \\ Id.new_keypair()) do
    address = Registry.address(node_id, :module, :label)

    expected_address = %Address{
      label: :label,
      engine: :module,
      node_id: node_id.external
    }

    assert expected_address == address

    address
  end

  @doc """
  Given a node id and an engine, I generate a name that can be used to register processes.
  """
  @spec generate_name(Id.t()) :: {:via, atom(), {atom(), Address.t()}}
  def generate_name(node_id \\ Id.new_keypair()) do
    name = Registry.name(node_id, :module)

    expected_name =
      {:via, Elixir.Registry,
       {Registry, %Address{node_id: node_id.external, engine: :module}}}

    assert name == expected_name

    name
  end

  @doc """
  I generate a name from a module, node id and label.
  """
  @spec generate_name_with_label() :: {:via, atom(), {atom(), Address.t()}}
  def generate_name_with_label(node_id \\ Id.new_keypair()) do
    name = Registry.name(node_id, :module, :label)

    expected_name =
      {:via, Elixir.Registry,
       {Registry,
        %Address{node_id: node_id.external, engine: :module, label: :label}}}

    assert name == expected_name

    name
  end

  @doc """
  I register a process in the registry and then lookup its pid.
  """
  @spec find_pid_of_process(Id.t()) :: :ok
  def find_pid_of_process(node_id \\ Id.new_keypair()) do
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
  @spec list_engines_for_node(Id.t()) :: :ok
  def list_engines_for_node(node_id \\ Id.new_keypair()) do
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
    assert Enum.sort(Registry.engines_for(node_id.external)) == engines

    for engine_pid <- engine_pids do
      send(engine_pid, :stop)
    end

    :ok
  end
end
