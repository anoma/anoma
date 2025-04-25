defmodule Anoma.Node.Registry do
  @moduledoc """
  I am the Node Registry module.

  I provide functionality for creating addresses, registering engines to
  specific addresses, and querying information regarding the registered
  addresses and engines.

  ### Public API
  - `address/3`
  - `register/3`
  - `via/3`
  - `whereis/3`
  - `engines_for/1`
  - `local_node_id/0`
  - `dump_register/0`
  """
  use TypedStruct

  typedstruct enforce: true, module: Address do
    @typedoc """
    I represent the Anoma Address.

    ### Fields
    - `:node_id`         - The node id.
    - `:engine`          - The engine name.
    - `:label`           - The label. Default: nil.
    """
    field(:node_id, String.t())
    field(:engine, atom())
    field(:label, atom(), default: nil)
  end

  @doc """
  I create an address with specified node id, engine name and label.
  """
  @spec address(String.t(), atom(), atom()) :: Address.t()
  def address(node_id, engine, label \\ nil) do
    %Address{node_id: node_id, engine: engine, label: label}
  end

  @doc """
  I register the calling process under the name of the engine at the given node id.
  """
  @spec register(String.t(), atom(), atom()) :: {:ok, pid} | {:error, term}
  def register(node_id, engine, label \\ nil) do
    address = address(node_id, engine, label)
    Elixir.Registry.register(__MODULE__, address, nil)
  end

  @doc """
  I generate the :via stanza for a process with the given node_id and engine name.
  """
  @spec via(String.t(), atom(), atom()) ::
          {:via, Registry, {atom(), Address.t()}}
  def via(node_id, engine, label \\ nil) do
    address = address(node_id, engine, label)

    {:via, Registry, {__MODULE__, address}}
  end

  @doc """
  Given a node id and engine, I check if there is a process registered with that address
  and return the pid.
  """
  @spec whereis(String.t(), atom(), atom()) :: pid() | nil
  def whereis(node_id, engine, label \\ nil) do
    address = address(node_id, engine, label)

    case Registry.lookup(__MODULE__, address) do
      [{pid, _}] -> pid
      [] -> nil
    end
  end

  @doc """
  Given a node_id, I return a list of all the engine names I know for this node.

  I filter out all the non-engine names based on an allow list.
  """
  @spec engines_for(String.t()) :: [atom()]
  def engines_for(node_id) do
    pattern = {%{node_id: :"$1", engine: :"$2"}, :"$3", :"$4"}

    # guards: filters applied on the results

    guards = [{:==, :"$1", node_id}]

    # shape: the shape of the results the registry should return
    shape = [:"$2"]

    Registry.select(__MODULE__, [{pattern, guards, shape}])
    |> Enum.filter(
      &(&1 in [
          Anoma.Node.Transaction.Mempool,
          Anoma.Node.Intents.IntentPool,
          Client
        ])
    )
    |> Enum.sort()
  end

  @doc """
  Given an engine type, I return the name of the local engine of that type.

  If there are multiple engines of that type present, I raise an error.
  """
  @spec local_node_id() :: {:ok, any()} | {:error, term()}
  def local_node_id() do
    pattern = {%{node_id: :"$1"}, :"$2", :"$3"}

    # shape: the shape of the results the registry should return
    shape = [:"$1"]

    Registry.select(__MODULE__, [{pattern, [], shape}])
    |> Enum.uniq()
    |> case do
      [] ->
        {:error, :no_node_running}

      [node_id] ->
        {:ok, node_id}

      [_, _ | _] ->
        {:error, :multiple_nodes}
    end
  end

  @doc """
  I return the contents of the registry.
  """
  @spec dump_register() :: [{Address.t(), pid(), any()}]
  def dump_register() do
    Registry.select(__MODULE__, [
      {{:"$1", :"$2", :"$3"}, [], [{{:"$1", :"$2", :"$3"}}]}
    ])
    |> Enum.sort()
  end
end
