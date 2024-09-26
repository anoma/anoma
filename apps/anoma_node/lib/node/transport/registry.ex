defmodule Anoma.Node.Transport.Registry do
  @moduledoc """
  I contain logic to deal with the registry of processes.

  I contain logic to register a process locally, lookup a process locally, or generate the
  key to give to a process.

  """
  def registry_name(node_id) do
    :"registry#{:erlang.phash2(node_id)}"
  end

  @doc """
  I create a key to store a process in the registry.

  ### Pattern-Matching Variations

  - `key/3` - I create a key for a local process such as the router, mempool, etc.
  - `key/4` - I create a key for a remote process such as a remote router, remote mempool, etc.
  """

  def key(local_node_id, remote_node_id, name, value \\ nil) do
    key = %{node: remote_node_id, name: name, label: nil}

    if value == nil do
      {:via, Registry, {registry_name(local_node_id), key}}
    else
      {:via, Registry, {registry_name(local_node_id), key, value}}
    end
  end

  def dump_register(node_id) do
    registry_name = :"registry#{:erlang.phash2(node_id)}"

    Registry.select(registry_name, [
      {{:"$1", :"$2", :"$3"}, [], [{{:"$1", :"$2", :"$3"}}]}
    ])
    |> Enum.sort()
  end

  def register(local_node_id, remote_node_id, name, label \\ nil) do
    registry_name = :"registry#{:erlang.phash2(local_node_id)}"

    key = %{node: remote_node_id, name: name, label: label}
    Elixir.Registry.register(registry_name, key, nil)
  end

  @doc """
  I lookup the process id for a given process name.
  """
  def lookup(local_node_id, remote_node_id, name, label \\ nil) do
    registry_name = :"registry#{:erlang.phash2(local_node_id)}"

    # match pattern: this pattern should match the values in the registry
    # at least {:"$1", :"$2", :"$3"} ({key, pid, value})
    pattern = {%{node: :"$1", name: :"$2", label: :"$3"}, :"$4", :"$5"}

    # guards: filters applied on the results
    label_guard =
      if label == nil do
        []
      else
        [{:==, :"$3", label}]
      end

    guards = [{:==, :"$2", name}, {:==, :"$1", remote_node_id}] ++ label_guard

    # shape: the shape of the results the registry should return
    shape = [{{:"$1", :"$2", :"$3", :"$4", :"$5"}}]

    Elixir.Registry.select(registry_name, [{pattern, guards, shape}])
  end

  def lookup_by_label(local_node_id, remote_node_id, label) do
    registry_name = :"registry#{:erlang.phash2(local_node_id)}"

    # match pattern: this pattern should match the values in the registry
    # at least {:"$1", :"$2", :"$3"} ({key, pid, value})
    pattern = {%{node: :"$1", name: :"$2", label: :"$3"}, :"$4", :"$5"}

    # guards: filters applied on the results

    guards =
      [{:==, :"$1", remote_node_id}, {:==, :"$3", label}]

    # shape: the shape of the results the registry should return
    shape = [{{:"$1", :"$2", :"$3", :"$4", :"$5"}}]

    Elixir.Registry.select(registry_name, [{pattern, guards, shape}])
  end
end
