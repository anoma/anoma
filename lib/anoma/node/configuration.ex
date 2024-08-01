defmodule Anoma.Node.Configuration do
  @moduledoc """
  I am the implementation of the Configuration Engine.

  I remember the configuration that the application was launched with as
  well as call the snapshotting and snapshot-deletion functionality.

  ### Public API
  I have the following public functionality:

  - `snapshot/1`
  - `delete_dump/1`
  """

  alias __MODULE__
  alias Anoma.Node.Router
  alias Anoma.Node.Logger

  use TypedStruct
  use Router.Engine

  typedstruct do
    @typedoc """
    I am the type of the Configuration Engine.

    ### Fields

    - `:configuration` - The configuration data stored in specified format.
                         Please consult the
                         `t:Anoma.Configuration.configuration_map/0`.
                         Enforced: true.
    - `:logger` - The address of the Logger Engine. Enforced: false.
    """

    field(:configuration, Anoma.Configuration.configuration_map(),
      enforce: true
    )

    field(:logger, Router.Addr.t(), enforce: false)
  end

  @doc """
  I am the Configuration Engine initialization function.

  I receive a Configuration.t() structure and launch the engine instance
  with the fed-in state.
  """

  @spec init(Configuration.t()) :: {:ok, Configuration.t()}
  def init(%__MODULE__{} = state) do
    {:ok, state}
  end

  ############################################################
  #                      Public RPC API                      #
  ############################################################

  @doc """
  I am the snapshot function.

  I take a snapshots of the current state. The topic sends back a message
  to the caller saying `:snapshot_done`.

  The path for the snapshot is taken directly from the configuration map.
  """

  @spec snapshot(Router.addr()) :: :ok
  def snapshot(config) do
    Router.cast(config, :snapshot)
  end

  @doc """
  I am the function deleting the snapshot file.

  I check the dump path and check whether there is any file snapshot there.
  If so, I delete it, otherwise I do nothing.
  """

  @spec delete_dump(Router.addr()) :: :ok
  def delete_dump(config) do
    Router.cast(config, :delete_dump)
  end

  ############################################################
  #                    Genserver Behavior                    #
  ############################################################

  def handle_cast(:snapshot, _caller, config = %__MODULE__{}) do
    do_snapshot(config)

    {:noreply, config}
  end

  def handle_cast(:delete_dump, _from, config = %__MODULE__{}) do
    do_delete(config)

    {:noreply, config}
  end

  ############################################################
  #                  Genserver Implementation                #
  ############################################################

  @spec do_snapshot(Configuration.t()) :: :ok | pid()
  defp do_snapshot(config) do
    configuration = config.configuration
    logger = config.logger
    dump_path = configuration["dump"]["dump"]
    node_name = configuration["node"]["name"] |> String.to_atom()

    if configuration do
      spawn(fn ->
        case Anoma.Dump.dump_full_path(dump_path, node_name) do
          {:ok, :ok} ->
            log({:dump_ok, dump_path, node_name, logger})

          {:error, reason} ->
            log({:dump_error, dump_path, node_name, reason, logger})
        end
      end)
    else
      log({:no_config, config.logger})
    end
  end

  @spec do_delete(Configuration.t()) :: :ok | {:error, atom()}
  defp do_delete(config) do
    configuration = config.configuration

    if configuration do
      File.rm(configuration["dump"]["dump"])
    else
      log({:no_config, config.logger})
    end
  end

  ############################################################
  #                     Logging Info                         #
  ############################################################

  defp log({:dump_ok, dump_path, node_name, logger}) do
    Logger.add(
      logger,
      :info,
      "Dump successful. Snapshot path: #{inspect(dump_path)}. Node name: #{inspect(node_name)}"
    )
  end

  defp log({:dump_error, dump_path, node_name, reason, logger}) do
    Logger.add(
      logger,
      :error,
      "Dump failed. Snapshot path: #{inspect(dump_path)}.
      Node name: #{inspect(node_name)}. Reason: #{inspect(reason)}"
    )
  end

  defp log({:no_config, logger}) do
    Logger.add(
      logger,
      :error,
      "No configuration provided, Not performing action"
    )
  end
end
