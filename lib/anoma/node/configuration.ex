defmodule Anoma.Node.Configuration do
  @moduledoc """

  I am an agent that remembers the configuration that the application
  was launched with

  ### Public API

  I have the following public functionality:

  - `get_time/1`
  - `get_epoch/1`
  """

  alias __MODULE__
  alias Anoma.Node.Router
  alias Anoma.Configuration
  alias Anoma.Node.Logger

  use TypedStruct
  use Router.Engine

  typedstruct do
    field(:configuration, Configuration.configuration_map(), enforce: true)
    field(:logger, Router.Addr.t(), enforce: false)
  end

  def init(%__MODULE__{} = state) do
    {:ok, state}
  end

  ############################################################
  #                      Public RPC API                      #
  ############################################################

  @doc """
  Takes a snapshots of the current state

  This topic sends back a message to the caller saying :snapshot_done
  """
  @spec snapshot(Router.addr()) :: :ok
  def snapshot(config) do
    Router.cast(config, :snapshot)
  end

  @spec delete_dump(Router.addr()) :: :ok
  def delete_dump(config) do
    Router.cast(config, :delete_dump)
  end

  ############################################################
  #                    Genserver Behavior                    #
  ############################################################
  def handle_cast(:snapshot, _caller, config = %__MODULE__{}) do
    configuration = config.configuration
    logger = config.logger
    dump_path = configuration["dump"]["dump"]
    node_name = configuration["node"]["name"] |> String.to_atom()

    if configuration do
      spawn(fn ->
      log_info({:start_dump, config.logger})
        case Anoma.Dump.dump_full_path(dump_path, node_name) do
          {:ok, :ok} ->
            log_info({:dump_ok, dump_path, node_name, logger})

          {:error, reason} ->
            log_info({:dump_error, dump_path, node_name, reason, logger})
        end
      end)
    else
      log_info({:no_config, config.logger})
    end

    {:noreply, config}
  end

  def handle_cast(:delete_dump, _from, config = %__MODULE__{}) do
    configuration = config.configuration

    if configuration do
      File.rm(configuration["dump"]["dump"])
    else
      log_info({:no_config, config.logger})
    end

    {:noreply, config}
  end

  ############################################################
  #                     Logging Info                         #
  ############################################################

  defp log_info({:dump_ok, dump_path, node_name, logger}) do
    Logger.add(
      logger,
      :info,
      "Dump succesfull. Snapshot path: #{inspect(dump_path)}. Node name: #{inspect(node_name)}"
    )
  end

  defp log_info({:dump_error, dump_path, node_name, reason, logger}) do
    Logger.add(
      logger,
      :error,
      "Dump failed. Snapshot path: #{inspect(dump_path)}.
      Node name: #{inspect(node_name)}. Reason: #{inspect(reason)}"
    )
  end

  defp log_info({:start_dump, logger}) do
    Logger.add(
      logger,
      :info,
      "Started dump"
    )
  end

  defp log_info({:no_config, logger}) do
    Logger.add(
      logger,
      :error,
      "No configuration provided, Not performing action"
    )
  end
end
