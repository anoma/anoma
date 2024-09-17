defmodule Anoma.Node.Transport2 do
  @moduledoc """

  """

  alias Anoma.Node.Transport2.ProxySupervisor
  alias Anoma.Node.Transport2.TCPClient
  alias Anoma.Node.Transport2.TCPServer
  alias Anoma.Node.Transport2.TCPSupervisor

  ############################################################
  #                           Registry                       #
  ############################################################

  @doc """
  I create a new proxy engine to talk to remote engines.
  """
  def create_engine_proxy(remote_id) do
    res =
      DynamicSupervisor.start_child(
        ProxySupervisor,
        {EngineProxy, [remote_id]}
      )

    case res do
      {:ok, pid} ->
        {:ok, pid}

      {:ok, pid, _} ->
        {:ok, pid}

      _ ->
        {:error, :failed_to_start_proxy}
    end
  end

  ############################################################
  #                           Client                         #
  ############################################################

  @doc """
  I create a new TCP client
  """
  def start_tcp_client(config) do
    create_connecter(config)
  end

  ############################################################
  #                           Server                         #
  ############################################################

  @doc """
  I create a new TCP listening server under the transport supervisor.
  """
  def start_tcp_server(config) do
    {:ok, listen_socket} = :gen_tcp.listen(config.port, mode: :binary)

    IO.puts("tcp server on port #{inspect(:inet.port(listen_socket))}")

    create_accepter(listen_socket, config.router_key)
  end

  ############################################################
  #                           Helpers                        #
  ############################################################

  defp create_connecter(config) do
    res =
      DynamicSupervisor.start_child(
        TCPSupervisor,
        {TCPClient, [config.host, config.port, config.router_key]}
      )

    case res do
      {:ok, pid} ->
        {:ok, pid}

      {:ok, pid, _} ->
        {:ok, pid}

      _ ->
        {:error, :failed_to_start_connecter}
    end
  end

  def create_accepter(listen_socket, id) do
    res =
      DynamicSupervisor.start_child(
        TCPSupervisor,
        {TCPServer, [listen_socket, id]}
      )

    case res do
      {:ok, pid} ->
        {:ok, pid}

      {:ok, pid, _} ->
        {:ok, pid}

      _ ->
        {:error, :failed_to_start_accepter}
    end
  end
end
