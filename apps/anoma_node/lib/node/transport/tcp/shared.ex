defmodule Anoma.Node.Transport.TCP.Shared do
  @moduledoc """
  I contain logic that is shared between the TCP server and the TCP client.

  ### Public API

  I provide the following public functionality:

  - `handle_connection_established/3`
  - `handle_received_bytes/3`
  - `handle_send/2`
  """
  require Logger

  alias Anoma.Crypto.Id
  alias Anoma.Node.Transport.Discovery
  alias Anoma.Node.Transport.Messages.Handler

  import Anoma.Node.Transport.MessageEncoding

  @doc """
  I handle a new succesful connection.

  When a TCP server of client establish a connection with a remote node, I am evaluated.

  I make the local node announce itself to the remote node.
  """
  @spec handle_connection_established(GenServer.server(), Id.t()) :: :ok
  def handle_connection_established(connection_pid, node_id) do
    Logger.debug("new connection with node")
    Discovery.announce_self(connection_pid, node_id)
    :ok
  end

  @doc """
  I handle incoming binary data by decoding it and then passing it on the the message handler.

  ### Options

  - `:connection_type` - This flag is passed to the message handler in case it needs to register
                         this process as a connection for the remote node.
  """
  @spec handle_received_bytes(binary(), Id.t(), Keyword.t()) ::
          :ok | {:error, :failed_to_decode}
  def handle_received_bytes(bytes, local_node_id, opts \\ []) do
    case decode_bytes(bytes) do
      {:ok, message} ->
        Handler.handle_message(message, local_node_id, opts)

      {:error, :failed_to_decode} ->
        {:error, :failed_to_decode}
    end
  end

  @doc """
  I send a message over the socket to the other node.
  """
  @spec handle_send(term(), :inet.socket()) ::
          {:ok, :sent} | {:error, :failed_to_send}
  def handle_send(message, socket) do
    encoded_message = encode_message(message)

    case :gen_tcp.send(socket, encoded_message) do
      :ok ->
        {:ok, :sent}

      {:error, _} ->
        {:error, :failed_to_send}
    end
  end
end
