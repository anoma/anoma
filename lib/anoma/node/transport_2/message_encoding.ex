defmodule Anoma.Node.Transport2.MessageEncoding do
  @moduledoc """
  I contain logic to encode and decode the bytes coming in over a network connection.
  """

  @doc """
  I decode a binary message into a proper term.
  """
  @spec decode_bytes(binary) :: {:ok, term()} | {:error, :failed_to_decode}
  def decode_bytes(bytes) do
    try do
      {:ok, :erlang.binary_to_term(bytes)}
    rescue
      _e in ArgumentError -> {:error, :failed_to_decode}
    end
  end

  @doc """
  I encode any term into a binary message for the socket.
  """
  @spec encode_message(term()) :: binary()
  def encode_message(term) do
    :erlang.term_to_binary(term)
  end
end
