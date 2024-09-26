defmodule Anoma.Node.Transport.MessageEncoding do
  @moduledoc """
  I contain logic to encode and decode the bytes coming in over a network connection.

  ### Public API

  I provide the following public functionality:

  - `decode_bytes/1`
  - `encode_message/1`
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
  I encode any term into a binary.
  """
  @spec encode_message(term()) :: binary()
  def encode_message(term) do
    :erlang.term_to_binary(term)
  end
end
