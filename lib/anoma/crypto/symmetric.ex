defmodule Anoma.Crypto.Symmetric do
  @typedoc """
  I represent the symmetric types available to the system
  """

  alias Anoma.Crypto.Randomness

  @type t() :: xchacha()

  # should we include AD data? For our purposes I think not?
  @type xchacha() :: {:xchacha, xchacha_key(), xchacha_nonce()}
  @type xchacha_nonce() :: <<_::192>>
  @type xchacha_key() :: <<_::256>>

  @spec random_xchacha_nonce() :: xchacha_nonce()
  def random_xchacha_nonce() do
    Randomness.get_random(:enacl.aead_xchacha20poly1305_ietf_NPUBBYTES())
  end

  @spec random_xchacha_key() :: xchacha_key()
  def random_xchacha_key(), do: :enacl.secretstream_xchacha20poly1305_keygen()

  @spec random_xchacha() :: xchacha()
  def random_xchacha() do
    {:xchacha, random_xchacha_key(), random_xchacha_nonce()}
  end

  @doc """
  I encrypt data given any known schema and a message.

  The message will be turned into binary via :erlang.term_to_binary,
  so please do not turn it to binary before hand.
  """
  @spec encrypt(any(), t()) :: binary()
  def encrypt(message, sym) do
    encrypt_raw(:erlang.term_to_binary(message), sym)
  end

  @spec decrypt(binary(), t()) :: term() | {:error, any()}
  def decrypt(binary, sym) do
    res = decrypt_raw(binary, sym)

    case res do
      b when is_binary(b) -> :erlang.binary_to_term(b)
      x -> x
    end
  end

  @doc """
  I encrypt data given any known schema and a message.

  I am raw in that I do not serialize the data, Only use me if you
  know what you are doing.

  """
  @spec encrypt_raw(binary(), t()) :: binary()
  def encrypt_raw(binary, {:xchacha, key, nonce}) do
    :enacl.aead_xchacha20poly1305_ietf_encrypt(binary, <<>>, nonce, key)
  end

  @spec decrypt_raw(binary(), t()) :: binary() | {:error, any()}
  def decrypt_raw(binary, {:xchacha, key, nonce}) do
    :enacl.aead_xchacha20poly1305_ietf_decrypt(binary, <<>>, nonce, key)
  end
end
