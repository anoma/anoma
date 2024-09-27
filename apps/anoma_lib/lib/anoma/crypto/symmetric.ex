defmodule Anoma.Crypto.Symmetric do
  @typedoc """
  I represent the symmetric types available to the system
  """

  alias Anoma.Crypto.Randomness

  @type t() :: chacha()

  # should we include AD data? For our purposes I think not?
  @type xchacha() :: {:xchacha, xchacha_key(), xchacha_nonce()}
  @type xchacha_nonce() :: <<_::192>>
  @type xchacha_key() :: <<_::256>>

  @type chacha() :: {:chacha, xchacha_key(), xchacha_nonce()}
  @type chacha_nonce() :: <<_::96>>
  @type chacha_key() :: <<_::256>>

  @tag_size 16

  @spec random_chacha_nonce() :: chacha_nonce()
  def random_chacha_nonce() do
    Randomness.get_random(12)
  end

  @spec random_chacha_key() :: chacha_key()
  def random_chacha_key(), do: Randomness.get_random(32)

  @spec random_chacha() :: chacha()
  def random_chacha() do
    {:chacha, random_chacha_key(), random_chacha_nonce()}
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
  def encrypt_raw(binary, {:chacha, key, nonce}) do
    {cipher, tag} = :crypto.crypto_one_time_aead(:chacha20_poly1305, key, nonce, binary, <<>>, true)
    tag <> cipher
  end

  @spec decrypt_raw(binary(), t()) :: binary() | {:error, any()}
  def decrypt_raw(<<tag::binary-size(16), message::binary>>, {:chacha, key, nonce}) do
    :crypto.crypto_one_time_aead(:chacha20_poly1305, key, nonce, message, <<>>, tag, false)
  end
end
