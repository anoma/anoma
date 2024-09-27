defmodule Anoma.Crypto.Encrypt do
  @type public() :: box_public()
  @type secret() :: box_secret()

  @type box_public() :: iolist()
  @type box_secret() :: iolist()

  @spec new_keypair() :: %{public: box_public(), secret: box_secret()}
  def new_keypair() do
    {pub, priv} = :crypto.generate_key(:rsa, {2048, 65537})
    %{public: pub, secret: priv}
  end

  @doc """
  I seal the given message for the publicly known recipient.

  The message will be turned into binary via :erlang.term_to_binary,
  so please do not turn it to binary before hand.
  """
  @spec seal(any(), box_public()) :: binary()
  def seal(message, pub) do
    :crypto.public_encrypt(:rsa, :erlang.term_to_binary(message), pub, [])
  end

  @spec unseal(binary(), box_public(), box_secret()) ::
          {:ok, any()} | {:error, :failed_verification}
  def unseal(cipher, _pub, sec) do
    try do
      :erlang.binary_to_term(:crypto.private_decrypt(:rsa, cipher, sec, []))
    rescue
      _e in ArgumentError -> {:error, :failed_verification}
    end
  end
end
