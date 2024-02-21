defmodule Anoma.Crypto.Encrypt do
  @type public() :: box_public()
  @type secret() :: box_secret()

  @type box_public() :: <<_::256>>
  @type box_secret() :: <<_::256>>

  @spec new_keypair() :: %{public: box_public(), secret: box_secret()}
  def new_keypair() do
    :enacl.box_keypair()
  end

  @doc """
  I seal the given message for the publicly known recipient.

  The message will be turned into binary via :erlang.term_to_binary,
  so please do not turn it to binary before hand.
  """
  @spec seal(any(), box_public()) :: binary()
  def seal(message, pub) do
    :enacl.box_seal(:erlang.term_to_binary(message), pub)
  end

  @spec unseal(binary(), box_public(), box_secret()) ::
          {:ok, any()} | {:error, :failed_verification}
  def unseal(cipher, pub, sec) do
    try do
      with {:ok, encrypt} <- :enacl.box_seal_open(cipher, pub, sec) do
        {:ok, :erlang.binary_to_term(encrypt)}
      end
    rescue
      _e in ArgumentError -> {:error, :failed_verification}
    end
  end
end
