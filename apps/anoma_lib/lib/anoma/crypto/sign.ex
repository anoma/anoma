defmodule Anoma.Crypto.Sign do
  @type public() :: ed25519_public()
  @type secret() :: ed25519_secret()

  @type ed25519_public() :: <<_::256>>
  @type ed25519_secret() :: <<_::256>>

  @spec new_keypair() :: %{public: ed25519_public(), secret: ed25519_secret()}
  def new_keypair do
    {pub, priv} = :crypto.generate_key(:eddsa, :ed25519)
    %{public: pub, secret: priv}
  end

  @spec sign(binary(), ed25519_secret()) :: binary()
  def sign(message, secret) do
    sign_detached(message, secret) <> message
  end

  @spec sign_detached(binary(), ed25519_secret()) :: binary()
  def sign_detached(message, secret) do
    :crypto.sign(:eddsa, :none, message, [secret, :ed25519])
  end

  @spec verify(binary, ed25519_public()) :: {:ok, binary()} | {:error, term()}
  def verify(<<sign::binary-size(64), message::binary>>, public) do
    if verify_detached(sign, message, public) do
      {:ok, message}
    else
      {:error, :failed_verification}
    end
  end

  @spec verify_detached(binary, binary, ed25519_public()) :: boolean()
  def verify_detached(signature, message, public) do
    :crypto.verify(:eddsa, :sha256, message, signature, [public, :ed25519])
  end
end
