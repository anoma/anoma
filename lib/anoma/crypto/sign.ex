defmodule Anoma.Crypto.Sign do
  @type public() :: ed25519_public()
  @type secret() :: ed25519_secret()

  @type ed25519_public() :: <<_::256>>
  @type ed25519_secret() :: <<_::512>>

  @spec new_keypair() :: %{public: ed25519_public(), secret: ed25519_secret()}
  def new_keypair do
    :enacl.crypto_sign_ed25519_keypair()
  end

  @spec sign(binary(), ed25519_secret()) :: binary()
  def sign(message, secret) do
    :enacl.sign(message, secret)
  end

  @spec sign_detatched(binary(), ed25519_secret()) :: binary()
  def sign_detatched(message, secret) do
    :enacl.sign_detached(message, secret)
  end

  @spec verify(binary, ed25519_public()) :: {:ok, binary()} | {:error, term()}
  def verify(signed_message, public) do
    :enacl.sign_open(signed_message, public)
  end

  @spec verify_detatched(binary, binary, ed25519_public()) :: boolean()
  def verify_detatched(signature, message, public) do
    :enacl.sign_verify_detached(signature, message, public)
  end
end
