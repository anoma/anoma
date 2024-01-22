defmodule Anoma.Sign do
  @moduledoc false

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

  @spec verify(binary, ed25519_public()) :: {:ok, binary()} | {:error, term()}
  def verify(signed_message, public) do
    :enacl.sign_open(signed_message, public)
  end
end
