defmodule Anoma.Node.Transaction.Narwhal.Block do
  @moduledoc """
  I am a Narwhal block.

  I am proposed by a Primary and contain batch digests from the
  Worker, certificates from the previous round, and the creator's
  signature.

  ### Public API

  - `digest/1` - SHA-256 hash of the block (excludes signature).
  - `sign/2`   - Sign a block with a secret key.
  - `valid?/1` - Verify the creator's signature.

  ### Fields

  - `:batch_digests` - SHA-256 hashes of transaction batches.
  - `:certificates`  - Certificates from round r-1.
  - `:round`         - The round this block belongs to.
  - `:creator`       - The creator's public key.
  - `:signature`     - The creator's Ed25519 signature.
  """

  alias Anoma.Crypto.Sign

  use TypedStruct

  typedstruct do
    field(:batch_digests, [binary()], default: [])

    field(:certificates, [Anoma.Node.Transaction.Narwhal.Cert.t()],
      default: []
    )

    field(:round, non_neg_integer(), default: 0)
    field(:creator, binary())
    field(:signature, binary())
  end

  @spec digest(t()) :: binary()
  def digest(block), do: :crypto.hash(:sha256, signable_binary(block))

  @spec sign(t(), binary()) :: t()
  def sign(block, secret_key) do
    %{
      block
      | signature: Sign.sign_detached(signable_binary(block), secret_key)
    }
  end

  @spec valid?(t()) :: boolean()
  def valid?(%__MODULE__{signature: nil}), do: false

  def valid?(block) do
    Sign.verify_detached(
      block.signature,
      signable_binary(block),
      block.creator
    )
  end

  @spec signable_binary(t()) :: binary()
  defp signable_binary(block) do
    block
    |> Map.from_struct()
    |> Map.drop([:signature])
    # `:deterministic` canonicalizes map key order so the bytes are
    # identical on every node. Without it a block signed/digested on
    # its creator fails verification after crossing a VM boundary,
    # because the receiver's map layout (and thus default
    # term_to_binary) differs -- which silently stalls cross-VM
    # consensus.
    |> :erlang.term_to_binary([:deterministic])
  end
end
