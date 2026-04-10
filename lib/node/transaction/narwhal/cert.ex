defmodule Anoma.Node.Transaction.Narwhal.Cert do
  @moduledoc """
  I am a Narwhal certificate of availability.

  I prove that a quorum of validators have seen and signed a block.
  Signatures are stored in a map keyed by public key for automatic
  dedup by validator.

  The block itself is stored separately in mnesia (via
  `Narwhal.Supervisor.store_block/3`). I only carry the
  block digest, matching the paper's definition.

  ### Public API

  - `add_signature/3`     - Add a validator's signature.
  - `has_quorum?/2`       - Check if quorum threshold is met.
  - `verified_quorum?/3`  - Verify signatures cryptographically.

  ### Fields

  - `:block_digest` - SHA-256 digest of the certified block.
  - `:validator`    - The block creator's public key.
  - `:round`        - The round this certificate belongs to.
  - `:signatures`   - Map of `public_key => signature`.
  """

  alias Anoma.Crypto.Sign
  alias Anoma.Node.Transaction.Narwhal.Config

  use TypedStruct

  typedstruct do
    field(:block_digest, binary(), enforce: true)
    field(:validator, binary(), enforce: true)
    field(:round, non_neg_integer(), default: 0)

    field(
      :signatures,
      %{binary() => binary()},
      default: %{}
    )
  end

  @doc """
  I add a signature to the certificate, deduplicating by
  validator public key. The first signature per validator
  wins — subsequent signatures from the same key are
  ignored.
  """
  @spec add_signature(t(), binary(), binary()) :: t()
  def add_signature(cert, pub_key, signature) do
    %{cert | signatures: Map.put_new(cert.signatures, pub_key, signature)}
  end

  @doc """
  I check whether the certificate has reached the given
  quorum threshold. I do NOT verify signatures — use
  `verified_quorum?/3` for cryptographic verification.
  """
  @spec has_quorum?(t(), pos_integer()) :: boolean()
  def has_quorum?(cert, threshold) do
    map_size(cert.signatures) >= threshold
  end

  @doc """
  I verify that the certificate has >= threshold valid
  signatures from validators in the given set.

  Each signature is checked with Ed25519 verify_detached
  against the cert's block_digest.
  """
  @spec verified_quorum?(t(), pos_integer(), MapSet.t(binary())) :: boolean()
  def verified_quorum?(cert, threshold, validator_set) do
    cert.signatures
    |> Enum.count(fn {pub_key, signature} ->
      MapSet.member?(validator_set, pub_key) and
        Sign.verify_detached(
          signature,
          cert.block_digest,
          pub_key
        )
    end)
    |> Kernel.>=(threshold)
  end

  @doc """
  I check whether the certificate is valid for the given config:
  the validator is in the set and the cert has a verified quorum.
  """
  @spec valid_for_config?(t(), Config.t()) :: boolean()
  def valid_for_config?(cert = %__MODULE__{validator: v}, config) do
    MapSet.member?(config.validator_set, v) and
      verified_quorum?(cert, Config.quorum(config), config.validator_set)
  end
end
