defmodule Anoma.Identity.Parameters do
  @moduledoc """
  I specify what parameters to use when generating a new identity.
  """

  @type t() :: :ed25519 | :secp256k1 | :bls
end
