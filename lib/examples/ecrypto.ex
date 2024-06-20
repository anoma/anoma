defmodule Examples.ECrypto do
  use Memoize

  require ExUnit.Assertions
  import ExUnit.Assertions

  alias Anoma.Crypto.Symmetric
  alias Anoma.Crypto.Id

  ####################################################################
  ##                              IDs                               ##
  ####################################################################

  @spec alice() :: Id.t()
  defmemo alice() do
    Id.new_keypair()
  end

  @spec bertha() :: Id.t()
  defmemo bertha() do
    Id.new_keypair()
  end

  @spec eve() :: Id.t()
  defmemo eve() do
    Id.new_keypair()
  end

  @spec londo() :: Id.t()
  defmemo londo() do
    keys = Id.new_keypair()

    assert keys == keys |> Id.salt_keys(xcc()) |> Id.unsalt_keys(xcc()),
           "unsalt · salt ≡ identity"

    keys
  end

  ####################################################################
  ##                           Symmetric                            ##
  ####################################################################

  @spec xcc() :: Symmetric.t()
  defmemo xcc() do
    sym = Symmetric.random_xchacha()

    assert 555 == Symmetric.encrypt(555, sym) |> Symmetric.decrypt(sym),
           "decrypt · encrypt ≡ identity"

    sym
  end
end
