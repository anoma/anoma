defmodule Examples.ECrypto do
  use Memoize

  require ExUnit.Assertions

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
    Id.new_keypair()
  end
end
