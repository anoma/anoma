defmodule Anoma.LocalDomain.Applications.Sample do
  @moduledoc """
  A sample local domain application: a fake wallet.

  API:
    Sample.store(id, privkey) stores the string privkey as id's private key
    Sample.privkey(id) gets id's private key
    Sample.pubkey(id) gets id's public key (the string with "PUBLIC_"
    prepended. this is all fake, remember)
  """

  def store(id, privkey) do
    key = ["sample", id]
    Anoma.LocalDomain.Storage.write_local(key, privkey)
  end

  def privkey(id) do
    key = ["sample", id]
    Anoma.LocalDomain.Storage.read_local(key)
  end

  def pubkey(id) do
    key = ["sample", id]
    privkey = Anoma.LocalDomain.Storage.read_local(key)

    "PUBLIC_" <> privkey
  end

  def init() do
    Anoma.LocalDomain.HandlerRegistry.register(
      {[["anoma", "local"]], ["sample"]}, &scry/2
    )
  end

  def scry(_, _) do
    "scried value"
  end
end
