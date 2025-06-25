defmodule Anoma.LocalDomain.Applications.Sample do
  @moduledoc """
  I am a sample local domain application: a fake wallet.

  API:
    Sample.store(id, privkey) stores the string privkey as id's private key
    Sample.privkey(id) gets id's private key
    Sample.pubkey(id) gets id's public key (the string with "PUBLIC_"
    prepended. this is all fake, remember)
  """

  use Anoma.LocalDomain.Application, name: "sample"

  def store(id, privkey) do
    key = ~k"/sample/!id"
    # todo: provide a write api
    Anoma.LocalDomain.Storage.write_local(key, privkey)
  end

  def privkey(id) do
    # todo: finish scry
    key = ~k"/sample/!id"
    Anoma.LocalDomain.Storage.read_local(key)
  end

  def pubkey(id) do
    key = ~k"/sample/!id"
    privkey = Anoma.LocalDomain.Storage.read_local(key)

    "PUBLIC_" <> privkey
  end

  @impl true
  def scry(_, ~k"ray") do
    {:ok, "special result for ray"}
  end

  @impl true
  def scry(prev, key) do
    super(prev, key)
  end
end
