defmodule AnomaTest.Identity.Decryption do
  use ExUnit.Case, async: true

  alias Anoma.Identity.{Encryption}
  alias Anoma.Node.Identity.Decryption
  alias Anoma.Crypto.Id

  doctest(Anoma.Identity.Encryption)
  doctest(Anoma.Node.Identity.Decryption)

  test "basic encryption and decryption works" do
    id = Id.new_keypair()
    fields = {id.internal.encrypt, id.external.encrypt, id.kind_encrypt}
    {:ok, dpid} = Decryption.start_link(fields)
    cipher = Encryption.seal(555, id.external, false)
    assert {:ok, 555} == Decryption.decrypt(dpid, cipher)
  end

  test "Trying to decrypt made up encryptions fails" do
    id = Id.new_keypair()
    fields = {id.internal.encrypt, id.external.encrypt, id.kind_encrypt}
    {:ok, dpid} = Decryption.start_link(fields)
    assert {:error, :failed_verification} == Decryption.decrypt(dpid, <<3>>)
  end
end
