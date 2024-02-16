defmodule AnomaTest.Identity.Manager do
  use ExUnit.Case, async: true

  alias Anoma.Crypto.Symmetric
  alias Anoma.Crypto.Id
  alias Anoma.Identity.Backend.Memory
  alias Anoma.Identity.Manager
  alias Anoma.Node.Identity.Commitment

  doctest(Anoma.Identity.Manager)

  setup_all do
    tab = :manager_table_test
    Id.initalize(tab)

    key = Symmetric.random_xchacha()

    mem = %Memory{symmetric: key, table: tab}
    [tab: tab, key: key, mem: mem]
  end

  test "random key is not can not be connected", %{mem: mem} do
    pair = Id.new_keypair()

    assert {:error, _} =
             Manager.connect(pair.external, mem, :commit_and_decrypt)
  end

  test "Generating Works", %{mem: mem} do
    assert {%{commitment: com, decryption: _}, _} =
             Manager.generate(mem, :ed25519, :commit_and_decrypt)

    assert {:ok, _} = Commitment.commit(com, 555)
  end

  test "Can connect to generated id", %{mem: mem} do
    {_, pub} = Manager.generate(mem, :ed25519, :commit_and_decrypt)

    assert {:ok, %{commitment: com, decryption: _}} =
             Manager.connect(pub, mem, :commit_and_decrypt)

    assert {:ok, _} = Commitment.commit(com, 555)
  end

  test "Proper permissions", %{mem: mem} do
    generate = fn perms -> Manager.generate(mem, :ed25519, perms) end

    assert {%{commitment: _, decryption: _}, _} =
             generate.(:commit_and_decrypt)

    {com, _} = generate.(:commit)
    {dec, _} = generate.(:decrypt)

    refute Map.has_key?(com, :decryption) || Map.has_key?(dec, :commitment)
    assert Map.has_key?(com, :commitment) && Map.has_key?(dec, :decryption)
  end

  test "delete works", %{mem: mem} do
    {_, pub} = Manager.generate(mem, :ed25519, :commit_and_decrypt)
    Manager.delete(pub, mem)
    assert {:error, _} = Manager.connect(pub, mem, :commit_and_decrypt)
  end
end
