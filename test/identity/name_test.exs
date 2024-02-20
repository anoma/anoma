defmodule AnomaTest.Identity.Name do
  use ExUnit.Case, async: true

  alias Anoma.Crypto.Symmetric
  alias Anoma.Storage
  alias Anoma.Crypto.Id
  alias Anoma.Identity.Backend.Memory
  alias Anoma.Identity.{Manager, Name}
  alias Anoma.Node.Identity.Commitment

  doctest(Anoma.Identity.Name)

  setup_all do
    tab = :name_manager_table_test
    Id.initalize(tab)

    key = Symmetric.random_xchacha()

    mem = %Memory{symmetric: key, table: tab}

    # base storage testing default
    storage = %Storage{
      qualified: AnomaTest.Identity.Name.Qualified,
      order: AnomaTest.Identity.Name.Order
    }

    Storage.ensure_new(storage)

    namespace = %Name{storage: storage, keyspace: tab}

    [ns: namespace, st: storage, mem: mem]
  end

  test "Properly reserve the ns", %{ns: namespace, mem: mem, st: storage} do
    Storage.ensure_new(storage)
    {%{commitment: com}, pub} = Manager.generate(mem, :ed25519, :commit)
    {:ok, commited} = Commitment.commit(com, "Alice")
    name = [Name.name_space(), "Alice"]
    assert Name.reserve_namespace(namespace, "Alice", pub, commited) == :ok
    assert Storage.get_keyspace(storage, name) == [{name, pub}]
  end

  test "Improper placement", %{ns: namespace, mem: mem, st: storage} do
    Storage.ensure_new(storage)
    # First pass all good
    {%{commitment: com}, pub} = Manager.generate(mem, :ed25519, :commit)
    {:ok, commited} = Commitment.commit(com, "Alice")
    assert Name.reserve_namespace(namespace, "Alice", pub, commited) == :ok
    # no longer good
    assert Name.reserve_namespace(namespace, "Alice", pub, commited) ==
             :already_there

    assert Name.reserve_namespace(namespace, "A", pub, commited) ==
             :improper_data
  end

  test "Adding to a namespace", %{ns: namespace, mem: mem, st: storage} do
    Storage.ensure_new(storage)
    {%{commitment: com}, pub} = Manager.generate(mem, :ed25519, :commit)
    {:ok, commited} = Commitment.commit(com, "Alice")

    assert Name.reserve_namespace(namespace, "Alice", pub, commited) == :ok

    {_, new_identity} = Manager.generate(mem, :ed25519, :commit)

    keyspace = ["Alice", "Urbit"]

    {:ok, attestation} =
      Commitment.commit(com, {keyspace, new_identity})

    assert Name.add(namespace, attestation, {keyspace, new_identity}) == :ok

    assert Name.add(namespace, attestation, {keyspace, new_identity}) ==
             :already_there

    assert Name.add(namespace, attestation, {keyspace, ["Alice", "Anoma"]}) ==
             :improper_data

    assert Name.add(namespace, attestation, {["Londo", "Narn"], new_identity}) ==
             :no_namespace

    assert Name.all_identities(namespace, "Alice") ==
             MapSet.new([new_identity, pub])
  end
end
