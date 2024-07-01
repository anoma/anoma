defmodule Examples.EIdentity do
  use Memoize

  require ExUnit.Assertions
  import ExUnit.Assertions

  alias Anoma.Crypto.Id
  alias Anoma.Symbol
  alias Anoma.Identity.{Backend.Memory, Encryption, Verification, Manager}
  alias Anoma.Node
  alias Anoma.Node.{Identity.Decryption, Identity.Commitment, Router}

  alias Examples.{ECrypto, ENode.EStorage}

  @spec alice_commits() :: GenServer.server()
  def alice_commits() do
    akey = ECrypto.alice()
    assert {:ok, cpid} = Commitment.start_link({akey.internal.sign, :ed25519})
    assert {:ok, data} = Commitment.commit(cpid, 555)

    assert Verification.verify_request(data, 555, akey.external),
           "should verify the same data"

    refute Verification.verify_request(data, <<555>>, akey.external),
           "Should not verify unrelated data"

    assert {:ok, data} = Commitment.commit_combined(cpid, <<55>>)

    assert {:ok, <<55>>} == Verification.verify_combined(data, akey.external),
           "should verify the same data coming in, and give back uncombined values"

    refute Verification.verify_combined(<<5>>, akey.external),
           "Fails on unrelated data"

    cpid
  end

  @spec alice_decrypts() :: GenServer.server()
  def alice_decrypts() do
    akey = ECrypto.alice()
    fields = {akey.internal.encrypt, akey.external.encrypt, akey.kind_encrypt}
    assert {:ok, dpid} = Decryption.start_link(fields)

    cipher = Encryption.seal(555, akey.external, false)
    assert {:ok, 555} == Decryption.decrypt(dpid, cipher)
    assert {:error, :failed_verification} == Decryption.decrypt(dpid, <<3>>)

    dpid
  end

  @spec failure_to_connect() :: any()
  @spec failure_to_connect(Symbol.s()) :: any()
  def failure_to_connect(storage_name \\ "failure_to_connect") do
    mem = memory_key(EStorage.empty_storage(storage_name).storage)
    err = Manager.connect(ECrypto.alice().external, mem, :commit_and_decrypt)

    assert {:error, _} = err,
           "Can not connect to a key that isn't in storage"

    err
  end

  @spec memory_storage() ::
          {Node.t(), Memory.t(), Manager.instance(), Id.Extern.t()}
  @spec memory_storage(Symbol.s()) ::
          {Node.t(), Memory.t(), Manager.instance(), Id.Extern.t()}
  def memory_storage(storage_name \\ "memory_storage") do
    anode = EStorage.empty_storage(storage_name)
    mem = memory_key(anode.storage)

    assert {:ok, {eng, pub}} =
             Manager.generate(mem, :ed25519, :commit_and_decrypt)

    assert is_pid(Map.get(eng, :decryption)) and
             is_pid(Map.get(eng, :commitment)),
           "Both the commitment and decryption engine should be generated"

    assert {:ok, _} = Manager.connect(pub, mem, :commit_and_decrypt),
           "Can connect to a previously generated id"

    assert {:ok, c} = Commitment.commit(eng.commitment, 555)
    assert Verification.verify_request(c, 555, pub)

    {anode, mem, eng, pub}
  end

  @spec memory_storage_connected_engines() ::
          {Node.t(), Memory.t(), Manager.instance(), Id.Extern.t()}
  @spec memory_storage_connected_engines(Symbol.s()) ::
          {Node.t(), Memory.t(), Manager.instance(), Id.Extern.t()}
  def memory_storage_connected_engines(storage_name \\ "memory_storage_con") do
    {anode, mem, _eng, pub} = memory_storage(storage_name)

    assert {:ok, con_eng} = Manager.connect(pub, mem, :commit_and_decrypt)

    assert {:ok, c} = Commitment.commit(con_eng.commitment, 555)
    assert Verification.verify_request(c, 555, pub)

    {anode, mem, con_eng, pub}
  end

  @spec no_memory_storage() ::
          {Node.t(), Memory.t(), Id.Extern.t()}
  @spec no_memory_storage(Symbol.s()) ::
          {Node.t(), Memory.t(), Id.Extern.t()}
  def no_memory_storage(storage_name \\ "memory_storage_con") do
    {anode, mem, _eng, pub} = memory_storage(storage_name)
    Manager.delete(pub, mem)

    assert {:error, _} = Manager.connect(pub, mem, :commit_and_decrypt),
           "Deleting the key from storage makes it un-connectable"

    {anode, mem, pub}
  end

  @spec same_id_multiple_times() ::
          {Node.t(), Memory.t(), Manager.instance(), Id.Extern.t()}
  @spec same_id_multiple_times(Symbol.s()) ::
          {Node.t(), Memory.t(), Manager.instance(), Id.Extern.t()}
  def same_id_multiple_times(storage_name \\ "multiple_times") do
    {anode, mem, pub} = no_memory_storage(storage_name)

    assert {:ok, {com, pub1}} = Manager.generate(mem, :ed25519, :commit),
           "We can regenerate out an id and override an old one"

    assert {:ok, {dec, pub2}} = Manager.generate(mem, :ed25519, :decrypt),
           "We can regenerate out an id and override an old one"

    assert pub1 != pub2 && pub != pub2,
           "Generating out a new key will replace the one in memory"

    refute Map.has_key?(com, :decryption) || Map.has_key?(dec, :commitment)
    assert Map.has_key?(com, :commitment) && Map.has_key?(dec, :decryption)

    eng = %{commitment: com.commitment, decryption: dec.decryption}

    {anode, mem, eng, pub}
  end

  ####################################################################
  ##                             Phase 1                            ##
  ####################################################################

  @spec memory_key(Router.addr()) :: Memory.t()
  def memory_key(storage) do
    %Memory{symmetric: ECrypto.xcc(), storage: storage, nonce: <<>>}
  end

  # Just reuse EStorage.anode()!
end
