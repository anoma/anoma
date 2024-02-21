defmodule AnomaTest.Identity.SignsFor do
  use ExUnit.Case, async: true

  alias Anoma.Storage
  alias Anoma.Node.Identity.Commitment
  alias Anoma.Crypto.Id
  alias Anoma.Identity.{SignsFor, Verification}
  alias Anoma.Crypto.Id

  doctest(Anoma.Identity.SignsFor)

  setup_all do
    # base storage testing default
    storage = %Storage{
      qualified: AnomaTest.Identity.SignsFor.Qualified,
      order: AnomaTest.Identity.SignsFor.Order
    }

    Storage.ensure_new(storage)
    [st: storage]
  end

  test "proper signature API", %{st: storage} do
    pair = Id.new_keypair()
    our_id = pair.external
    can_sign_for_me = Id.new_keypair().external
    can_not_sign_for_me = Id.new_keypair().external

    encapsulated = {pair.internal.sign, :ed25519}
    {:ok, cpid} = Commitment.start_link(encapsulated)

    {:ok, signed_key} = Commitment.commit(cpid, can_sign_for_me)

    assert SignsFor.sign_for(storage, our_id, can_sign_for_me, signed_key) ==
             :ok

    assert SignsFor.sign_for(storage, our_id, can_not_sign_for_me, signed_key) ==
             :key_not_verified

    assert SignsFor.known(storage, our_id) == MapSet.new([can_sign_for_me])
    assert SignsFor.known(storage, can_sign_for_me) == MapSet.new([])
    assert SignsFor.signs_for?(storage, our_id, can_sign_for_me)
    refute SignsFor.signs_for?(storage, our_id, can_not_sign_for_me)
  end

  test "transitive signature", %{st: storage} do
    pair = Id.new_keypair()
    our_id = pair.external
    other_id = Id.new_keypair()
    other_other_id = Id.new_keypair()
    can_sign_for_me = other_id.external
    can_also_sign_for_me = other_other_id.external

    {:ok, cpid} = Commitment.start_link({pair.internal.sign, :ed25519})
    {:ok, cpid_o} = Commitment.start_link({other_id.internal.sign, :ed25519})

    {:ok, cpid_oo} =
      Commitment.start_link({other_other_id.internal.sign, :ed25519})

    {:ok, signed_key} = Commitment.commit(cpid, can_sign_for_me)

    assert SignsFor.sign_for(storage, our_id, can_sign_for_me, signed_key) ==
             :ok

    {:ok, signed} = Commitment.commit(cpid_o, <<3>>)
    # Verify data signed by someone who signs for us
    assert Verification.verify_request(signed, <<3>>, our_id, storage)

    # Add an indirection signs for request
    {:ok, signed_key} = Commitment.commit(cpid_o, can_also_sign_for_me)

    assert SignsFor.sign_for(
             storage,
             can_sign_for_me,
             can_also_sign_for_me,
             signed_key
           ) ==
             :ok

    {:ok, signed} = Commitment.commit(cpid_oo, <<3>>)
    {:ok, signed_blob} = Commitment.commit_combined(cpid_oo, <<3>>)
    # Verify data signed by someone who signs for us
    assert Verification.verify_request(signed, <<3>>, our_id, storage)
    assert Verification.verify_combined(signed_blob, our_id, storage)
  end
end
