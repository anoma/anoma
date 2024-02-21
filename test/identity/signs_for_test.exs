defmodule AnomaTest.Identity.SignsFor do
  use ExUnit.Case, async: true

  alias Anoma.Storage
  alias Anoma.Node.Identity.Commitment
  alias Anoma.Crypto.Id
  alias Anoma.Identity.SignsFor
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
end
