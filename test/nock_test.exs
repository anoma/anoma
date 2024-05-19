defmodule AnomaTest.Nock do
  use ExUnit.Case, async: true

  import Nock
  import TestHelper.Nock
  alias Anoma.Order
  alias Anoma.Node.Storage
  alias Anoma.Node.Ordering
  alias Anoma.Crypto.Sign

  doctest(Nock)

  setup_all do
    storage = %Storage{
      qualified: AnomaTest.Nock.Qualified,
      order: AnomaTest.Nock.Order
    }

    {:ok, router} = Anoma.Node.Router.start()

    {:ok, storage} =
      Anoma.Node.Router.start_engine(router, Storage, storage)

    # on_exit(fn -> Anoma.Node.Router.stop(router.id) end)
    {:ok, ordering} =
      Anoma.Node.Router.start_engine(router, Ordering, %{
        table: storage
      })

    snapshot_path = [:my_special_nock_snaphsot | 0]

    env = %Nock{snapshot_path: snapshot_path, ordering: ordering}

    [env: env]
  end

  describe "testing jets" do
    test "signing success" do
      %{public: _pub, secret: sec} = Sign.new_keypair()

      msg =
        "The blood is already on my hands.
        Right or wrong, .. I must follow the path .. to its end."

      signed = Sign.sign_detached(msg, sec)

      assert {:ok, signed} ==
               nock(using_sign_detatched_core(), [
                 9,
                 2,
                 10,
                 [6, 1, msg | sec],
                 0 | 1
               ])
    end

    test "Sign wrong types error" do
      assert :error ==
               nock(using_sign_detatched_core(), [
                 9,
                 2,
                 10,
                 [6, 1, <<3>> | 5],
                 0 | 1
               ])

      assert :error ==
               nock(using_sign_detatched_core(), [
                 9,
                 2,
                 10,
                 [6, 1, 3 | <<5>>],
                 0 | 1
               ])

      assert :error ==
               nock(using_sign_detatched_core(), [
                 9,
                 2,
                 10,
                 [6, 1, <<3>> | <<5>>],
                 0 | 1
               ])
    end

    test "Verification works" do
      %{public: pub, secret: sec} = Sign.new_keypair()
      msg = "babylon 5 is no more"

      assert {:ok, signed} =
               nock(using_sign_detatched_core(), [
                 9,
                 2,
                 10,
                 [6, 1, msg | sec],
                 0 | 1
               ])

      assert {:ok, 0} ==
               nock(using_verify_detatched_core(), [
                 9,
                 2,
                 10,
                 [6, 1, signed, msg | pub],
                 0 | 1
               ])

      assert {:ok, signed} =
               nock(using_sign_core(), [9, 2, 10, [6, 1, msg | sec], 0 | 1])

      assert {:ok, msg} ==
               nock(using_verify_core(), [
                 9,
                 2,
                 10,
                 [6, 1, signed | pub],
                 0 | 1
               ])
    end

    test "Verification fails gracefully" do
      %{public: pub, secret: sec} = Sign.new_keypair()

      msg =
        "They are alone. They are a dying people. We should let them pass."

      assert {:ok, signed} =
               nock(using_sign_detatched_core(), [
                 9,
                 2,
                 10,
                 [6, 1, msg | sec],
                 0 | 1
               ])

      assert {:ok, 1} ==
               nock(using_verify_detatched_core(), [
                 9,
                 2,
                 10,
                 [6, 1, signed, <<3>> | pub],
                 0 | 1
               ])

      assert {:ok, 1} ==
               nock(using_verify_detatched_core(), [
                 9,
                 2,
                 10,
                 [6, 1, <<2>>, <<3>> | <<1>>],
                 0 | 1
               ])

      assert {:ok, signed} =
               nock(using_sign_core(), [9, 2, 10, [6, 1, msg | sec], 0 | 1])

      assert :error =
               nock(using_verify_core(), [
                 9,
                 2,
                 10,
                 [6, 1, signed | sec],
                 0 | 1
               ])
    end
  end

  describe "Bitwise operations" do
    test "end works" do
      assert nock(using_end(), [9, 2, 10, [6, 1, 5 | 80], 0 | 1]) == {:ok, 16}

      assert nock(using_end(1), [9, 2, 10, [6, 1, 3 | 80], 0 | 1]) ==
               {:ok, 16}

      assert nock(using_end(1), [9, 2, 10, [6, 1, 4 | 80], 0 | 1]) ==
               {:ok, 80}
    end

    test "met works" do
      assert nock(using_met(0), [9, 2, 10, [6, 1 | 28], 0 | 1]) == {:ok, 5}
      assert nock(using_met(1), [9, 2, 10, [6, 1 | 28], 0 | 1]) == {:ok, 3}
      assert nock(using_met(2), [9, 2, 10, [6, 1 | 28], 0 | 1]) == {:ok, 2}
    end

    test "lsh works" do
      assert nock(using_lsh(), [9, 2, 10, [6, 1, 2 | 6], 0 | 1]) == {:ok, 24}
      assert nock(using_lsh(1), [9, 2, 10, [6, 1, 2 | 6], 0 | 1]) == {:ok, 96}

      assert nock(using_lsh(2), [9, 2, 10, [6, 1, 2 | 6], 0 | 1]) ==
               {:ok, 1536}
    end

    test "rsh works" do
      assert nock(using_rsh(0), [9, 2, 10, [6, 1, 2 | 40], 0 | 1]) ==
               {:ok, 10}

      assert nock(using_rsh(1), [9, 2, 10, [6, 1, 2 | 40], 0 | 1]) == {:ok, 2}
      assert nock(using_rsh(2), [9, 2, 10, [6, 1, 1 | 40], 0 | 1]) == {:ok, 2}
    end

    test "bex works" do
      assert nock(using_bex(), [9, 2, 10, [6, 1 | 2], 0 | 1]) == {:ok, 4}
      assert nock(using_bex(), [9, 2, 10, [6, 1 | 5], 0 | 1]) == {:ok, 32}

      assert nock(using_bex(), [9, 2, 10, [6, 1 | 28], 0 | 1]) ==
               {:ok, 268_435_456}
    end

    test "mix works" do
      assert nock(using_mix(), [9, 2, 10, [6, 1, 3 | 5], 0 | 1]) == {:ok, 6}
      assert nock(using_mix(), [9, 2, 10, [6, 1, 11 | 11], 0 | 1]) == {:ok, 0}
    end
  end

  describe "Basic functionality" do
    test "base call" do
      assert nock(using_dec_core(), [9, 2, 0 | 1]) == {:ok, 998}
    end

    test "call with changing arguments" do
      assert nock(using_dec_core(), [9, 2, 10, [6, 1 | 5], 0 | 1]) == {:ok, 4}
    end

    test "dec works on binaries" do
      assert nock(using_dec_core(), [9, 2, 10, [6, 1 | <<22>>], 0 | 1]) ==
               {:ok, 21}
    end
  end

  describe "Standard Library" do
    test "calling fib" do
      assert nock(factorial(), [9, 2, 10, [6, 1 | 7], 0 | 1]) == {:ok, 13}
    end
  end

  describe "Scrying" do
    test "successful scry", %{env: env} do
      key = 777
      id = System.unique_integer([:positive])
      storage = Ordering.get_storage(env.ordering)
      {:ok, increment} = nock(increment_counter_val(key), [9, 2, 0 | 1], env)

      Storage.ensure_new(storage)

      # setup id in the system for snapshot 1
      Ordering.new_order(env.ordering, [Order.new(1, id, self())])
      # put the key with some value
      Storage.put(storage, key, 5)
      # Now snapshot it so we can scry
      Storage.put_snapshot(storage, hd(env.snapshot_path))
      assert {:ok, val} = nock(increment, [9, 2, 10, [6, 1 | id], 0 | 1], env)
      assert val == [777 | 6]
    end

    test "scry may return error if not found", %{env: env} do
      key = 666
      id = System.unique_integer([:positive])
      storage = Ordering.get_storage(env.ordering)
      {:ok, increment} = nock(increment_counter_val(key), [9, 2, 0 | 1], env)

      Storage.ensure_new(storage)

      # setup id in the system for snapshot 1
      Ordering.new_order(env.ordering, [Order.new(1, id, self())])
      # Now snapshot it so we can scry
      Storage.put_snapshot(storage, hd(env.snapshot_path))
      assert :error = nock(increment, [9, 2, 10, [6, 1 | id], 0 | 1], env)
    end
  end

  test "jam and cue give correct values" do
    jam_and_cue(0, 2)
    jam_and_cue(1, 12)
    jam_and_cue(2, 72)
    jam_and_cue(19, 2480)
    jam_and_cue(581_949_002, 1_191_831_557_952)
    jam_and_cue([0 | 19], 39689)
    jam_and_cue([1 | 1], 817)
    jam_and_cue([10_000 | 10_000], 4_952_983_169)
    jam_and_cue([999_999_999 | 999_999_999], 1_301_217_674_263_809)
    jam_and_cue([222, 444 | 888], 250_038_217_192_960_129)
    jam_and_cue([[107 | 110] | [107 | 110]], 635_080_761_093)

    jam_and_cue(
      [0, 1, 2, 3, 4, 5, 6, 7, 8, 9 | 10],
      25_681_224_503_728_653_597_984_370_231_065
    )

    jam_and_cue(
      [99, 100, 101, 102, 103, 104 | 0],
      223_372_995_869_285_333_705_242_560_449
    )

    jam_and_cue(
      [[222, 444 | 888] | [222, 444 | 888]],
      170_479_614_045_978_345_989
    )

    jam_and_cue(
      [[0 | 1], [1 | 2], [2 | 3], [3 | 4] | 0],
      11_976_248_475_217_237_797
    )

    jam_and_cue(
      [
        [0 | 1],
        [1 | 2],
        [2 | 3],
        [3 | 4],
        [4 | 5],
        [5 | 6],
        [6 | 7],
        [7 | 8],
        [8 | 9] | 0
      ],
      7_694_087_033_387_855_647_747_387_855_514_468_399_947_749_137_782_565
    )

    jam_and_cue(
      [
        [0 | 1],
        [2 | 3],
        [4 | 5],
        [6 | 7],
        [8 | 9],
        [10 | 11],
        [12 | 13],
        [14 | 15],
        [16 | 17],
        [18 | 19],
        [20 | 21] | 0
      ],
      308_947_677_754_874_070_959_300_747_182_056_036_528_545_493_781_368_831_595_479_491_505_523_344_414_501
    )

    assert using_dec_core() ==
             using_dec_core() |> Nock.Jam.jam() |> Nock.Cue.cue!()
  end

  def jam_and_cue(jam_value, cue_value) do
    assert jam_value == Nock.Cue.cue!(cue_value)
    assert cue_value == Nock.Jam.jam(jam_value)
  end
end
