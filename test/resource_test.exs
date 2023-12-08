defmodule AnomaTest.Resource do
  use ExUnit.Case, async: true
  doctest Anoma.Resource

  import Anoma.Resource
  alias Anoma.Sign

  test "commitments and nullifiers" do
    keypair_a = Sign.new_keypair()
    keypair_b = Sign.new_keypair()

    a_r1 = new_with_npk(keypair_a.public)
    a_r2 = new_with_npk(keypair_a.public)
    b_r0 = new_with_npk(keypair_b.public)

    # just in case
    assert a_r1 != a_r2

    c_a_r1 = commitment(a_r1)
    c_a_r2 = commitment(a_r2)
    c_b_r0 = commitment(b_r0)

    n_a_r1 = nullifier(a_r1, keypair_a.secret)
    n_a_r2 = nullifier(a_r2, keypair_a.secret)
    n_b_r0 = nullifier(b_r0, keypair_b.secret)

    assert c_a_r1 |> commits_to(a_r1)
    refute c_a_r1 |> commits_to(a_r2)
    refute c_a_r1 |> commits_to(b_r0)

    refute c_a_r2 |> commits_to(a_r1)
    assert c_a_r2 |> commits_to(a_r2)
    refute c_a_r2 |> commits_to(b_r0)

    refute c_b_r0 |> commits_to(a_r1)
    refute c_b_r0 |> commits_to(a_r2)
    assert c_b_r0 |> commits_to(b_r0)

    assert n_a_r1 |> nullifies(a_r1)
    refute n_a_r1 |> nullifies(a_r2)
    refute n_a_r1 |> nullifies(b_r0)

    refute n_a_r2 |> nullifies(a_r1)
    assert n_a_r2 |> nullifies(a_r2)
    refute n_a_r2 |> nullifies(b_r0)

    refute n_b_r0 |> nullifies(a_r1)
    refute n_b_r0 |> nullifies(a_r2)
    assert n_b_r0 |> nullifies(b_r0)
  end

  test "nullify with wrong key" do
    keypair_a = Sign.new_keypair()
    keypair_b = Sign.new_keypair()

    a_resource = new_with_npk(keypair_a.public)
    wrong_nullifier = nullifier(a_resource, keypair_b.secret)

    refute wrong_nullifier |> nullifies(a_resource)
  end
end
