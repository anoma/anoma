defmodule PoseidonTest do
  use ExUnit.Case
  doctest Anoma.StarknetCrypto

  test "poseidon_single_hash_test" do
    x_string_unpadded = "9dad5d6f502ccbcb6d34ede04f0337df3b98936aaf782f4cc07d147e3a4fd6"
    x_string = String.pad_leading(x_string_unpadded, 64, "0")
    x_bytes = :binary.bin_to_list(Base.decode16!(x_string, case: :lower))
    output = Anoma.StarknetCrypto.poseidon_single(x_bytes)
    hash_string = "011222854783f17f1c580ff64671bc3868de034c236f956216e8ed4ab7533455"
    hash_bytes = :binary.bin_to_list(Base.decode16!(hash_string, case: :lower))
    assert hash_bytes == output
  end

  test "poseidon_hash_test" do
    x_string_unpadded = "b662f9017fa7956fd70e26129b1833e10ad000fd37b4d9f4e0ce6884b7bbe"
    y_string_unpadded = "1fe356bf76102cdae1bfbdc173602ead228b12904c00dad9cf16e035468bea"
    hash_string_unpadded = "75540825a6ecc5dc7d7c2f5f868164182742227f1367d66c43ee51ec7937a81"
    x_string = String.pad_leading(x_string_unpadded, 64, "0")
    y_string = String.pad_leading(y_string_unpadded, 64, "0")
    hash_string = String.pad_leading(hash_string_unpadded, 64, "0")
    x_bytes = :binary.bin_to_list(Base.decode16!(x_string, case: :lower))
    y_bytes = :binary.bin_to_list(Base.decode16!(y_string, case: :lower))
    output = Anoma.StarknetCrypto.poseidon(x_bytes, y_bytes)
    hash_bytes = :binary.bin_to_list(Base.decode16!(hash_string, case: :lower))
    assert hash_bytes == output
  end

  test "poseidon_hash_many" do
    i1_string_unpadded = "9bf52404586087391c5fbb42538692e7ca2149bac13c145ae4230a51a6fc47"
    i2_string_unpadded = "40304159ee9d2d611120fbd7c7fb8020cc8f7a599bfa108e0e085222b862c0"
    i3_string_unpadded = "46286e4f3c450761d960d6a151a9c0988f9e16f8a48d4c0a85817c009f806a"
    hash_string_unpadded = "1ec38b38dc88bac7b0ed6ff6326f975a06a59ac601b417745fd412a5d38e4f7"
    i1_string = String.pad_leading(i1_string_unpadded, 64, "0")
    i2_string = String.pad_leading(i2_string_unpadded, 64, "0")
    i3_string = String.pad_leading(i3_string_unpadded, 64, "0")
    hash_string = String.pad_leading(hash_string_unpadded, 64, "0")
    i1_bytes = :binary.bin_to_list(Base.decode16!(i1_string, case: :lower))
    i2_bytes = :binary.bin_to_list(Base.decode16!(i2_string, case: :lower))
    i3_bytes = :binary.bin_to_list(Base.decode16!(i3_string, case: :lower))
    hash_bytes = :binary.bin_to_list(Base.decode16!(hash_string, case: :lower))
    output = Anoma.StarknetCrypto.poseidon_many([i1_bytes, i2_bytes, i3_bytes])
    assert hash_bytes == output
  end
end
