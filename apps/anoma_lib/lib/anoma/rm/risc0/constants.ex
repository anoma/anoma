defmodule Anoma.RM.Risc0.Constants do
  def zero, do: <<0::256>>

  def one, do: <<1::256>>

  def random_hash,
    do:
      <<1, 187, 37, 213, 62, 82, 16, 216, 85, 16, 20, 9, 248, 138, 85, 191,
        92, 149, 228, 228, 215, 118, 57, 175, 251, 63, 166, 64, 144, 107, 240,
        61>>
end
