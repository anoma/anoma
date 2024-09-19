defmodule Anoma.Constants do
  def felt_zero, do: :binary.copy(<<0>>, 32)

  def felt_one, do: :binary.copy(<<0>>, 31) <> <<1>>

  def prf_expand_personalization_felt,
    do:
      <<0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 67, 97, 105, 114, 111,
        95, 69, 120, 112, 97, 110, 100, 83, 101, 101, 100>>

  def cairo_compliance_program_hash,
    do:
      <<1, 187, 37, 213, 62, 82, 16, 216, 85, 16, 20, 9, 248, 138, 85, 191,
        92, 149, 228, 228, 215, 118, 57, 175, 251, 63, 166, 64, 144, 107, 240,
        61>>
end
