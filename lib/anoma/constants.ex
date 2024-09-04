defmodule Anoma.Constants do
  def felt_zero, do: :binary.copy(<<0>>, 32)

  def felt_one, do: :binary.copy(<<0>>, 31) <> <<1>>

  def prf_expand_personalization_felt,
    do:
      <<0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 67, 97, 105, 114, 111,
        95, 69, 120, 112, 97, 110, 100, 83, 101, 101, 100>>

  def cairo_compliance_program_hash,
    do:
      <<7, 134, 65, 173, 238, 133, 49, 157, 88, 236, 149, 228, 209, 212, 18,
        125, 150, 169, 202, 54, 94, 119, 181, 224, 111, 40, 110, 113, 249,
        214, 202, 112>>
end
