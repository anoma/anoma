defmodule Anoma.Constants do
  def felt_zero, do: :binary.copy(<<0>>, 32)

  def felt_one, do: :binary.copy(<<0>>, 31) <> <<1>>

  def prf_expand_personalization_felt,
    do:
      <<0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 67, 97, 105, 114, 111,
        95, 69, 120, 112, 97, 110, 100, 83, 101, 101, 100>>
end
