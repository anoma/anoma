defmodule Anoma.Constants do
  def felt_zero, do: <<0::256>>

  def felt_one, do: <<1::256>>

  def prf_expand_personalization_felt,
    do:
      <<0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 67, 97, 105, 114, 111,
        95, 69, 120, 112, 97, 110, 100, 83, 101, 101, 100>>
end
