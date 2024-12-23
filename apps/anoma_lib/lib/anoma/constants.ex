defmodule Anoma.Constants do
  @spec felt_zero() :: binary()
  def felt_zero, do: <<0::256>>

  @spec felt_one() :: binary()
  def felt_one, do: <<1::256>>

  @spec prf_expand_personalization_felt() :: binary()
  def prf_expand_personalization_felt,
    do:
      <<0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 67, 97, 105, 114, 111,
        95, 69, 120, 112, 97, 110, 100, 83, 101, 101, 100>>

  @spec cairo_compliance_program_hash() :: binary()
  def cairo_compliance_program_hash,
    do:
      <<3, 32, 135, 125, 216, 121, 238, 174, 177, 233, 233, 232, 39, 9, 94,
        235, 157, 203, 181, 193, 194, 189, 91, 138, 36, 25, 128, 230, 197,
        118, 36, 27>>

  # cairo_trivial_resource_logic_hash is just for test
  @spec cairo_trivial_resource_logic_hash() :: binary()
  def cairo_trivial_resource_logic_hash,
    do:
      <<0, 243, 140, 176, 138, 18, 187, 241, 244, 57, 14, 171, 83, 197, 244,
        132, 155, 123, 124, 59, 152, 25, 9, 88, 193, 242, 178, 20, 246, 56,
        191, 55>>
end
