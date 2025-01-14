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

  @doc """
  The hash of the Cairo compliance program.
  The hash hex-string: 0x6944b2f82802027e79a3d4f6b83daaa45a9b860a800543a484a41cc38ef02f6
  """
  @spec cairo_compliance_program_hash() :: binary()
  def cairo_compliance_program_hash,
    do:
      <<6, 148, 75, 47, 130, 128, 32, 39, 231, 154, 61, 79, 107, 131, 218,
        170, 69, 169, 184, 96, 168, 0, 84, 58, 72, 74, 65, 204, 56, 239, 2,
        246>>

  @doc """
  The hash of the Cairo trivial logic program.
  The hash hex-string: 0x190b872fb979300555afbdab15fee956aeb133771dc1e290f75a8d7a2eedbe5
  """
  @spec cairo_trivial_resource_logic_hash() :: binary()
  def cairo_trivial_resource_logic_hash,
    do:
      <<1, 144, 184, 114, 251, 151, 147, 0, 85, 90, 251, 218, 177, 95, 238,
        149, 106, 235, 19, 55, 113, 220, 30, 41, 15, 117, 168, 215, 162, 238,
        219, 229>>

  @doc """
  The default merkle root in cairo RM is used when the root set is empty and can
  also serve as the root of ephemeral resources.
  """
  @spec default_cairo_rm_root() :: binary()
  def default_cairo_rm_root,
    do:
      <<4, 159, 35, 51, 235, 49, 48, 244, 131, 34, 79, 109, 240, 252, 193, 45,
        153, 168, 38, 144, 4, 120, 15, 77, 11, 26, 82, 221, 66, 229, 253,
        122>>
end
