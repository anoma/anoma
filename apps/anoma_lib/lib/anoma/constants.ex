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
  The hash hex-string: 0x37cca7d1f5a6a17c3ad7df70da9bca10a66078ffc488fbb704b61293a3d52ec
  """
  @spec cairo_trivial_resource_logic_hash() :: binary()
  def cairo_trivial_resource_logic_hash,
    do:
      <<3, 124, 202, 125, 31, 90, 106, 23, 195, 173, 125, 247, 13, 169, 188,
        161, 10, 102, 7, 143, 252, 72, 143, 187, 112, 75, 97, 41, 58, 61, 82,
        236>>

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

  @spec default_cairo_nullifier_key() :: binary()
  def default_cairo_nullifier_key,
    do: felt_one()
end
