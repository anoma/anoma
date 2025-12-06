defmodule Examples.ENockPoly.EPolyLanguage do
  @moduledoc """
  I provide examples for the NockPoly.PolyLanguage module.
  """

  use ExUnit.Case, async: true

  alias NockPoly.PolyLanguage

  @doc """
  I test the `fin_mapping` type and `validate_fin_mapping` function.

  A `fin_mapping` represents a function between finite sets, encoded as a list
  where each position in the list (domain element) contains the index of the
  corresponding codomain element.
  """
  def fin_mapping_test do
    # Valid mapping: identity on 3 elements
    assert :ok = PolyLanguage.validate_fin_mapping([0, 1, 2], 3, 3)

    # Valid mapping: empty domain maps to any codomain
    assert :ok = PolyLanguage.validate_fin_mapping([], 0, 5)

    # Valid mapping: all domain elements map to same codomain element
    assert :ok = PolyLanguage.validate_fin_mapping([0, 0, 0], 3, 1)

    # Valid mapping: permutation
    assert :ok = PolyLanguage.validate_fin_mapping([2, 0, 1], 3, 3)

    # Invalid mapping: wrong length (too short)
    assert {:error, [:invalid_mapping_length]} =
             PolyLanguage.validate_fin_mapping([0, 1], 3, 3)

    # Invalid mapping: wrong length (too long)
    assert {:error, [:invalid_mapping_length]} =
             PolyLanguage.validate_fin_mapping([0, 1, 2, 3], 3, 3)

    # Invalid mapping: index out of range
    assert {:error, [:mapping_out_of_range]} =
             PolyLanguage.validate_fin_mapping([0, 3, 1], 3, 3)

    # Invalid mapping: negative conceptually impossible (type is non_neg_integer)
    # but index exceeding codomain size is caught
    assert {:error, [:mapping_out_of_range]} =
             PolyLanguage.validate_fin_mapping([0, 1, 5], 3, 3)

    :ok
  end
end
