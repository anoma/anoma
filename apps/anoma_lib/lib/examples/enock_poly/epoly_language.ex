defmodule Examples.ENockPoly.EPolyLanguage do
  @moduledoc """
  I provide examples for the NockPoly.PolyLanguage module.

  A `fin_mapping` represents a function between finite sets, encoded as a list
  where each position in the list (domain element) contains the index of the
  corresponding codomain element.
  """

  use Memoize

  import ExUnit.Assertions

  alias NockPoly.PolyLanguage

  def identity_fin_mapping() do
    mapping = [0, 1, 2]
    result = PolyLanguage.validate_fin_mapping(mapping, 3, 3)
    assert result == :ok
    mapping
  end

  def empty_domain_fin_mapping() do
    mapping = []
    result = PolyLanguage.validate_fin_mapping(mapping, 0, 5)
    assert result == :ok
    mapping
  end

  def constant_fin_mapping() do
    mapping = [0, 0, 0]
    result = PolyLanguage.validate_fin_mapping(mapping, 3, 1)
    assert result == :ok
    mapping
  end

  def permutation_fin_mapping() do
    mapping = [2, 0, 1]
    result = PolyLanguage.validate_fin_mapping(mapping, 3, 3)
    assert result == :ok
    mapping
  end

  def invalid_fin_mapping_too_short() do
    mapping = [0, 1]
    result = PolyLanguage.validate_fin_mapping(mapping, 3, 3)
    assert result == {:error, [:invalid_mapping_length]}
    result
  end

  def invalid_fin_mapping_too_long() do
    mapping = [0, 1, 2, 3]
    result = PolyLanguage.validate_fin_mapping(mapping, 3, 3)
    assert result == {:error, [:invalid_mapping_length]}
    result
  end

  def invalid_fin_mapping_index_out_of_range() do
    mapping = [0, 3, 1]
    result = PolyLanguage.validate_fin_mapping(mapping, 3, 3)
    assert result == {:error, [:mapping_out_of_range]}
    result
  end

  def invalid_fin_mapping_exceeds_codomain() do
    mapping = [0, 1, 5]
    result = PolyLanguage.validate_fin_mapping(mapping, 3, 3)
    assert result == {:error, [:mapping_out_of_range]}
    result
  end
end
