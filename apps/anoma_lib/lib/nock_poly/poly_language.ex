defmodule NockPoly.PolyLanguage do
  @moduledoc """
  I provide components for building custom languages on top of polynomial terms.

  A polynomial language is defined by specifying the structure of its terms
  using polynomial functors. This module provides types and functions for
  describing such structures, including mappings between finite sets that
  describe how constructor parameters relate to types.
  """

  @typedoc """
  I represent a mapping from elements in a domain to elements in a codomain.
  I am a list of non-negative integers where:
  - Length = size of domain
  - Each element = index in codomain (must be in range [0..(codomain_size-1)])

  Example: [2, 0, 1] means:
  - Domain size is 3
  - Element 0 in domain maps to element 2 in codomain
  - Element 1 in domain maps to element 0 in codomain
  - Element 2 in domain maps to element 1 in codomain
  """
  @type fin_mapping :: [non_neg_integer()]

  @doc """
  I validate that a finite mapping is valid for given domain and codomain sizes.
  """
  @spec validate_fin_mapping(
          fin_mapping(),
          non_neg_integer(),
          non_neg_integer()
        ) ::
          :ok | {:error, nonempty_list(atom())}
  def validate_fin_mapping(mapping, domain_size, codomain_size) do
    cond do
      length(mapping) != domain_size ->
        {:error, [:invalid_mapping_length]}

      Enum.any?(mapping, &(&1 >= codomain_size)) ->
        {:error, [:mapping_out_of_range]}

      true ->
        :ok
    end
  end
end
