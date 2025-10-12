defmodule Examples.ENockPoly.EGenericTerm do
  use Memoize

  import ExUnit.Assertions
  import NockPoly
  import NockPoly.Term.MacroDefs
  alias NockPoly.GenericTerm

  def generic_term_nat_zero() do
    GenericTerm.nat(0)
  end

  def generic_term_nat_zero_typecheck() do
    result = GenericTerm.typecheck(generic_term_nat_zero())
    assert result == :ok
    result
  end

  def generic_term_nat_zero_string() do
    result = GenericTerm.to_string(generic_term_nat_zero())
    assert result == "0"
    result
  end

  def generic_term_nat_positive() do
    GenericTerm.nat(42)
  end

  def generic_term_nat_positive_typecheck() do
    result = GenericTerm.typecheck(generic_term_nat_positive())
    assert result == :ok
    result
  end

  def generic_term_nat_positive_string() do
    result = GenericTerm.to_string(generic_term_nat_positive())
    assert result == "42"
    result
  end

  def generic_term_prod_empty() do
    GenericTerm.prod([])
  end

  def generic_term_prod_empty_typecheck() do
    result = GenericTerm.typecheck(generic_term_prod_empty())
    assert result == :ok
    result
  end

  def generic_term_prod_empty_string() do
    result = GenericTerm.to_string(generic_term_prod_empty())
    assert result == "()"
    result
  end

  def generic_term_prod_single() do
    GenericTerm.prod([generic_term_nat_positive()])
  end

  def generic_term_prod_single_typecheck() do
    result = GenericTerm.typecheck(generic_term_prod_single())
    assert result == :ok
    result
  end

  def generic_term_prod_single_string() do
    result = GenericTerm.to_string(generic_term_prod_single())
    assert result == "(42)"
    result
  end

  def generic_term_prod_multiple() do
    n1 = GenericTerm.nat(1)
    n2 = GenericTerm.nat(2)
    n3 = GenericTerm.nat(3)
    GenericTerm.prod([n1, n2, n3])
  end

  def generic_term_prod_multiple_typecheck() do
    result = GenericTerm.typecheck(generic_term_prod_multiple())
    assert result == :ok
    result
  end

  def generic_term_prod_multiple_string() do
    result = GenericTerm.to_string(generic_term_prod_multiple())
    assert result == "(1, 2, 3)"
    result
  end

  def generic_term_coprod_inl() do
    GenericTerm.coprod(0, GenericTerm.nat(5))
  end

  def generic_term_coprod_inl_typecheck() do
    result = GenericTerm.typecheck(generic_term_coprod_inl())
    assert result == :ok
    result
  end

  def generic_term_coprod_inl_string() do
    result = GenericTerm.to_string(generic_term_coprod_inl())
    assert result == "inl(5)"
    result
  end

  def generic_term_coprod_inr() do
    GenericTerm.coprod(1, GenericTerm.nat(7))
  end

  def generic_term_coprod_inr_typecheck() do
    result = GenericTerm.typecheck(generic_term_coprod_inr())
    assert result == :ok
    result
  end

  def generic_term_coprod_inr_string() do
    result = GenericTerm.to_string(generic_term_coprod_inr())
    assert result == "inr(7)"
    result
  end

  def generic_term_coprod_higher_index() do
    GenericTerm.coprod(5, GenericTerm.nat(9))
  end

  def generic_term_coprod_higher_index_typecheck() do
    result = GenericTerm.typecheck(generic_term_coprod_higher_index())
    assert result == :ok
    result
  end

  def generic_term_coprod_higher_index_string() do
    result = GenericTerm.to_string(generic_term_coprod_higher_index())
    assert result == "in5(9)"
    result
  end

  def generic_term_nested_prod_coprod() do
    GenericTerm.prod([generic_term_coprod_inl(), generic_term_coprod_inr()])
  end

  def generic_term_nested_prod_coprod_typecheck() do
    result = GenericTerm.typecheck(generic_term_nested_prod_coprod())
    assert result == :ok
    result
  end

  def generic_term_nested_prod_coprod_string() do
    result = GenericTerm.to_string(generic_term_nested_prod_coprod())
    assert result == "(inl(5), inr(7))"
    result
  end

  def generic_term_nested_coprod_prod() do
    GenericTerm.coprod(0, generic_term_prod_multiple())
  end

  def generic_term_nested_coprod_prod_typecheck() do
    result = GenericTerm.typecheck(generic_term_nested_coprod_prod())
    assert result == :ok
    result
  end

  def generic_term_nested_coprod_prod_string() do
    result = GenericTerm.to_string(generic_term_nested_coprod_prod())
    assert result == "inl((1, 2, 3))"
    result
  end

  def generic_term_from_elixir_nat() do
    GenericTerm.from_elixir(42)
  end

  def generic_term_from_elixir_nat_typecheck() do
    result = GenericTerm.typecheck(generic_term_from_elixir_nat())
    assert result == :ok
    result
  end

  def generic_term_from_elixir_nat_string() do
    result = GenericTerm.to_string(generic_term_from_elixir_nat())
    assert result == "42"
    result
  end

  def generic_term_from_elixir_tuple_empty() do
    GenericTerm.from_elixir({})
  end

  def generic_term_from_elixir_tuple_empty_typecheck() do
    result = GenericTerm.typecheck(generic_term_from_elixir_tuple_empty())
    assert result == :ok
    result
  end

  def generic_term_from_elixir_tuple_empty_string() do
    result = GenericTerm.to_string(generic_term_from_elixir_tuple_empty())
    assert result == "()"
    result
  end

  def generic_term_from_elixir_tuple_single() do
    GenericTerm.from_elixir({10})
  end

  def generic_term_from_elixir_tuple_single_typecheck() do
    result = GenericTerm.typecheck(generic_term_from_elixir_tuple_single())
    assert result == :ok
    result
  end

  def generic_term_from_elixir_tuple_single_string() do
    result = GenericTerm.to_string(generic_term_from_elixir_tuple_single())
    assert result == "(10)"
    result
  end

  def generic_term_from_elixir_tuple_multiple() do
    GenericTerm.from_elixir({1, 2, 3})
  end

  def generic_term_from_elixir_tuple_multiple_typecheck() do
    result = GenericTerm.typecheck(generic_term_from_elixir_tuple_multiple())
    assert result == :ok
    result
  end

  def generic_term_from_elixir_tuple_multiple_string() do
    result = GenericTerm.to_string(generic_term_from_elixir_tuple_multiple())
    assert result == "(1, 2, 3)"
    result
  end

  def generic_term_from_elixir_inl() do
    GenericTerm.from_elixir({:inl, 5})
  end

  def generic_term_from_elixir_inl_typecheck() do
    result = GenericTerm.typecheck(generic_term_from_elixir_inl())
    assert result == :ok
    result
  end

  def generic_term_from_elixir_inl_string() do
    result = GenericTerm.to_string(generic_term_from_elixir_inl())
    assert result == "inl(5)"
    result
  end

  def generic_term_from_elixir_inr() do
    GenericTerm.from_elixir({:inr, 7})
  end

  def generic_term_from_elixir_inr_typecheck() do
    result = GenericTerm.typecheck(generic_term_from_elixir_inr())
    assert result == :ok
    result
  end

  def generic_term_from_elixir_inr_string() do
    result = GenericTerm.to_string(generic_term_from_elixir_inr())
    assert result == "inr(7)"
    result
  end

  def generic_term_from_elixir_in_indexed() do
    GenericTerm.from_elixir({:in, 3, 11})
  end

  def generic_term_from_elixir_in_indexed_typecheck() do
    result = GenericTerm.typecheck(generic_term_from_elixir_in_indexed())
    assert result == :ok
    result
  end

  def generic_term_from_elixir_in_indexed_string() do
    result = GenericTerm.to_string(generic_term_from_elixir_in_indexed())
    assert result == "in3(11)"
    result
  end

  def generic_term_from_elixir_nested() do
    GenericTerm.from_elixir({:inl, {1, 2}})
  end

  def generic_term_from_elixir_nested_typecheck() do
    result = GenericTerm.typecheck(generic_term_from_elixir_nested())
    assert result == :ok
    result
  end

  def generic_term_from_elixir_nested_string() do
    result = GenericTerm.to_string(generic_term_from_elixir_nested())
    assert result == "inl((1, 2))"
    result
  end

  def generic_term_from_elixir_invalid() do
    # Test with dynamic values that dialyzer can't analyze at compile time
    invalid_values = ["invalid", -1, [1, 2, 3]]

    Enum.each(invalid_values, fn value ->
      assert_raise ArgumentError, ~r/Cannot convert/, fn ->
        GenericTerm.from_elixir(value)
      end
    end)

    :ok
  end

  def generic_term_typecheck_nat_with_children() do
    tvc({:nat, 5}, [GenericTerm.nat(1)])
  end

  def generic_term_typecheck_nat_with_children_result() do
    result = GenericTerm.typecheck(generic_term_typecheck_nat_with_children())
    assert {:error, errors} = result

    assert Enum.any?(errors, fn e ->
             match?({:invalid_arity, {:nat, 5}, 0, 1}, e)
           end)

    result
  end

  def generic_term_typecheck_coprod_no_children() do
    tvc0({:coprod, 0})
  end

  def generic_term_typecheck_coprod_no_children_result() do
    result =
      GenericTerm.typecheck(generic_term_typecheck_coprod_no_children())

    assert {:error, errors} = result

    assert Enum.any?(errors, fn e ->
             match?({:invalid_arity, {:coprod, 0}, 1, 0}, e)
           end)

    result
  end

  def generic_term_typecheck_coprod_multiple_children() do
    tvc({:coprod, 0}, [GenericTerm.nat(1), GenericTerm.nat(2)])
  end

  def generic_term_typecheck_coprod_multiple_children_result() do
    result =
      GenericTerm.typecheck(generic_term_typecheck_coprod_multiple_children())

    assert {:error, errors} = result

    assert Enum.any?(errors, fn e ->
             match?({:invalid_arity, {:coprod, 0}, 1, 2}, e)
           end)

    result
  end

  def generic_term_typecheck_invalid_constructor() do
    tvc0({:invalid, 42})
  end

  def generic_term_typecheck_invalid_constructor_result() do
    result =
      GenericTerm.typecheck(generic_term_typecheck_invalid_constructor())

    assert {:error, errors} = result

    assert Enum.any?(errors, fn e ->
             match?({:invalid_constructor, {:invalid, 42}}, e)
           end)

    result
  end

  def generic_term_typecheck_propagate_child_errors() do
    GenericTerm.prod([generic_term_typecheck_nat_with_children()])
  end

  def generic_term_typecheck_propagate_child_errors_result() do
    result =
      GenericTerm.typecheck(generic_term_typecheck_propagate_child_errors())

    assert {:error, errors} = result
    assert length(errors) == 1

    assert Enum.any?(errors, fn e ->
             match?({:invalid_arity, {:nat, 5}, 0, 1}, e)
           end)

    result
  end

  def generic_term_typecheck_multiple_errors() do
    GenericTerm.prod([
      generic_term_typecheck_nat_with_children(),
      generic_term_typecheck_coprod_no_children()
    ])
  end

  def generic_term_typecheck_multiple_errors_result() do
    result = GenericTerm.typecheck(generic_term_typecheck_multiple_errors())
    assert {:error, errors} = result
    assert length(errors) == 2
    result
  end

  def generic_term_generic_tspec_coprod_0() do
    result = GenericTerm.generic_tspec({:coprod, 0})
    assert result == {:ok, 1}
    result
  end

  def generic_term_generic_tspec_coprod_999() do
    result = GenericTerm.generic_tspec({:coprod, 999})
    assert result == {:ok, 1}
    result
  end

  def generic_term_generic_tspec_prod() do
    result = GenericTerm.generic_tspec(:prod)
    assert result == {:ok, :variable_arity}
    result
  end

  def generic_term_generic_tspec_nat_0() do
    result = GenericTerm.generic_tspec({:nat, 0})
    assert result == {:ok, 0}
    result
  end

  def generic_term_generic_tspec_nat_999() do
    result = GenericTerm.generic_tspec({:nat, 999})
    assert result == {:ok, 0}
    result
  end

  def generic_term_generic_tspec_invalid() do
    result = GenericTerm.generic_tspec(:invalid)
    assert result == :invalid_constructor
    result
  end

  def generic_term_generic_tspec_coprod_negative() do
    result = GenericTerm.generic_tspec({:coprod, -1})
    assert result == :invalid_constructor
    result
  end

  def generic_term_generic_tspec_nat_negative() do
    result = GenericTerm.generic_tspec({:nat, -1})
    assert result == :invalid_constructor
    result
  end

  def generic_term_generic_tspec_string() do
    result = GenericTerm.generic_tspec("string")
    assert result == :invalid_constructor
    result
  end
end
