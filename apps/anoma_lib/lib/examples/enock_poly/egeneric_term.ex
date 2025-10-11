defmodule Examples.ENockPoly.EGenericTerm do
  use Memoize

  import ExUnit.Assertions
  import NockPoly
  import NockPoly.Term.MacroDefs
  alias NockPoly.GenericTerm

  def generic_term_nat_zero() do
    res = GenericTerm.nat(0)
    assert GenericTerm.typecheck(res) == :ok
    assert GenericTerm.to_string(res) == "0"
    res
  end

  def generic_term_nat_positive() do
    res = GenericTerm.nat(42)
    assert GenericTerm.typecheck(res) == :ok
    assert GenericTerm.to_string(res) == "42"
    res
  end

  def generic_term_prod_empty() do
    res = GenericTerm.prod([])
    assert GenericTerm.typecheck(res) == :ok
    assert GenericTerm.to_string(res) == "()"
    res
  end

  def generic_term_prod_single() do
    n = generic_term_nat_positive()
    res = GenericTerm.prod([n])
    assert GenericTerm.typecheck(res) == :ok
    assert GenericTerm.to_string(res) == "(42)"
    res
  end

  def generic_term_prod_multiple() do
    n1 = GenericTerm.nat(1)
    n2 = GenericTerm.nat(2)
    n3 = GenericTerm.nat(3)
    res = GenericTerm.prod([n1, n2, n3])
    assert GenericTerm.typecheck(res) == :ok
    assert GenericTerm.to_string(res) == "(1, 2, 3)"
    res
  end

  def generic_term_coprod_inl() do
    n = GenericTerm.nat(5)
    res = GenericTerm.coprod(0, n)
    assert GenericTerm.typecheck(res) == :ok
    assert GenericTerm.to_string(res) == "inl(5)"
    res
  end

  def generic_term_coprod_inr() do
    n = GenericTerm.nat(7)
    res = GenericTerm.coprod(1, n)
    assert GenericTerm.typecheck(res) == :ok
    assert GenericTerm.to_string(res) == "inr(7)"
    res
  end

  def generic_term_coprod_higher_index() do
    n = GenericTerm.nat(9)
    res = GenericTerm.coprod(5, n)
    assert GenericTerm.typecheck(res) == :ok
    assert GenericTerm.to_string(res) == "in5(9)"
    res
  end

  def generic_term_nested_prod_coprod() do
    inl = generic_term_coprod_inl()
    inr = generic_term_coprod_inr()
    res = GenericTerm.prod([inl, inr])
    assert GenericTerm.typecheck(res) == :ok
    assert GenericTerm.to_string(res) == "(inl(5), inr(7))"
    res
  end

  def generic_term_nested_coprod_prod() do
    prod = generic_term_prod_multiple()
    res = GenericTerm.coprod(0, prod)
    assert GenericTerm.typecheck(res) == :ok
    assert GenericTerm.to_string(res) == "inl((1, 2, 3))"
    res
  end

  def generic_term_from_elixir_nat() do
    res = GenericTerm.from_elixir(42)
    assert GenericTerm.typecheck(res) == :ok
    assert GenericTerm.to_string(res) == "42"
    res
  end

  def generic_term_from_elixir_tuple_empty() do
    res = GenericTerm.from_elixir({})
    assert GenericTerm.typecheck(res) == :ok
    assert GenericTerm.to_string(res) == "()"
    res
  end

  def generic_term_from_elixir_tuple_single() do
    res = GenericTerm.from_elixir({10})
    assert GenericTerm.typecheck(res) == :ok
    assert GenericTerm.to_string(res) == "(10)"
    res
  end

  def generic_term_from_elixir_tuple_multiple() do
    res = GenericTerm.from_elixir({1, 2, 3})
    assert GenericTerm.typecheck(res) == :ok
    assert GenericTerm.to_string(res) == "(1, 2, 3)"
    res
  end

  def generic_term_from_elixir_inl() do
    res = GenericTerm.from_elixir({:inl, 5})
    assert GenericTerm.typecheck(res) == :ok
    assert GenericTerm.to_string(res) == "inl(5)"
    res
  end

  def generic_term_from_elixir_inr() do
    res = GenericTerm.from_elixir({:inr, 7})
    assert GenericTerm.typecheck(res) == :ok
    assert GenericTerm.to_string(res) == "inr(7)"
    res
  end

  def generic_term_from_elixir_in_indexed() do
    res = GenericTerm.from_elixir({:in, 3, 11})
    assert GenericTerm.typecheck(res) == :ok
    assert GenericTerm.to_string(res) == "in3(11)"
    res
  end

  def generic_term_from_elixir_nested() do
    res = GenericTerm.from_elixir({:inl, {1, 2}})
    assert GenericTerm.typecheck(res) == :ok
    assert GenericTerm.to_string(res) == "inl((1, 2))"
    res
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
    invalid = tvc({:nat, 5}, [GenericTerm.nat(1)])
    result = GenericTerm.typecheck(invalid)
    assert {:error, errors} = result

    assert Enum.any?(errors, fn e ->
             match?({:invalid_arity, {:nat, 5}, 0, 1}, e)
           end)

    invalid
  end

  def generic_term_typecheck_coprod_no_children() do
    invalid = tvc0({:coprod, 0})
    result = GenericTerm.typecheck(invalid)
    assert {:error, errors} = result

    assert Enum.any?(errors, fn e ->
             match?({:invalid_arity, {:coprod, 0}, 1, 0}, e)
           end)

    invalid
  end

  def generic_term_typecheck_coprod_multiple_children() do
    n1 = GenericTerm.nat(1)
    n2 = GenericTerm.nat(2)
    invalid = tvc({:coprod, 0}, [n1, n2])
    result = GenericTerm.typecheck(invalid)
    assert {:error, errors} = result

    assert Enum.any?(errors, fn e ->
             match?({:invalid_arity, {:coprod, 0}, 1, 2}, e)
           end)

    invalid
  end

  def generic_term_typecheck_invalid_constructor() do
    invalid = tvc0({:invalid, 42})
    result = GenericTerm.typecheck(invalid)
    assert {:error, errors} = result

    assert Enum.any?(errors, fn e ->
             match?({:invalid_constructor, {:invalid, 42}}, e)
           end)

    invalid
  end

  def generic_term_typecheck_propagate_child_errors() do
    invalid_child = generic_term_typecheck_nat_with_children()
    invalid_parent = GenericTerm.prod([invalid_child])
    result = GenericTerm.typecheck(invalid_parent)
    assert {:error, errors} = result
    assert length(errors) == 1

    assert Enum.any?(errors, fn e ->
             match?({:invalid_arity, {:nat, 5}, 0, 1}, e)
           end)

    invalid_parent
  end

  def generic_term_typecheck_multiple_errors() do
    e1 = generic_term_typecheck_nat_with_children()
    e2 = generic_term_typecheck_coprod_no_children()
    invalid = GenericTerm.prod([e1, e2])
    result = GenericTerm.typecheck(invalid)
    assert {:error, errors} = result
    assert length(errors) == 2
    invalid
  end

  def generic_term_generic_tspec_coprod() do
    assert GenericTerm.generic_tspec({:coprod, 0}) == {:ok, 1}
    assert GenericTerm.generic_tspec({:coprod, 999}) == {:ok, 1}
    :ok
  end

  def generic_term_generic_tspec_prod() do
    assert GenericTerm.generic_tspec(:prod) == {:ok, :variable_arity}
    :ok
  end

  def generic_term_generic_tspec_nat() do
    assert GenericTerm.generic_tspec({:nat, 0}) == {:ok, 0}
    assert GenericTerm.generic_tspec({:nat, 999}) == {:ok, 0}
    :ok
  end

  def generic_term_generic_tspec_invalid() do
    assert GenericTerm.generic_tspec(:invalid) == {:invalid_constructor}
    assert GenericTerm.generic_tspec({:coprod, -1}) == {:invalid_constructor}
    assert GenericTerm.generic_tspec({:nat, -1}) == {:invalid_constructor}
    assert GenericTerm.generic_tspec("string") == {:invalid_constructor}
    :ok
  end
end
