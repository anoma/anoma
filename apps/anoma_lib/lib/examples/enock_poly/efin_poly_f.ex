defmodule Examples.ENockPoly.EFinPolyF do
  use Memoize

  import ExUnit.Assertions
  import NockPoly.Term.MacroDefs
  alias NockPoly.FinPolyF

  use TypedStruct

  # Common tspec (constructor-check function) for poly_term tests.
  defp common_tspec() do
    fn
      "zero" -> {:ok, 0}
      "one" -> {:ok, 1}
      "two" -> {:ok, 2}
      _ -> :invalid_constructor
    end
  end

  # Common vspec (variable-check function) for poly_term tests.
  defp common_vspec() do
    fn
      v when is_integer(v) and v >= 0 and v <= 4 -> :ok
      _ -> :invalid_variable
    end
  end

  @doc """
  poly_term_test_valid: A valid term using a string-based tspec.
  """
  def poly_term_test_valid() do
    res = tvc("one", [tvc0("zero")])
    assert FinPolyF.typecheck_v(res, {common_tspec(), common_vspec()}) == :ok
    res
  end

  @doc """
  poly_term_test_arity: Term with an arity mismatch.
  """
  def poly_term_test_arity() do
    res = tvc0("one")

    assert FinPolyF.typecheck_v(res, {common_tspec(), common_vspec()}) ==
             {:error, [{:invalid_arity, "one", 1, 0}]}

    res
  end

  @doc """
  poly_term_test_ctor: Term with an invalid constructor.
  """
  def poly_term_test_ctor() do
    res = tvc0("three")

    assert FinPolyF.typecheck_v(res, {common_tspec(), common_vspec()}) ==
             {:error, [{:invalid_constructor, "three"}]}

    res
  end

  @doc """
  poly_term_test_valid_variable: A valid variable term.
  """
  def poly_term_test_valid_variable() do
    res = tvv(2)
    assert FinPolyF.typecheck_v(res, {common_tspec(), common_vspec()}) == :ok
    res
  end

  @doc """
  poly_term_test_invalid_variable: A term with an invalid variable.
  """
  def poly_term_test_invalid_variable() do
    res = tvv(10)

    assert FinPolyF.typecheck_v(res, {common_tspec(), common_vspec()}) ==
             {:error, [{:invalid_variable, 10}]}

    res
  end

  @doc """
  poly_term_test_vspec_ok: Using vspec_ok to always succeed.
  """
  def poly_term_test_vspec_ok() do
    res = tvv(10)

    assert FinPolyF.typecheck_v(res, {common_tspec(), &FinPolyF.vspec_ok/1}) ==
             :ok

    res
  end

  @doc """
  poly_term_test_multi: A term accumulating multiple errors.
  """
  def poly_term_test_multi() do
    res = tvc("two", [tvv(10), tvc0("three")])

    expected_errors = [
      {:invalid_variable, 10},
      {:invalid_constructor, "three"}
    ]

    assert FinPolyF.typecheck_v(res, {common_tspec(), common_vspec()}) ==
             {:error, expected_errors}

    res
  end
end
