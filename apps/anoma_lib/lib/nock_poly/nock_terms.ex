defmodule NockPoly.NockTerms do
  @moduledoc """
  I am a polynomial specification for Nock terms.

  A Nock term is a binary tree where:
    - An atom node is represented as `{:atom, noun}` (with `noun` having
      type Noun.noun_atom()).  Atoms have arity 0.
    - A cell node is represented as `:cell`.
      Cells have arity 2.

  All constructors are always valid (any term of type `Noun.noun_atom()` is
  a valid Nock atom).
  """

  require Noun

  use TypedStruct

  alias NockPoly.Term
  alias NockPoly.FinPolyF

  @type nock_term_ctor :: {:atom, Noun.noun_atom()} | :cell
  @type nock_poly_term :: Term.t(nock_term_ctor)

  @spec nock_tspec(nock_term_ctor) :: {:ok, non_neg_integer()}
  def nock_tspec({:atom, _noun}) do
    {:ok, 0}
  end

  def nock_tspec(:cell) do
    {:ok, 2}
  end

  @spec typecheck(nock_poly_term) ::
          FinPolyF.check_result(nock_term_ctor, none())
  def typecheck(term) do
    FinPolyF.typecheck(term, &nock_tspec/1)
  end

  @doc """
  I convert a Noun.t() into a Nock polynomial term (nock_poly_term).

  If `noun` satisfies Noun.is_noun_atom/1, it is wrapped as an atom;
  otherwise it must be a cell represented as a two-element list, which is
  recursively converted. This function always succeeds and produces a term
  that passes the Nock typecheck.
  """
  @spec from_noun(Noun.t()) :: nock_poly_term
  def from_noun(noun) do
    cond do
      Noun.is_noun_atom(noun) ->
        Term.com_tv({:atom, noun}, [])

      Noun.is_noun_cell(noun) ->
        case noun do
          [left | right] ->
            Term.com_tv(:cell, [from_noun(left), from_noun(right)])
        end
    end
  end

  @spec to_noun_algebra({nock_term_ctor, [Noun.t()]}) :: Noun.t()
  defp to_noun_algebra({ctor, results}) do
    case {ctor, results} do
      {{:atom, noun}, []} ->
        noun

      {:cell, [left, right]} ->
        [left | right]
    end
  end

  @doc """
  I convert a Nock polynomial term (nock_poly_term) back into a Noun.t().

  It is assumed that the term has successfully passed the Nock typecheck.
  I use `cata/2` (the catamorphism) so that I am not directly recursive.
  """
  @spec to_noun(nock_poly_term) :: Noun.t()
  def to_noun(term) do
    Term.cata(term, &to_noun_algebra/1)
  end

  @typedoc "I am an open Nock polynomial term containing metavariables of type `v`."
  @type open_nock_poly_term(v) :: Term.tv(nock_term_ctor, v)

  @doc """
  I substitute metavariables in an open Nock term using the provided
  substitution function.

  Given an open Nock term of type `open_nock_poly_term(v)` and a substitution
  function `f` from `v` to a closed Nock term, I return a closed
  Nock term (of type `nock_poly_term`).

  I am implemented using monadic bind (`tv_bind`).
  """
  @spec substitute(open_nock_poly_term(v), (v -> nock_poly_term)) ::
          nock_poly_term
        when v: term
  def substitute(term, f) do
    Term.tv_bind(f, term)
  end
end
