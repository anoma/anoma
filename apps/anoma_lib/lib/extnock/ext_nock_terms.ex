defmodule ExtNock.ExtNockTerms do
  alias NockPoly.Term
  alias NockPoly.NockTerms
  require Noun

  @moduledoc """
  I provide extended Nock term functionality, adding semantic constructors
  beyond the basic `:atom` and `:cell` constructors of standard Nock.

  Extended terms comprise an internal supserset of Nock whch can be compiled
  down to standard Nock formulas for execution.
  """
  alias NockPoly.Term
  alias NockPoly.NockTerms
  # Alias utility functions for creating terms
  alias NockPoly.Term, as: T

  @typedoc """
  I represent an extended constructor for Nock terms, providing higher-level
  semantic operations beyond raw Nock.

  As development progresses, I will be expanded to include more constructors
  for all Nock operations and library functions.
  """
  @type ext_term_ctor ::
          :slot
          | :constant
          | :evaluate
          | :cell_test
          | :incr
          | :eq
          | :ife
          | :compose
          | :push
          | :invoke
          | :replace
          | :hint
          | :create_core_1
          | :call_core_1

  @typedoc """
  I am a constructor for extended Nock terms, which can be either a standard
  Nock term constructor or an extended constructor.
  """
  @type ext_nock_term_ctor :: NockTerms.nock_term_ctor() | ext_term_ctor()

  @typedoc """
  I am an extended polynomial Nock term with variables of type `v`.
  I extend the standard `open_nock_poly_term` with additional constructors.
  """
  @type ext_nock_poly_term(v) :: Term.tv(ext_nock_term_ctor(), v)

  @doc """
  I provide a type specification for extended constructors, defining
  their expected arities.

  I return {:ok, arity} for valid constructors with their expected arity,
  or {:standard, ctor} for (what I expect to be) a standard Nock constructor.
  """
  @spec ext_tspec(ext_term_ctor()) ::
          {:ok, non_neg_integer()} | {:standard, NockTerms.nock_term_ctor()}
  def ext_tspec(:slot), do: {:ok, 1}
  def ext_tspec(:constant), do: {:ok, 1}
  def ext_tspec(:evaluate), do: {:ok, 2}
  def ext_tspec(:cell_test), do: {:ok, 1}
  def ext_tspec(:incr), do: {:ok, 1}
  def ext_tspec(:eq), do: {:ok, 2}
  def ext_tspec(:ife), do: {:ok, 3}
  def ext_tspec(:compose), do: {:ok, 2}
  def ext_tspec(:push), do: {:ok, 2}
  def ext_tspec(:invoke), do: {:ok, 2}
  def ext_tspec(:replace), do: {:ok, 3}
  def ext_tspec(:hint), do: {:ok, 2}
  def ext_tspec(:create_core_1), do: {:ok, 3}
  def ext_tspec(:call_core_1), do: {:ok, 1}
  def ext_tspec(ctor), do: {:standard, ctor}

  @doc """
  I provide a type specification function for all term constructors
  (extended constructors or standard Nock constructors).

  I first check if the constructor is an extended constructor, and if not,
  I delegate to the standard Nock term typespec function.
  """
  @spec ext_nock_poly_tspec(ext_nock_term_ctor()) ::
          {:ok, non_neg_integer()} | :invalid_constructor
  def ext_nock_poly_tspec(ctor) do
    case ext_tspec(ctor) do
      {:ok, arity} ->
        {:ok, arity}

      {:standard, sctor} ->
        NockTerms.nock_tspec(sctor)
    end
  end

  @doc """
  I typecheck an extended Nock polynomial term.

  I use the FinPolyF typecheck system with our extended constructor typespec
  function to validate the entire term tree.
  """
  @spec typecheck(ext_nock_poly_term(v)) ::
          NockPoly.FinPolyF.check_result(ext_nock_term_ctor(), v)
        when v: term()
  def typecheck(term) do
    NockPoly.FinPolyF.typecheck_v(
      term,
      {&ext_nock_poly_tspec/1, &NockPoly.FinPolyF.vspec_ok/1}
    )
  end

  @type ext_nock_compile_error_list(v) ::
          [
            NockPoly.FinPolyF.typecheck_error(ext_nock_term_ctor(), v)
          ]

  @type compile_result(v) ::
          {:ok, NockTerms.open_nock_poly_term(v)}
          | {:error, ext_nock_compile_error_list(v)}

  @doc """
  I implement the algebra for compiling extended term constructors to standard
  Nock term constructors.

  I should only be called with terms that have already been typechecked,
  as I assume that each constructor has the correct arity.

  I handle each extended constructor by transforming it into an equivalent
  representation using only standard Nock constructors.
  """
  @spec compile_algebra() ::
          (Term.termf(
             ext_nock_term_ctor(),
             NockTerms.open_nock_poly_term(v)
           ) ->
             NockTerms.open_nock_poly_term(v))
        when v: term()
  def compile_algebra do
    fn
      {:slot, [addr]} ->
        T.com_tv(:cell, [T.com_tv({:atom, 0}, []), addr])

      {:constant, [val]} ->
        T.com_tv(:cell, [T.com_tv({:atom, 1}, []), val])

      {:evaluate, [b, c]} ->
        # Structure: [2 b c] which is really [2 [b c]]
        # This compiles to *[a 2 b c] -> *[*[a b] *[a c]]
        T.com_tv(:cell, [T.com_tv({:atom, 2}, []), T.com_tv(:cell, [b, c])])

      {:cell_test, [a]} ->
        # Structure: [3 a]
        # This compiles to *[a 3 b] -> ?*[a b]
        # Returns 0 if the result of *[a b] is a cell, 1 if it's an atom
        T.com_tv(:cell, [T.com_tv({:atom, 3}, []), a])

      {:incr, [a]} ->
        # Structure: [4 a]
        # This compiles to *[a 4 b] -> +*[a b]
        # Returns the atom that results from incrementing *[a b] by 1
        T.com_tv(:cell, [T.com_tv({:atom, 4}, []), a])

      {:eq, [a, b]} ->
        # Structure: [5 a b]
        # This compiles to *[a 5 b c] -> =(*[a b] *[a c])
        # Returns 0 if *[a b] equals *[a c], 1 otherwise
        T.com_tv(:cell, [T.com_tv({:atom, 5}, []), T.com_tv(:cell, [a, b])])

      {:ife, [test, then_branch, else_branch]} ->
        # Structure: [6 test 0-case 1-case]
        # *[a 6 b c d] is equivalent to *[a *[[c d] 0 *[[2 3] 0 *[a 4 4 b]]]]
        # If *[a b] equals 0, returns *[a c]; if 1, returns *[a d]
        T.com_tv(:cell, [
          T.com_tv({:atom, 6}, []),
          T.com_tv(:cell, [
            test,
            T.com_tv(:cell, [then_branch, else_branch])
          ])
        ])

      {:compose, [b, c]} ->
        # Structure: [7 b c]
        # This compiles to *[a 7 b c] -> *[*[a b] c]
        # First applies b to the subject (a), then applies c to the result
        T.com_tv(:cell, [T.com_tv({:atom, 7}, []), T.com_tv(:cell, [b, c])])

      {:push, [b, c]} ->
        # Structure: [8 b c]
        # This compiles to *[a 8 b c] -> *[[*[a b] a] c]
        # First evaluates b on subject to get a value
        # Then builds a cell [value subject] and evaluates c on that cell
        T.com_tv(:cell, [T.com_tv({:atom, 8}, []), T.com_tv(:cell, [b, c])])

      {:invoke, [b, c]} ->
        # Structure: [9 b c]
        # This compiles to *[a 9 b c] -> *[*[a c] 2 [0 1] 0 b]
        # Creates a core (by evaluating c), then pulls arm b from that core
        T.com_tv(:cell, [T.com_tv({:atom, 9}, []), T.com_tv(:cell, [b, c])])

      {:replace, [axis, replacement, subject]} ->
        # Structure: [10 [axis replacement] subject]
        # This compiles to *[a 10 [b c] d] -> #[b *[a c] *[a d]]
        # Replaces at axis b in *[a d] with the result of *[a c]
        T.com_tv(:cell, [
          T.com_tv({:atom, 10}, []),
          T.com_tv(:cell, [T.com_tv(:cell, [axis, replacement]), subject])
        ])

      {:hint, [hint, formula]} ->
        # Structure: [11 hint formula]
        # *[a 11 [b c] d]          -> *[[*[a c] *[a d]] 0 3]
        # *[a 11 b c], `b` an atom -> *[a c]
        #
        # Either way, the visible effect is to ignore the hint and evaluate
        # the formula against a given subject, but the hint may be used
        # by the interpreter, and if the hint is a cell, then the second
        # component of it is evaluated against the subject to produce the
        # hint which the interpreter may consider -- and that evaluation
        # might in particular crash!
        T.com_tv(:cell, [
          T.com_tv({:atom, 11}, []),
          T.com_tv(:cell, [hint, formula])
        ])

      # This is the simplest core-creation instruction we define.
      # It is a core which, when invoked, generates a core, with a battery
      # consisting of a formula passed in to the instruction, a default
      # argument (also passed in to the instruction) and the battery of the
      # core-creating core, and a payload which is also passed in to the
      # instruction.  In its use by the `:call_core_1` instruction defined
      # below, it pushes all of that onto the stack and then the core that
      # it creates is invoked (with the default argument substituted into).
      {:create_core_1, [formula, default_arg, payload]} ->
        T.com_tv(:cell, [
          # This is the battery.  It is a list of length three, so
          # when invoked it produces a list of length three, with each
          # element produced by invocation of the corresponding element
          # of the battery against the subject.
          #
          # The element simply uses the constant instruction (ignoring the
          # subject).  That constant is code-valued, and is passed in to
          # the instruction as an argument.
          T.com_tv(:cell, [
            T.com_tv(:cell, [
              T.com_tv({:atom, 1}, []),
              formula
            ]),
            T.com_tv(:cell, [
              default_arg,
              T.com_tv(:cell, [
                T.com_tv({:atom, 0}, []),
                T.com_tv({:atom, 2}, [])
              ])
              # The result of evaluating this battery against a subject
              # is `[(formula) (default arg) (slot 2 of the subject)]`.
              # Since, when invoked by :call_core_1, the core itself is
              # the subject, slot 2 will be this battery itself.
            ])
          ]),
          # payload
          payload
        ])

      {:call_core_1, [arg_value]} ->
        T.com_tv(:cell, [
          # The "call" as a whole is a push -- it uses the subject to
          # build a formula, then pushes that formula onto the subject,
          # thus extending the subject before calling a formula on it.
          T.com_tv({:atom, 8}, []),
          T.com_tv(:cell, [
            T.com_tv(:cell, [
              # The first argument to the push is the formula which acts
              # on the subject to produce the noun to push onto the subject.
              # That formula is a core creation and invocation; the core
              # is made from slot 1 -- i.e. the entire subject -- and the
              # arm pulled from it is axis 2 (which is the entire battery,
              # so the expected subject is a one-arm core). Thus, the effect of
              # the :push is to extend the subject by the result of treating it as
              # a one-armed core and firing it, then calling the next formula
              # below.  We have seen the result of firing the arm of the
              # core created by `create_core_1` which will be used as the
              # subject, and when we invoke it as a core, its subject is
              # itself, so the noun pushed onto the subject by the :push
              # is `[(formula) (default argument) (battery of subject core)]`.
              T.com_tv({:atom, 9}, []),
              T.com_tv(:cell, [
                T.com_tv({:atom, 2}, []),
                T.com_tv(:cell, [
                  T.com_tv({:atom, 0}, []),
                  T.com_tv({:atom, 1}, [])
                ])
              ])
            ]),
            T.com_tv(:cell, [
              # Below is the formula invoked by the :push instruction after
              # extending the subject.  Like the formula which extends the
              # subject above, it creates a core and fires it, meaning it
              # evaluates its battery against the whole core itself.
              # However, its core creation is more involved than simply
              # "take the whole subject".
              T.com_tv({:atom, 9}, []),
              T.com_tv(:cell, [
                T.com_tv({:atom, 2}, []),
                T.com_tv(:cell, [
                  T.com_tv({:atom, 10}, []),
                  T.com_tv(:cell, [
                    # This code creates the core invoked by the :push instruction.
                    # It is a replacement of axis 6 of slot 2 of the subject; because
                    # we just extended the subject, its slot 2 is precisely the
                    # extension that we created.  That extension, in turn, as
                    # described above, is the result of treating the subject as a
                    # one-armed core and firing it.
                    #
                    # Consequently, we expect the subject to be a one-armed core
                    # which creates something at axis 6 which we intend to replace.
                    # That is the sample -- the placeholder where the arguments
                    # will be plugged in.
                    T.com_tv(:cell, [
                      T.com_tv({:atom, 6}, []),
                      # And this is how we generate the arguments which we plug in to
                      # the sample:  we take slot 3 of the subject, which, since we
                      # just extended the subject, is the _original_ subject, before
                      # the push.  Then we operate on that subject with some formula.
                      # In this specific case, as it turns out, we ignore the
                      # subject -- the formula just produces a constant atom whose
                      # value is `inc_call_arg`.  That is, we are presuming the
                      # subject to be a one-armed core with one argument, which
                      # has a sample at axis 6 to be replaced with the value of
                      # the argument for a particular invocation.
                      T.com_tv(:cell, [
                        T.com_tv({:atom, 7}, []),
                        T.com_tv(:cell, [
                          T.com_tv(:cell, [
                            T.com_tv({:atom, 0}, []),
                            T.com_tv({:atom, 3}, [])
                          ]),
                          T.com_tv(:cell, [
                            T.com_tv({:atom, 1}, []),
                            arg_value
                          ])
                        ])
                      ])
                    ]),
                    T.com_tv(:cell, [
                      T.com_tv({:atom, 0}, []),
                      T.com_tv({:atom, 2}, [])
                    ])
                  ])
                ])
              ])
            ])
          ])
        ])

      # When we put together our descriptions of :create_core_1 and
      # :call_core_1, we find that the overall effect of evaluating
      # a :call_core_1 (the formula) against a :create_core_1 (the subject)
      # is as follows:
      #
      # - The push instruction extends the subject simply by treating
      #   it as a core and firing it.  The subject _is_ a core, and its
      #   battery, as we have seen, produces
      #   `[(formula) (default argument) (slot 2 of the subject)]`.
      #   When firing a core, the subject is the core itself, so this becomes
      #   `[(formula) (default argument) (battery of the subject core)]`.
      #   Thus that list is pushed onto the subject, which is, again, a
      #   core, so the new subject is
      #   `[[(formula) (default argument) (battery of subject core)] (subject core)]`.
      # - The push instruction, having extended the subject, calls a
      #   formula, which is also a creation and invocation of a core.
      #   This core is generated by a replacement of axis 6 of
      #   slot 2 of the new subject resulting from the push.  Slot 2 of
      #   the new subject is what we just pushed, which is quoted above.
      #   Slot 6 of that is the default argument produced by the constant 0 operation
      #   in the battery of the subject core.  It is the
      #   argument that will be used if not replaced (although :call_core_1
      #   always replaces it, other invocations of the core might not).
      #   So the core produced by the replacement is
      #   `[[(formula) (replacement value) (battery of subject core)] (subject core)]`.
      #   When that core is activated, the battery -- which is the code
      #   provided as the formula to :create_core_1 -- is invoked with the entire
      #   core as the subject, with slot 6 of that entire core being the
      #   replacement value.

      # For standard Nock constructors, keep them as-is but wrap in :tcom tag
      {ctor, children} ->
        T.com_tv(ctor, children)
    end
  end

  @doc """
  I convert an extended Nock term to a standard Nock term by first typechecking
  and then compiling any extended constructors down to their raw Nock equivalents.

  I return {:ok, compiled_term} on success or {:error, errors} if typechecking fails.
  """
  @spec compile_to_nock_term(ext_nock_poly_term(v)) ::
          compile_result(v)
        when v: term()
  def compile_to_nock_term(term) do
    # First typecheck the term
    case typecheck(term) do
      :ok ->
        # If valid, apply the algebra to transform the term
        compiled_term =
          Term.eval(compile_algebra(), &Function.identity/1, term)

        {:ok, compiled_term}

      {:error, errors} ->
        {:error, errors}
    end
  end

  @doc """
  I convert an extended Nock term to a standard Nock term, raising an error
  if compilation fails.

  This is a convenience wrapper around compile_to_nock_term that unwraps the
  {:ok, term} result or raises an error with the typecheck failures.
  """
  @spec compile_to_nock_term!(ext_nock_poly_term(v)) ::
          NockTerms.open_nock_poly_term(v)
        when v: term()
  def compile_to_nock_term!(term) do
    case compile_to_nock_term(term) do
      {:ok, compiled} ->
        compiled

      {:error, errors} ->
        raise "Invalid extended Nock term: #{inspect(errors)}"
    end
  end

  @doc """
  I substitute variables in an extended Nock term using the provided substitution
  function, then return the result.

  This is a convenience wrapper around Term.tv_bind/2.
  """
  @spec substitute(ext_nock_poly_term(v), (v -> ext_nock_poly_term(w))) ::
          ext_nock_poly_term(w)
        when v: term(), w: term()
  def substitute(term, f) do
    Term.tv_bind(f, term)
  end

  @type to_noun_result(v) ::
          {:ok, Noun.t()}
          | {:error, ext_nock_compile_error_list(v)}

  @doc """
  I convert an extended Nock term to a standard Nock noun by first compiling
  any extended constructors, then converting to a noun.

  I return {:ok, noun} on success or {:error, errors} if typechecking fails.

  This is the main function for preparing extended terms for execution.
  """
  @spec to_noun(ext_nock_poly_term(v)) :: to_noun_result(v)
        when v: term()
  def to_noun(term) do
    case compile_to_nock_term(term) do
      {:ok, compiled} ->
        {:ok, NockTerms.to_noun(compiled)}

      {:error, errors} ->
        {:error, errors}
    end
  end

  @doc """
  I convert an extended Nock term to a standard Nock noun, raising an error
  if compilation fails.

  This is a convenience wrapper around to_noun that unwraps the
  {:ok, noun} result or raises an error with the typecheck failures.
  """
  @spec to_noun!(ext_nock_poly_term(v)) :: Noun.t()
        when v: term()
  def to_noun!(term) do
    case to_noun(term) do
      {:ok, noun} ->
        noun

      {:error, errors} ->
        raise "Invalid extended Nock term: #{inspect(errors)}"
    end
  end

  @doc """
  I convert an s-expression directly to a Nock noun in one step.

  This is a convenience function that combines from_sexpr and to_noun.
  The s-expression must not contain any variables.
  """
  @spec sexpr_to_noun(nock_poly_sexpr(none())) :: {:ok, Noun.t()} | :error
  def sexpr_to_noun(sexpr) do
    with {:ok, term} <- from_sexpr(sexpr),
         {:ok, noun} <- to_noun(term) do
      {:ok, noun}
    else
      _ -> :error
    end
  end

  @doc """
  I convert an s-expression directly to a Nock noun, raising an error
  if the conversion fails at any step.

  This is a convenience wrapper around sexpr_to_noun that unwraps the
  {:ok, noun} result or raises an error if conversion fails.
  The s-expression must not contain any variables.
  """
  @spec sexpr_to_noun!(nock_poly_sexpr(none())) :: Noun.t()
  def sexpr_to_noun!(sexpr) do
    case sexpr_to_noun(sexpr) do
      {:ok, noun} ->
        noun

      _ ->
        raise "S-expression does not represent a Nock noun: #{inspect(sexpr)}"
    end
  end

  @typedoc """
  I am a convenience S-expression representation for Nock polynomial terms
  with variables of type `v`.

  I can be:
  - A Nock atom (satisfying Noun.is_noun_atom/1)
  - A variable term of the form {:var, v}
  - A list of nock_poly_sexpr(v) (representing a cell node)
  - An extended constructor term of the form {:constructor, [args]}

  The args list must match the arity of the constructor as defined in ext_tspec.
  As extended constructors are added, they will all follow this consistent format.
  """
  @type nock_poly_sexpr(v) ::
          Noun.noun_atom()
          | {:var, v}
          | [nock_poly_sexpr(v)]
          | {ext_term_ctor(), [nock_poly_sexpr(v)]}

  @doc """
  I convert an S-expression representation of a Nock term into an ext_nock_poly_term.

  For a term of the form {:var, v}, I return a variable term.
  For a term which is a Nock atom, I return an atom term.
  For a term which is a list, I recursively convert both the head and tail elements,
  and if both succeed, return a cell term.
  For extended constructors like {:wut, subject}, I handle them appropriately.

  As extended constructors are added, this function will be expanded to handle them.

  I return :error if the term is an empty list or if any recursive conversion fails.
  """
  @spec from_sexpr(nock_poly_sexpr(v)) ::
          {:ok, ext_nock_poly_term(v)} | :error
        when v: term()
  def from_sexpr(sexpr) do
    cond do
      # Variable case
      match?({:var, _}, sexpr) ->
        {:var, v} = sexpr
        {:ok, T.var_tv(v)}

      # Generic handling for any extended constructor
      is_tuple(sexpr) and tuple_size(sexpr) == 2 and is_atom(elem(sexpr, 0)) and
          is_list(elem(sexpr, 1)) ->
        {ctor, args} = sexpr

        case ext_tspec(ctor) do
          {:ok, expected_arity} ->
            if length(args) != expected_arity do
              :error
            else
              args_result = Enum.map(args, &from_sexpr/1)

              if Enum.any?(args_result, &(&1 == :error)) do
                :error
              else
                converted_args =
                  Enum.map(args_result, fn {:ok, term} -> term end)

                {:ok, T.com_tv(ctor, converted_args)}
              end
            end
        end

      # An empty s-expression is not a valid `ext_nock_poly_term`;
      # a polynomial term always has a constructor
      sexpr == [] ->
        :error

      # Treating a singleton list simply as the element which it contains
      # allows us to avoid special-casing in the multi-element-list case
      # (we treat S-expression lists as improper lists made of nested cells)
      is_list(sexpr) and length(sexpr) == 1 ->
        [head] = sexpr
        from_sexpr(head)

      # As with Nock, a multi-element list is made of nested cells
      is_list(sexpr) ->
        [head | tail] = sexpr

        with {:ok, head_term} <- from_sexpr(head),
             {:ok, tail_term} <- from_sexpr(tail) do
          {:ok, T.com_tv(:cell, [head_term, tail_term])}
        else
          _ -> :error
        end

      Noun.is_noun_atom(sexpr) ->
        {:ok, T.com_tv({:atom, sexpr}, [])}

      true ->
        :error
    end
  end

  @doc """
  I convert an S-expression representation of a Nock term into an
  `ext_nock_poly_term`, raising an error if conversion fails.

  I am a convenience wrapper around `from_sexpr/1` that unwraps the
  {:ok, term} result or raises an error if conversion fails.  Because
  the s-expression form is intended for internal convenience, we will
  typically use it in cases where we are explicitly generating expressions
  which are guaranteed to convert successfully to polynomial terms.

  See `from_sexpr/1` for details on the conversion process.
  """
  @spec from_sexpr!(nock_poly_sexpr(v)) :: ext_nock_poly_term(v)
        when v: term()
  def from_sexpr!(sexpr) do
    case from_sexpr(sexpr) do
      {:ok, term} ->
        term

      :error ->
        raise "S-expression does not represent a Nock polynomial term: #{inspect(sexpr)}"
    end
  end
end
