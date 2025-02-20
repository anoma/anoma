defmodule ExtNock do
  @moduledoc """
  Macros for writing open polynomial Nock terms as an internal DSL.

  These macros compile our extended (open) Nock term language
  into the underlying representation provided by `NockPoly`.

  In the future, these macros will support metavariables and substitution,
  allowing library definitions to be written in a DSL that resembles Hoon.
  """

  require NockPoly

  # I contain `defmacro`s (and `defmacros` only), to distinguish them
  # for ignoring code coverage (which does not know how to tell whether
  # a macro has been expanded and executed).
  defmodule MacroDefs do
  end
end
