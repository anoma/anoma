defprotocol Anoma.Eval do
  @moduledoc """
  I represent being Evalable.

  This comes in two forms:

  1. Interpretable
  2. Compilable

  Being Compilable means that I can compile you into the base ops, see
  `Anoma.Logic.compiled_op()` for more information.

  Being Interpertable means that wen can run the code inline given
  some partial transaction.

  For the time being, many forms are interpretable but not compilable.

  This is due to the compilable instructions being a bit too limited.
  """

  @type io() :: :in | :out
  @type input() :: {io, list(Anoma.Resource.t()), list(Anoma.Resource.t())}

  # what is the output type?!?!?!?!?
  @doc """

  I `apply` the given logic onto a partial transaction.

  """
  @spec apply(t(), input()) :: any()
  def apply(logic, tx)

  # let's compile!!!
  # compilation may fail, so :err could be returned

  @doc """

  I `compile` the given term into a `Anoma.Logic.compiled_op()`
  form.

  ### Properties

       iex(1)> apply(logic, tx) == apply(compile(logic), tx)
       true

  """
  @spec compile(t()) :: t() | :err
  def compile(term)
end
