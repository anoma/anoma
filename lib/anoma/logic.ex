defmodule Anoma.Logic do
  use TypedStruct

  # these typedstructs are mainly short hands over new modules, with
  # their own types

  typedstruct module: Inc do
    @moduledoc """
    I represent the Increment instruction.

    I take 1 input and produce an output where the input is incremented.
    """
    field(:in1, Anoma.Logic.t())
  end

  typedstruct module: Neg do
    @moduledoc """
    I represent the Negation instruction

    I take 1 input and produce an output where the input is negated
    """
    field(:in1, Anoma.Logic.t())
  end

  typedstruct module: Add do
    @moduledoc """
    I represent the Addition instruction.

    I take 2 inputs and produce an output where the inputs are added
    together
    """
    field(:in1, Anoma.Logic.t())
    field(:in2, Anoma.Logic.t())
  end

  typedstruct module: Mul do
    @moduledoc """
    I represent the Multiplication instruction.

    I take 2 inputs and produce an output where the inputs are added
    together
    """
    field(:in1, Anoma.Logic.t())
    field(:in2, Anoma.Logic.t())
  end

  typedstruct module: Lt do
    @moduledoc """
    I represent the Less Than instruction.

    I take 2 inputs and produce an output where 0 is true and 1 is false
    """
    field(:in1, Anoma.Logic.t())
    field(:in2, Anoma.Logic.t())
  end

  typedstruct module: Branch do
    @moduledoc """
    I represent the Branching/If instruction

    I take 3 inputs.

    ### Fields

    - in1 Is the value to branch on, 0 is true and 1 is false
    - ip1 Is the logic to run if it's true
    - ip2 Is the logic to run if it's false
    """
    field(:in1, Anoma.Logic.t())
    field(:ip1, Anoma.Logic.t())
    field(:ip2, Anoma.Logic.t())
  end

  alias Anoma.Logic.{Neg, Branch, Lt, Mul, Add, Inc}

  @typedoc """
  I represent a compiled op. I am what will be compiled to verify that
  some constraint is satisfied. I am less featureful than `op`, but I
  am the canonical representation of `op` when compiled.
  """

  # These are the known ops, not the full set.
  # these are what will be compiled

  @type compiled_op() ::
          Lt.t()
          | Branch.t()
          | Neg.t()
          | Mul.t()
          | Add.t()
          | Inc.t()
          | number()

  @typedoc """

  I represent a logic. Most instructions are operations on some field
  element/register. I am is what interpreted by solvers and will be
  compiled for efficient verification. I may be compiled into a
  `compiled_op()`
  """

  @type t() :: Anoma.Eval.t()

  @spec is_compiled_op(term()) :: boolean()
  def is_compiled_op(%Lt{}), do: true
  def is_compiled_op(%Neg{}), do: true
  def is_compiled_op(%Mul{}), do: true
  def is_compiled_op(%Add{}), do: true
  def is_compiled_op(%Inc{}), do: true
  def is_compiled_op(%Branch{}), do: true
  def is_compiled_op(t) when is_number(t), do: true
  def is_compiled_op(_), do: false

  # no branch
  def interpreter_op(%Lt{}), do: &lt/2
  def interpreter_op(%Neg{}), do: &-/1
  def interpreter_op(%Mul{}), do: &*/2
  def interpreter_op(%Add{}), do: &+/2
  def interpreter_op(%Inc{}), do: &(1 + &1)

  defp lt(arg1, arg2) do
    if arg1 < arg2 do
      0
    else
      1
    end
  end

  defimpl Anoma.Eval, for: [Lt, Neg, Mul, Add, Inc] do
    def apply(term, tx) do
      arguments =
        term
        |> Map.from_struct()
        |> Map.values()
        |> Enum.map(fn term -> Anoma.Eval.apply(term, tx) end)

      term
      |> Anoma.Logic.interpreter_op()
      |> Kernel.apply(arguments)
    end

    def compile(term) do
      term
    end
  end

  defimpl Anoma.Eval, for: Branch do
    def apply(term, tx) do
      if Anoma.Eval.apply(term.in1, tx) == 0 do
        Anoma.Eval.apply(term.ip1, tx)
      else
        Anoma.Eval.apply(term.ip2, tx)
      end
    end

    def compile(term) do
      term
    end
  end

  defimpl Anoma.Eval, for: Integer do
    def apply(term, _tx) do
      term
    end

    def compile(term) do
      term
    end
  end
end
