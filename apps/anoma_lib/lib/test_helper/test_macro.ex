defmodule TestHelper.TestMacro do
  @moduledoc """
  I am a module populated by macros associated to Anoma Testing.

  My use macro replaces ExUnit.case use macro with the caveat of ignoring
  the Assertions imports.

  Use me in order to define various macros to be used in tests.
  """

  alias __MODULE__

  require ExUnit.Case

  defmacro __using__(opts) do
    quote do
      require ExUnit.Assertions
      require ExUnit.Assertions
      require TestMacro

      import ExUnit.Assertions, only: [flunk: 1]
      import ExUnit.Callbacks
      import ExUnit.Case, only: [describe: 2, test: 1, test: 2, test: 3]
      import ExUnit.DocTest
      import TestMacro

      unless ExUnit.Case.__register__(__MODULE__, unquote(opts)) do
        use ExUnit.Callbacks
      end
    end
  end

  @doc """
  I call the `assert` macro from ExUnit.Assertions

  If the environment is :debug I pry on errors.
  """
  defmacro assert(expr, msg \\ nil) do
    message_parse(expr, :assert, msg)
  end

  @doc """
  I call the `refute` macro from ExUnit.Assertions

  If the environment is :debug I pry on errors.
  """
  defmacro refute(expr, msg \\ nil) do
    message_parse(expr, :refute, msg)
  end

  @doc """
  I call the `assert_receive` macro from ExUnit.Assertions

  If the environment is :debug I pry on errors.
  """
  defmacro assert_receive(expr, timeout \\ nil, failure_message \\ nil) do
    assertion_abstract(Mix.env(), :assert_receive, [
      expr,
      timeout,
      failure_message
    ])
  end

  @doc """
  I call the `assert_refute` macro from ExUnit.Assertions

  If the environment is :debug I pry on errors.
  """
  defmacro refute_receive(expr, timeout \\ nil, failure_message \\ nil) do
    assertion_abstract(Mix.env(), :refute_receive, [
      expr,
      timeout,
      failure_message
    ])
  end

  @doc """
  Depending on the given message, I call either `assert` or `refute` with
  different arities.
  """
  @spec message_parse(Macro.input(), atom(), String.t() | nil) ::
          Macro.input()
  def message_parse(expr, atom, msg) do
    assert_expression =
      unless msg do
        [expr]
      else
        [expr, [message: msg]]
      end

    assertion_abstract(Mix.env(), atom, assert_expression)
  end

  @doc """
  I catch a variable binding expression in debug mode and call `call_assert`
  on it.

  If the expression caught is variable-bining, after evaluating I also
  call said binding at AST level to bind it outside try-rescue.

  If the expression is not in debug mode, I simply call the assertion on
  the expression.
  """
  @spec assertion_abstract(atom(), atom(), Macro.input()) :: Macro.input()
  def assertion_abstract(:debug, atom, expr) do
    call_assert(atom, expr)
  end

  def assertion_abstract(_env, atom, expr) do
    assertion_alias(atom, expr)
  end

  @doc """
  I call the try functionality to capture the error.

  ### Pattern-Matching Variations

  - `call_assert(atom, [{:=, _, [left, _]}])` - Afterwards I bind the
                                                left side variables.
  - `call_assert(atom, expr)` - Just call the capture.
  """
  @spec call_assert(atom(), Macro.input()) :: Macro.input()
  def call_assert(atom, expr = [{:=, _, [left, _]}]) do
    quote do
      unquote(left) = unquote(quote_try(atom, expr))
    end
  end

  def call_assert(atom, expr) do
    quote_try(atom, expr)
  end

  @doc """
  I quote the error-capturing of expressions.
  """
  @spec quote_try(atom(), Macro.input()) :: Macro.input()
  def quote_try(atom, expr) do
    quote do
      try do
        unquote(try_assert(atom, expr))
      rescue
        _ ->
          {:current_stacktrace, list} =
            Process.info(self(), :current_stacktrace)

          IO.puts("\nAssert statement failed. Stacktrace:\n")
          for info <- list, do: info |> inspect() |> IO.puts()
          IO.puts("")
          require IEx
          IEx.pry()
      end
    end
  end

  @doc """
  I present a quoted expression to try depending on the input.

  ### Pattern-Matching Variations

  - `try_assert(atom, [{:=, _, _}])` - I use the assertion, print the
                                       binded variables to escape
                                       warnings and then print the
                                       original result
  - `try_assert(atom, expr)` - I use the assertion
  """
  @spec try_assert(atom(), Macro.input()) :: Macro.input()
  def try_assert(atom, expr = [{:=, _, _}]) do
    quote do
      right = unquote(assertion_alias(atom, expr))
      binding()
      right
    end
  end

  def try_assert(atom, expr) do
    assertion_alias(atom, expr)
  end

  @doc """
  I add the ExUnit.Assertions prefix to the function call on the AST level
  and apply it to the expression as a quoted structure.
  """
  @spec assertion_alias(atom(), Macro.input()) :: Macro.input()
  def assertion_alias(atom, expr) do
    {{:., [],
      [
        {:__aliases__, [alias: false], [:ExUnit, :Assertions]},
        atom
      ]}, [], expr}
  end
end
