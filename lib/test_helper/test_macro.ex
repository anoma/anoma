defmodule TestHelper.TestMacro do
  @moduledoc """
  I am a module populated by macros associated to Anoma Testing.

  My use macro replaces ExUnit.case use macro with the caveat of ignoring
  the Assertions imports.

  Use me in order to define various macros to be used in tests.
  """
  require ExUnit.Case

  alias __MODULE__

  defmacro __using__(opts) do
    quote do
      require TestMacro
      require ExUnit.Assertions

      import TestMacro

      unless ExUnit.Case.__register__(__MODULE__, unquote(opts)) do
        use ExUnit.Callbacks
      end

      import ExUnit.Callbacks
      import ExUnit.Case, only: [describe: 2, test: 1, test: 2, test: 3]
      import ExUnit.DocTest

      require ExUnit.Assertions

      import ExUnit.Assertions, only: [flunk: 1]
    end
  end

  @doc """
  I call the `assert` macro from ExUnit.Assertions

  If the environment is :debug I pry on errors.
  """
  defmacro assert(expr) do
    assertion_abstract(Mix.env(), :assert, [quote(do: unquote(expr))])
  end

  @doc """
  I call the `refute` macro from ExUnit.Assertions

  If the environment is :debug I pry on errors.
  """
  defmacro refute(expr) do
    assertion_abstract(Mix.env(), :refute, [quote(do: unquote(expr))])
  end

  @doc """
  I call the `assert_receive` macro from ExUnit.Assertions

  If the environment is :debug I pry on errors.
  """
  defmacro assert_receive(expr, timeout \\ nil, failure_message \\ nil) do
    assertion_abstract(Mix.env(), :assert_receive, [
      quote(do: unquote(expr)),
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
      quote(do: unquote(expr)),
      timeout,
      failure_message
    ])
  end

  @doc """
  I catch a variable binding expression in debug mode and call `call_assert`
  on it.

  If the expression caught is variable-bining, after evaluating I also
  call said binding at AST level to bind it outside try-rescue.

  If the expression is not in debug mode, I simply call the assertion on
  the expression.
  """
  def assertion_abstract(:debug, atom, [{:=, _ctx, [_left, _right]}] = [expr]) do
    call_assert(atom, [expr])
    expr
  end

  def assertion_abstract(:debug, atom, expr) do
    call_assert(atom, expr)
  end

  def assertion_abstract(_env, atom, expr) do
    {assertion_alias(atom), [], expr}
  end

  @doc """
  I perform the try-rescue functionality, directly evaluating the assertion.
  """
  def call_assert(atom, expr) do
    quote do
      try do
        unquote({assertion_alias(atom), [], expr})
      rescue
        _ -> unquote(trace_and_pry())
      end
    end
  end

  @doc """
  I print the current stacktrace for the user and pry.
  """
  def trace_and_pry() do
    quote do
      {:current_stacktrace, list} = Process.info(self(), :current_stacktrace)
      IO.puts("\nAssert statement failed. Stacktrace:\n")
      for info <- list, do: info |> inspect() |> IO.puts()
      IO.puts("")
      require IEx
      IEx.pry()
    end
  end

  @doc """
  I add the ExUnit.Assertions prefix to the function call on the AST level.
  """
  def assertion_alias(atom) do
    {:., [],
     [
       {:__aliases__, [alias: false], [:ExUnit, :Assertions]},
       atom
     ]}
  end
end
