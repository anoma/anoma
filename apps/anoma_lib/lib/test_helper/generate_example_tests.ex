defmodule TestHelper.GenerateExampleTests do
  @moduledoc """
  I generate test cases for all public functions with arity 0 from the given module.

  This macro inspects the given `module` at compile time, retrieves all its public
  functions with arity 0 (excluding any functions specified in the `:skip` option
  and the `__struct__/0` function), and generates a test case for each one.

  ### Usage

  ```elixir
  use TestHelper.GenerateExampleTests, for: EMyModule, skip: [:func1, :func2]
  ```

  ### Options

  - `:for` - (Required) The module from which to retrieve the public functions.
  - `:skip` - (Optional) A list of function names (as atoms) to exclude from the generated tests.

  ### Notes

  - If a function specified in `:skip` is not a public function with arity 0 in the module,
    a compile-time error is raised.

  """

  @excluded_functions [:__struct__]

  defmacro __using__(opts) do
    module = extract_module(opts, __CALLER__)
    skip_functions = extract_skip_functions(opts)
    functions = extract_module_functions(module, skip_functions)

    tests =
      for func <- functions do
        test_name = "example #{inspect(module)}.#{func}/0"

        quote do
          @tag unquote(func)
          test unquote(test_name) do
            unquote(module).unquote(func)()
          end
        end
      end

    quote do
      (unquote_splicing(tests))
    end
  end

  defp extract_module(opts, caller) do
    module = Keyword.fetch!(opts, :for)
    module = Macro.expand(module, caller)

    if not is_atom(module) do
      raise ArgumentError,
            "Expected :for option to be an atom, got: #{inspect(module)}"
    end

    if not Code.ensure_loaded?(module) do
      raise ArgumentError, "Module #{inspect(module)} is not loaded"
    end

    module
  end

  defp extract_skip_functions(opts) do
    skip_functions = Keyword.get(opts, :skip, [])

    if not (is_list(skip_functions) and Enum.all?(skip_functions, &is_atom/1)) do
      raise ArgumentError, ":skip option must be a list of atoms"
    end

    skip_functions
  end

  defp extract_module_functions(module, skip_functions) do
    module_functions = get_module_functions(module)
    validate_skip_functions(skip_functions, module_functions, module)
    module_functions -- skip_functions
  end

  defp get_module_functions(module) do
    module.__info__(:functions)
    |> Enum.filter(fn {func, arity} ->
      arity == 0 and func not in @excluded_functions
    end)
    |> Enum.map(fn {func, _arity} -> func end)
  end

  defp validate_skip_functions(skip_functions, module_functions, module) do
    undefined_functions = skip_functions -- module_functions

    if undefined_functions != [] do
      raise ArgumentError,
            "Cannot skip undefined functions: #{inspect(undefined_functions)} in module #{inspect(module)}"
    end
  end
end
