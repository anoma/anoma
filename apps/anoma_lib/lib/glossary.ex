defmodule GlossaryBuilder do
  @moduledoc """
  I am the Glossary module. I provide a way to define and document terms throughout the codebase.

  I am useful to link back to the definition of a term, and to provide a consistent definition
  across the codebase in docstrings.

  ## Usage

  You can link back to a term in the glossary using generated links.
  A link is generated with the auto-link syntax.
  The following example will link to the term `anoma` in the module `Anoma.Glossary`.

  ``[anoma](`Anoma.Glossary.anoma/0`)``

  > **Note** If the term is defined in the glossary using spaces, the spaces will be replaced with
  underscores.

  ### Public API

  I have the following public functionality:

  #### Macros

  - `define/2`
  """

  @spec __using__(any()) ::
          {:import,
           [{:context, GlossaryBuilder} | {:end_of_expression, [...]}, ...],
           [GlossaryBuilder, ...]}
  @doc """
  I am the __using__ macro for the Glossary module.
  I import the Glossary module and make my macros available.
  """
  defmacro __using__(_options) do
    quote do
      import unquote(__MODULE__)
    end
  end

  @doc """
  I am the `define/2` macro. I define a new entry in a glossary.

  ## Example

  The following snippet defines the term `anoma`.

  ```
  define anoma do
    \"\"\"
    An intent-centric architecture decentralized counterparty discovery, solving,
    information flow control, and atomic multi‑chain settlement.
    \"\"\"
  end
  ```

  Spaces in the names of definitions are automatically replaced with underscores.

  ```
  define transaction candidate do
    \"\"\"
    A transaction candidate is `t:Noun.t/0` that evaluates to a valid or
    invalid `transaction` for a specified
    `t:Anoma.Node.Executor.Worker.backend/0`
    \"\"\"
  end
  ```

  """
  @spec define(String.t() | atom(), do: String.t()) :: Macro.t()
  defmacro define(label, do: description) do
    label = sanitize_label(label) |> String.to_atom()

    quote do
      @unquote(label)(unquote(description))
      @doc unquote(description)
      @spec unquote(label)() :: String.t()
      def unquote(label)(), do: unquote(description)
    end
  end

  ############################################################
  #                          Helpers                         #
  ############################################################

  # I turn the given label into a string.
  # I accept atoms, strings, or ast nodes that represent a function call.
  @type ast_node :: {atom(), any(), nil | [ast_node()]}
  @spec sanitize_label(ast_node | String.t() | atom()) :: String.t()
  defp sanitize_label({label, _, nil}) do
    sanitize_label(label)
  end

  # function call
  defp sanitize_label({label, _, labels}) do
    [label | labels]
    |> Enum.map_join("_", fn label -> sanitize_label(label) end)
  end

  # atom
  defp sanitize_label(label) when is_atom(label) do
    label |> Atom.to_string() |> sanitize_label()
  end

  # string
  defp sanitize_label(label) when is_binary(label) do
    label
    |> String.replace(~r/[^a-zA-Z0-9]/, "_")
    |> String.trim()
  end
end
