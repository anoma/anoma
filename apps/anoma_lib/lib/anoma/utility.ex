defmodule Anoma.Utility do
  @moduledoc """

  I provide utility functions for users.

  ### Public API

  I possess the following public API:

  - message_label/1
  """

  @doc """
  Helps labeling for `Kino.Process.seq_trace/2`, for the Router abstraction
  """
  @spec message_label(any()) :: {:ok, String.t()} | :continue
  def message_label(message) do
    case message do
      {:"$gen_call", _ref, {:router_call, _, term}} ->
        {:ok, "CALL: #{label_from_value(term)}"}

      # Custom logs for logger
      {:"$gen_cast", {:router_cast, _, {:add, _, level, _}}} ->
        {:ok, "ADD LEVEL: #{label_from_value(level)}"}

      {:"$gen_cast", {:router_cast, _, term}} ->
        {:ok, "CAST: #{label_from_value(term)}"}

      _ ->
        :continue
    end
  end

  # taken from the source code itself
  defp label_from_value(tuple)
       when is_tuple(tuple) and is_atom(elem(tuple, 0)),
       do: elem(tuple, 0)

  defp label_from_value(atom) when is_atom(atom), do: atom
  defp label_from_value(ref) when is_reference(ref), do: inspect(ref)
  defp label_from_value(tuple) when is_tuple(tuple), do: "tuple"
  defp label_from_value(_), do: "term"

  @doc """
  I am the function definition macro for debugging.

  I mainly should be used on functions which satisfy:

  1) Needing to be private i.e. not expose API to other Engines.
  2) Important for core functionality and hence requiring documentation.

  Given the two are satisfied, use me to define a function which becomes
  public once in debug mode and otherwise remains private.

  I am to be used alongside the `docp/1` macro to allow for good
  documentation practices.
  """

  defmacro defbug(name, expr \\ nil) do
    if Mix.env() == :debug do
      quote do
        def(unquote(name), unquote(expr))
      end
    else
      quote do
        defp(unquote(name), unquote(expr))
      end
    end
  end

  @doc """
  I am a macro which allows to provide documentation to possibly private
  functions.

  If the environment of the application is `:debug` I actually put the docs
  into the compiled module. If not, I produce `nil`, which does not
  interact with overall module environment.
  """

  defmacro docp(string) do
    if Mix.env() == :debug do
      quote do
        @doc unquote(string)
      end
    end
  end
end
