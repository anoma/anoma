defmodule Anoma.Node.Utility do
  @moduledoc """

  I have utility functions that are common amongst all Node

  ### Argument Constructors

  I contain common arguments for constructing genserver arguments:

    - `name/1`


  ### Tracing Utilities

    - `message_label/1`

  ### Communicators

  I also contain functions that make creating Communicators easier

    - `name/2`
    - `com_name/1`
    - `broadcast/2`
  """

  @doc """

  Grabs the name out of the given `Keyword` argument list, and returns
  a keyword list only with name

  ### Parameters

    - `arg` - the keyword argument list
  """

  @spec name(Keyword.t()) :: Keyword.t()
  def name(arg) do
    name(arg, &Function.identity/1)
  end

  @doc """
  Helps labeling for `Kino.Process.seq_trace/2`, for the Router abstraction
  """
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

  ##############################################################################
  #                          Communicator Abstractions                         #
  ##############################################################################
  @doc """

  Grabs the name out of the given `Keyword` argument list, and applies
  a given function over the resulting name, returning an altered name.

  ### Parameters

    - `arg` - the keyword argument list
    - `name_alteration` - a function that alters the given name
  """
  @spec name(Keyword.t(), (atom() -> atom)) :: Keyword.t()
  def name(arg, name_alteration) do
    name = arg[:name]

    if name do
      [name: name_alteration.(name)]
    else
      []
    end
  end

  @doc """
  I create a communicator name from a given atom

  ### Parameters

  - `name` (atom) - the original name

  ## Returns

  The name with a communicator tag appended to it

  ## Examples

      iex> Anoma.Node.Utility.com_name(:joe)
      :joe_com
  """
  @spec com_name(atom()) :: atom()
  def com_name(name), do: (Atom.to_string(name) <> "_com") |> String.to_atom()

  @doc """
  I broadcast a given term to all subscribers

  ### Parameters
    - `subs` - the subscribers, we accept any enumerable term
    - `term` - the given term we wish to broadcast
  """
  @spec broadcast(Enum.t(), term()) :: :ok
  def broadcast(subs, term) do
    # this is bad we are assuming GenServer, lets make this generic by
    # passing in a module to do a cast or call or something.
    subs
    |> Enum.map(fn sub -> GenServer.cast(sub, term) end)

    :ok
  end

  @spec append_name(atom(), String.t()) :: atom()
  def append_name(name, appended) do
    (Atom.to_string(name) <> appended) |> String.to_atom()
  end
end
