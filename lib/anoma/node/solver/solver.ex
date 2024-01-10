defmodule Anoma.Node.Solver.Solver do
  @moduledoc """
  I am a strawman intent solver for testing purposes.
  """

  use GenServer
  import Bitwise
  alias Anoma.Resource.Transaction

  def start_link(arg) do
    GenServer.start_link(__MODULE__, arg, Anoma.Node.Utility.name(arg))
  end

  def init(_init) do
    {:ok, {[], []}}
  end

  @spec add_intent(GenServer.server(), Transaction.t()) ::
          list(Transaction.t())
  def add_intent(server, intent) do
    GenServer.call(server, {:add_intent, intent})
  end

  @spec del_intents(GenServer.server(), Transaction.t()) ::
          list(Transaction.t())
  def del_intents(server, intents) do
    GenServer.call(server, {:del_intents, intents})
  end

  @spec get_solved(GenServer.server()) :: list(Transaction.t())
  def get_solved(server) do
    GenServer.call(server, :get_solved)
  end

  # unsolved is a list of transactions solved is a list of
  # [balanced_tx | unbalanced_constituents] we keep around the solved
  # transactions because there's no guarantee that our solution is
  # actually chosen; for example, if we present [xy | [x, y]] as a
  # solution, and a different solution is chosen which solves x (and
  # therefore removes it from the intent pool) but leaves y around,
  # then we should return y to unsolved

  # todo should use a better comparator and less quadratic for the
  # things that don't have to be quadratic

  defp handle_solve({unsolved, solved}) do
    {new_unsolved, new_solved} = solve(unsolved)

    {:reply, Enum.map(new_solved, &hd/1),
     {new_unsolved, new_solved ++ solved}}
  end

  def handle_call({:add_intent, intent}, _from, {unsolved, solved}) do
    handle_solve({[intent | unsolved], solved})
  end

  def handle_call({:del_intents, deleted}, _from, {unsolved, solved}) do
    unsolved = Enum.filter(unsolved, fn x -> x in deleted end)

    {nolonger_solved, still_solved} =
      Enum.split_with(solved, fn x ->
        Enum.any?(tl(x), fn x -> x in deleted end)
      end)

    nolonger_solved =
      nolonger_solved
      |> Enum.map(&tl/1)
      |> Enum.concat()
      |> Enum.filter(fn x -> x not in deleted end)

    handle_solve({unsolved ++ nolonger_solved, still_solved})
  end

  def handle_call(:get_solved, _from, {unsolved, solved}) do
    {:reply, Enum.map(solved, &hd/1), {unsolved, solved}}
  end

  # powerset enumeration with binary numbers, because I am lazy
  # unset bits in n indicate which elements to select
  # we start at (1 <<< length(unbalanced) - 2)
  # -1 would be all set bits, meaning we select nothing, which is
  # obviously useless, so we start at -2 counting down instead of
  # counting up simplifies the termination condition; then, letting
  # unset bits--rather than set bits--indicate elements to select
  # means we consider small subsets rather than large ones initially
  # (and, in particular, for any x⊂y, we always consider x before y)
  def solve(unbalanced) do
    solve(unbalanced, [])
  end

  defp solve(unbalanced, balanced) do
    solve(unbalanced, balanced, (1 <<< length(unbalanced)) - 2)
  end

  defp solve(unbalanced, balanced, n) do
    if n < 0 do
      {unbalanced, balanced}
    else
      {_, selected, unselected} =
        Enum.reduce(unbalanced, {1, [], []}, fn el,
                                                {i, selected, unselected} ->
          if (i &&& n) == 0 do
            {i <<< 1, [el | selected], unselected}
          else
            {i <<< 1, selected, [el | unselected]}
          end
        end)

      composed_selected =
        Enum.reduce(selected, fn x, y ->
          x && y && Transaction.compose(x, y)
        end)

      if composed_selected &&
           Transaction.verify(composed_selected) do
        # got a match
        solve(unselected, [[composed_selected | selected] | balanced])
      else
        # no dice; continue to next subset
        solve(unbalanced, balanced, n - 1)
      end
    end
  end
end
