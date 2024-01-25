defmodule Anoma.Node.Solver.Solver do
  @moduledoc """
  I am a strawman intent solver for testing purposes.
  """

  use GenServer
  import Bitwise

  def start_link(arg) do
    GenServer.start_link(__MODULE__, arg, Anoma.Node.Utility.name(arg))
  end

  def init(_init) do
    {:ok, {[], []}}
  end

  @spec add_intent(GenServer.server(), Anoma.Transaction) ::
          list(Anoma.Transaction)
  def add_intent(server, intent) do
    GenServer.call(server, {:add_intent, intent})
  end

  @spec del_intents(GenServer.server(), Anoma.Transaction) ::
          list(Anoma.Transaction)
  def del_intents(server, intents) do
    GenServer.call(server, {:del_intents, intents})
  end

  @spec get_solved(GenServer.server()) :: list(Anoma.Transaction)
  def get_solved(server) do
    GenServer.call(server, :get_solved)
  end

  # unsolved is a list of transactions
  # solved is a list of [balanced_tx | unbalanced_constituents]
  # we keep around the solved transactions because there's no guarantee that our solution is actually chosen; for example, if we present [xy | [x, y]] as a solution, and a different solution is chosen which solves x (and therefore removes it from the intent pool) but leaves y around, then we should return y to unsolved

  # todo should use a better comparator and less quadratic for the things that don't have to be quadratic

  defp handle_solve({unsolved, solved}) do
    {new_unsolved, new_solved} = solve(unsolved)

    {:reply, Enum.map(new_solved, &hd/1),
     {new_unsolved, new_solved ++ solved}}
  end

  def handle_call({:add_intent, intent}, _from, {unsolved, solved}) do
    # Enum.each(balanced, fn tx -> AnomaInterface.Validator.Stub.propose_transaction(validator_conn, RPC.Convert.to_transaction(tx)) end)
    handle_solve({[intent | unsolved], solved})
  end

  def handle_call({:del_intents, deleted}, _from, {unsolved, solved}) do
    unsolved = Enum.filter(unsolved, fn x -> x in deleted end)

    {nolonger_solved, solved} =
      Enum.split_with(solved, fn x ->
        Enum.any?(tl(x), fn x -> x in deleted end)
      end)

    nolonger_solved =
      nolonger_solved
      |> Enum.map(&tl/1)
      |> Enum.concat()
      |> Enum.filter(fn x -> x in deleted end)

    handle_solve({unsolved ++ nolonger_solved, solved})
  end

  def handle_call(:get_solved, _from, {unsolved, solved}) do
    {:reply, Enum.map(solved, &hd/1), {unsolved, solved}}
  end

  # powerset enumeration with binary numbers, because I am lazy
  # unset bits in n indicate which elements to select
  # we start at (1 <<< length(unbalanced) - 2)
  # -1 would be all set bits, meaning we select nothing, which is obviously useless, so we start at -2
  # counting down instead of counting up simplifies the termination condition; then, letting unset bits--rather than set bits--indicate elements to select means we consider small subsets rather than large ones initially
  # it's still suboptimal though; we should first consider all subsets of size 1, then 2, 3, etc.
  # this way, we could still produce {{x, y, z, w}} when we only really needed {{x, y}, {z, w}}
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
          x && y && Anoma.Resource.Transaction.compose(x, y)
        end)

      if composed_selected &&
           Anoma.Resource.Transaction.verify(composed_selected) do
        # got a matc
        solve(unselected, [[composed_selected | selected] | balanced])
      else
        # no dice; continue to next subset
        solve(unbalanced, balanced, n - 1)
      end
    end
  end
end
