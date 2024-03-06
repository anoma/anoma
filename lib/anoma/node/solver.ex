defmodule Anoma.Node.Solver do
  @moduledoc """
  I am a strawman intent solver for testing purposes.
  """

  use TypedStruct
  import Bitwise
  alias Anoma.Resource.Transaction
  alias Anoma.Node.{Router, Logger}
  alias __MODULE__

  typedstruct do
    field(:solutions_topic, Router.Addr.t())
    field(:unsolved, [Transaction.t()], default: [])
    field(:solved, [Transaction.t()], default: [])
    field(:solved_set, MapSet.t(Transaction.t()), default: MapSet.new())
    field(:logger, Router.Addr.t(), enforce: false)
  end

  def init({router, logger, intent_pool, intents, solutions}) do
    unsolved = Enum.to_list(Router.call(intent_pool, :intents))
    :ok = Router.call(router, {:subscribe_topic, intents, :local})

    {:ok,
     %Solver{
       logger: logger,
       solutions_topic: solutions,
       unsolved: unsolved
     }}
  end

  @spec get_solved(Router.Addr.t()) :: list(Transaction.t())
  def get_solved(solver) do
    Router.call(solver, :get_solved)
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

  defp handle_solve(s) do
    {new_unsolved, new_solved} = solve(s.unsolved)
    log_info({:solve, new_unsolved, new_solved, s.logger})

    if new_solved != [] do
      Router.cast(
        s.solutions_topic,
        {:solutions, Enum.map(new_solved, &hd/1)}
      )
    end

    solved_set =
      Enum.reduce(new_solved, s.solved_set, fn [_ | intents], set ->
        Enum.reduce(intents, set, fn intent, set ->
          MapSet.put(set, intent)
        end)
      end)

    {:noreply,
     %{
       s
       | unsolved: new_unsolved,
         solved: new_solved ++ s.solved,
         solved_set: solved_set
     }}
  end

  def handle_cast({:new_intent, intent}, _, s) do
    if intent not in s.unsolved && !MapSet.member?(s.solved_set, intent) do
      log_info({:add, intent, s.logger})
      handle_solve(%{s | unsolved: [intent | s.unsolved]})
    else
      {:noreply, s}
    end
  end

  def handle_cast({:remove_intent, deleted}, _, s) do
    unsolved = Enum.filter(s.unsolved, fn x -> x != deleted end)

    logger = s.logger
    log_info({:del, deleted, logger})

    {nolonger_solved, still_solved} =
      Enum.split_with(s.solved, fn x -> deleted in tl(x) end)

    nolonger_solved =
      nolonger_solved
      |> Enum.map(&tl/1)
      |> Enum.concat()
      |> Enum.filter(fn x -> x != deleted end)

    log_info({:del_solved, nolonger_solved, logger})

    solved_set = MapSet.delete(s.solved_set, deleted)

    handle_solve(%{
      s
      | unsolved: unsolved ++ nolonger_solved,
        solved: still_solved,
        solved_set: solved_set
    })
  end

  def handle_call(:get_solved, _from, s) do
    log_info({:get, s.solved, s.logger})
    {:reply, Enum.map(s.solved, &hd/1), s}
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

  ############################################################
  #                     Logging Info                         #
  ############################################################

  defp log_info({:solve, unsolved, solved, logger}) do
    Logger.add(
      logger,
      :info,
      "Solved. Unsolved: #{inspect(unsolved)}. Solved: #{inspect(solved)}."
    )
  end

  defp log_info({:add, intent, logger}) do
    Logger.add(
      logger,
      :info,
      "Request to add intent: #{inspect(intent)}."
    )
  end

  defp log_info({:del, intent, logger}) do
    Logger.add(
      logger,
      :debug,
      "Request to delete intent: #{inspect(intent)}."
    )
  end

  defp log_info({:del_solved, nolonger_solved, logger}) do
    Logger.add(logger, :debug, "After intent deletion,
    following transactions are no longer solved:
    #{inspect(nolonger_solved)}.")
  end

  defp log_info({:get, solved, logger}) do
    Logger.add(
      logger,
      :info,
      "Request to get solved: #{inspect(solved)}."
    )
  end
end
