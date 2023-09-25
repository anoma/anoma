defmodule Nock do
  @moduledoc """
  Nock, a universal function on nouns.
  """

  @dialyzer :no_improper_lists

  def nock(subject, formula) do
    try do
      case formula do
        # autocons; a cell of formulas becomes a cell of results
        [formula_1 = [_ | _] | formula_2] ->
          {:ok, result_1} = nock(subject, formula_1)
          {:ok, result_2} = nock(subject, formula_2)
          {:ok, [result_1 | result_2]}

        # 0: read from subject
        [0 | axis] ->
          Noun.axis(axis, subject)

        # 1: constant
        [1 | constant] ->
          {:ok, constant}

        # 2: eval
        [2 | [subject_formula | formula_formula]] ->
          {:ok, new_subject} = nock(subject, subject_formula)
          {:ok, new_formula} = nock(subject, formula_formula)
          nock(new_subject, new_formula)

        # 3: cell test
        [3 | sub_formula] ->
          {:ok, sub_result} = nock(subject, sub_formula)
          # yes, this is the best guard i can find
          if is_list(sub_result) do
            {:ok, 0}
          else
            {:ok, 1}
          end

        # 4: increment
        [4 | sub_formula] ->
          {:ok, sub_result} = nock(subject, sub_formula)

          if is_integer(sub_result) do
            {:ok, sub_result + 1}
          else
            :error
          end

        # 5: noun equality
        [5 | [formula_1 | formula_2]] ->
          {:ok, result_1} = nock(subject, formula_1)
          {:ok, result_2} = nock(subject, formula_2)

          if result_1 == result_2 do
            {:ok, 0}
          else
            {:ok, 1}
          end

        # 6: if-then-else (spec macro)
        [6 | [cond | branches = [_true_branch | _false_branch]]] ->
          {:ok, cond_plus_two} = nock(subject, [4 | [4 | cond]])
          {:ok, crash_guard} = nock([2 | 3], [0 | cond_plus_two])
          {:ok, branch_formula} = nock(branches, [0 | crash_guard])
          nock(subject, branch_formula)

        # 7: with subject (spec macro)
        [7 | [subject_formula | sub_formula]] ->
          {:ok, new_subject} = nock(subject, subject_formula)
          nock(new_subject, sub_formula)

        # 8: push on subject (spec macro)
        [8 | [push_formula | sub_formula]] ->
          {:ok, pushed_noun} = nock(subject, push_formula)
          new_subject = [pushed_noun | subject]
          nock(new_subject, sub_formula)

        # 9: arm of core (spec macro)
        [9 | [axis | sub_formula]] ->
          {:ok, sub_result} = nock(subject, sub_formula)
          nock(sub_result, [2 | [[0 | 1] | [0 | axis]]])

        # 10: replace at axis
        [10 | [[axis | replacement_formula] | sub_formula]] ->
          {:ok, replacement} = nock(subject, replacement_formula)
          {:ok, sub_result} = nock(subject, sub_formula)
          Noun.replace(axis, replacement, sub_result)

        # 11: hint
        [11 | [[_hint_name | hint_formula] | sub_formula]] ->
          # must be computed, but is discarded
          {:ok, _hint_result} = nock(subject, hint_formula)
          nock(subject, sub_formula)

        [11 | [_hint_name | sub_formula]] ->
          nock(subject, sub_formula)

        # else, error
        _ ->
          :error
      end
    rescue
      _ in MatchError -> :error
    end
  end

  def cons(a, b) do
    [a | b]
  end

  def nock_0(axis) do
    [0 | axis]
  end

  def nock_1(constant) do
    [1 | constant]
  end

  def nock_2(subject_formula, formula_formula) do
    [2 | [subject_formula | formula_formula]]
  end

  def nock_3(sub_formula) do
    [3 | sub_formula]
  end

  def nock_4(sub_formula) do
    [4 | sub_formula]
  end

  def nock_5(formula_1, formula_2) do
    [5 | [formula_1 | formula_2]]
  end

  def nock_6(cond, true_branch, false_branch) do
    [6 | [cond | [true_branch | false_branch]]]
  end

  def nock_7(subject_formula, sub_formula) do
    [7 | [subject_formula | sub_formula]]
  end

  def nock_8(push_formula, sub_formula) do
    [8 | [push_formula | sub_formula]]
  end

  def nock_9(axis, sub_formula) do
    [9 | [axis | sub_formula]]
  end

  def nock_10(axis, replacement_formula, sub_formula) do
    [10 | [[axis | replacement_formula] | sub_formula]]
  end

  def nock_11(hint_name, hint_formula, sub_formula) do
    [11 | [[hint_name | hint_formula] | sub_formula]]
  end

  def nock_11(hint_name, sub_formula) do
    [11 | [hint_name | sub_formula]]
  end

  def decrement_arm do
    nock_8(
      nock_1(0),
      nock_8(
        nock_1(
          nock_6(
            nock_5(
              nock_0(30),
              nock_4(nock_0(6))
            ),
            nock_0(6),
            nock_9(
              2,
              nock_10(
                6,
                nock_4(nock_0(6)),
                nock_0(1)
              )
            )
          )
        ),
        nock_9(2, nock_0(1))
      )
    )
  end

  def decrement_core do
    context = 0
    sample = 123
    [decrement_arm() | [sample | context]]
  end
end
