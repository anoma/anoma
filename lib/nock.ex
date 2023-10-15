defmodule Nock do
  @moduledoc """
  Nock, a universal function on nouns.
  """

  @dialyzer :no_improper_lists

  # temporary stub functions for jet scaffolding
  def get_jet(battery_mug) do
    # simulating Map fetch/2 not finding the key
    :error
  end

  def put_jet(_battery_mug, _jet_info) do
    nil
  end

  # top-level nock 4k interpreter.

  # nock 9: check if the core's battery has a jet registration first
  def nock(subject, [9, axis | core_formula]) do
    {:ok, core} = nock(subject, core_formula)

    maybe_battery_mug =
      try do
        {:ok, Noun.mug(hd(core))}
      rescue
        _ in ArgumentError -> :error
      end

    case maybe_battery_mug do
      {:ok, battery_mug} ->
        maybe_jet = get_jet(battery_mug)

        case maybe_jet do
          # there's no jet. just use naive nock
          :error ->
            nock(core, [2 | [[0 | 1] | [0 | axis]]])

          # a jet exists. mug the parent too
          {_label, parent_axis, parent_mug, jet_function} ->
            maybe_parent = Noun.axis(parent_axis, core)

            case maybe_parent do
              {:ok, parent} ->
                try do
                  # IO.inspect(Noun.mug(parent), label: "mugged parent")
                  # elixir syntax for normal erlang =
                  ^parent_mug = Noun.mug(parent)
                  # it's all there. use the jet.
                  # note: this is vastly simplified from a full jetting
                  # implementation. in particular, we are only jetting
                  # gates; we ignore which formula 9 used, assuming
                  # that it's at axis 2.
                  jet_function.(core)
                rescue
                  # parent mug didn't match. can't use the jet
                  _ in MatchError -> nock(core, [2 | [[0 | 1] | [0 | axis]]])
                end

              # the parent didn't even exist, the jet is bogus
              :error ->
                nock(core, [2 | [[0 | 1] | [0 | axis]]])
            end
        end

      :error ->
        # an atom is not a valid formula, this can only crash.
        # however, i don't want to introduce elisions of the spec yet
        nock(core, [2 | [[0 | 1] | [0 | axis]]])
    end
  end

  # generic case: use naive nock to reduce once.
  def nock(subject, formula) do
    naive_nock(subject, formula)
  end

  # naive nock interpreter: reduce via the nock 4k spec rules.
  # note: this must recurse into nock/2, not itself.
  def naive_nock(subject, formula) do
    try do
      case formula do
        # autocons; a cell of formulas becomes a cell of results
        # *[a [b c] d]        [*[a b c] *[a d]]
        [formula_1 = [_ | _] | formula_2] ->
          {:ok, result_1} = nock(subject, formula_1)
          {:ok, result_2} = nock(subject, formula_2)
          {:ok, [result_1 | result_2]}

        # 0: read from subject
        # *[a 0 b]            /[b a]
        [0 | axis] ->
          Noun.axis(axis, subject)

        # 1: constant
        # *[a 1 b]            b
        [1 | constant] ->
          {:ok, constant}

        # 2: eval
        # *[a 2 b c]          *[*[a b] *[a c]]
        [2, subject_formula | formula_formula] ->
          {:ok, new_subject} = nock(subject, subject_formula)
          {:ok, new_formula} = nock(subject, formula_formula)
          nock(new_subject, new_formula)

        # 3: cell test
        # *[a 3 b]            ?*[a b]
        [3 | sub_formula] ->
          {:ok, sub_result} = nock(subject, sub_formula)
          # yes, this is the best guard i can find
          if is_list(sub_result) do
            {:ok, 0}
          else
            {:ok, 1}
          end

        # 4: increment
        # *[a 4 b]            +*[a b]
        [4 | sub_formula] ->
          {:ok, sub_result} = nock(subject, sub_formula)

          if is_integer(sub_result) do
            {:ok, sub_result + 1}
          else
            :error
          end

        # 5: noun equality
        # *[a 5 b c]          =[*[a b] *[a c]]
        [5, formula_1 | formula_2] ->
          {:ok, result_1} = nock(subject, formula_1)
          {:ok, result_2} = nock(subject, formula_2)

          if result_1 == result_2 do
            {:ok, 0}
          else
            {:ok, 1}
          end

        # 6: if-then-else (spec macro)
        # *[a 6 b c d]        *[a *[[c d] 0 *[[2 3] 0 *[a 4 4 b]]]]
        [6, cond | branches = [_true_branch | _false_branch]] ->
          {:ok, cond_plus_two} = nock(subject, [4 | [4 | cond]])
          {:ok, crash_guard} = nock([2 | 3], [0 | cond_plus_two])
          {:ok, branch_formula} = nock(branches, [0 | crash_guard])
          nock(subject, branch_formula)

        # 7: with subject (spec macro)
        # *[a 7 b c]          *[*[a b] c]
        [7, subject_formula | sub_formula] ->
          {:ok, new_subject} = nock(subject, subject_formula)
          nock(new_subject, sub_formula)

        # 8: push on subject (spec macro)
        # *[a 8 b c]          *[[*[a b] a] c]
        [8, push_formula | sub_formula] ->
          {:ok, pushed_noun} = nock(subject, push_formula)
          new_subject = [pushed_noun | subject]
          nock(new_subject, sub_formula)

        # 9: arm of core (spec macro)
        # *[a 9 b c]          *[*[a c] 2 [0 1] 0 b]
        [9, axis | sub_formula] ->
          {:ok, sub_result} = nock(subject, sub_formula)
          nock(sub_result, [2 | [[0 | 1] | [0 | axis]]])

        # 10: replace at axis
        # *[a 10 [b c] d]     #[b *[a c] *[a d]]
        [10, [axis | replacement_formula] | sub_formula] ->
          {:ok, replacement} = nock(subject, replacement_formula)
          {:ok, sub_result} = nock(subject, sub_formula)
          Noun.replace(axis, replacement, sub_result)

        # 11: hint (spec macro)
        # *[a 11 [b c] d]     *[[*[a c] *[a d]] 0 3]
        [11, [_hint_noun | hint_formula] | sub_formula] ->
          # must be computed, but is discarded
          {:ok, hint_result} = nock(subject, hint_formula)
          {:ok, real_result} = nock(subject, sub_formula)
          nock([hint_result | real_result], [0 | 3])

        # *[a 11 b c]         *[a c]
        [11, _hint_noun | sub_formula] ->
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
    [2, subject_formula | formula_formula]
  end

  def nock_3(sub_formula) do
    [3 | sub_formula]
  end

  def nock_4(sub_formula) do
    [4 | sub_formula]
  end

  def nock_5(formula_1, formula_2) do
    [5, formula_1 | formula_2]
  end

  def nock_6(cond, true_branch, false_branch) do
    [6, cond, true_branch | false_branch]
  end

  def nock_7(subject_formula, sub_formula) do
    [7, subject_formula | sub_formula]
  end

  def nock_8(push_formula, sub_formula) do
    [8, push_formula | sub_formula]
  end

  def nock_9(axis, sub_formula) do
    [9, axis | sub_formula]
  end

  def nock_10(axis, replacement_formula, sub_formula) do
    [10, [axis | replacement_formula] | sub_formula]
  end

  def nock_11(hint_noun, hint_formula, sub_formula) do
    [11, [hint_noun | hint_formula] | sub_formula]
  end

  def nock_11(hint_noun, sub_formula) do
    [11, hint_noun | sub_formula]
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
    [decrement_arm(), sample | context]
  end

  # compiled output from hoon/anoma.hoon
  stdlib_string = """
      [ [ [ 7
            [ 8
              [1 1 1]
              [ 1
                8
                [1 0]
                8
                [ 1
                  6
                  [5 [1 0] 0 60]
                  [0 6]
                  9
                  2
                  10
                  [60 8 [9 342 0 31] 9 2 10 [6 0 124] 0 2]
                  10
                  [6 8 [9 20 0 31] 9 2 10 [6 [0 125] 0 14] 0 2]
                  0
                  1
                ]
                9
                2
                0
                1
              ]
              0
              1
            ]
            11
            [1.953.718.630 1 7.107.949 [0 7] 0]
            0
            1
          ]
          [ [ 7
              [ 8
                [1 0 0]
                [ 1
                  6
                  [5 [1 0] 0 12]
                  [0 13]
                  9
                  2
                  10
                  [6 [8 [9 342 0 7] 9 2 10 [6 0 28] 0 2] 4 0 13]
                  0
                  1
                ]
                0
                1
              ]
              11
              [1.953.718.630 1 6.579.297 [0 7] 0]
              0
              1
            ]
            [ [ 7
                [ 8
                  [1 0 0]
                  [ 1
                    6
                    [5 [0 12] 0 13]
                    [1 0]
                    6
                    [8 [9 343 0 7] 9 2 10 [6 [0 28] 0 29] 0 2]
                    [1 0]
                    1
                    1
                  ]
                  0
                  1
                ]
                11
                [1.953.718.630 1 6.648.940 [0 7] 0]
                0
                1
              ]
              [ 7
                [ 8
                  [1 1 1]
                  [ 1
                    6
                    [5 [1 0] 0 13]
                    [0 0]
                    8
                    [1 0]
                    8
                    [ 1
                      6
                      [8 [9 343 0 31] 9 2 10 [6 [0 124] 0 125] 0 2]
                      [0 6]
                      9
                      2
                      10
                      [60 8 [9 47 0 31] 9 2 10 [6 [0 124] 0 125] 0 2]
                      10
                      [6 4 0 6]
                      0
                      1
                    ]
                    9
                    2
                    0
                    1
                  ]
                  0
                  1
                ]
                11
                [1.953.718.630 1 7.760.228 [0 7] 0]
                0
                1
              ]
              [ 7
                [ 8
                  [1 0]
                  [ 1
                    6
                    [5 [1 0] 0 6]
                    [0 0]
                    8
                    [1 0]
                    8
                    [1 6 [5 [0 30] 4 0 6] [0 6] 9 2 10 [6 4 0 6] 0 1]
                    9
                    2
                    0
                    1
                  ]
                  0
                  1
                ]
                11
                [1.953.718.630 1 6.514.020 [0 7] 0]
                0
                1
              ]
              7
              [ 8
                [1 0 0]
                [ 1
                  6
                  [6 [5 [0 12] 0 13] [1 1] 1 0]
                  [ 6
                    [ 8
                      [ 1
                        6
                        [5 [1 0] 0 28]
                        [1 0]
                        6
                        [ 6
                          [6 [5 [1 0] 0 29] [1 1] 1 0]
                          [ 6
                            [ 9
                              2
                              10
                              [ 14
                                [8 [9 342 0 15] 9 2 10 [6 0 60] 0 2]
                                8
                                [9 342 0 15]
                                9
                                2
                                10
                                [6 0 61]
                                0
                                2
                              ]
                              0
                              1
                            ]
                            [1 0]
                            1
                            1
                          ]
                          1
                          1
                        ]
                        [1 0]
                        1
                        1
                      ]
                      9
                      2
                      0
                      1
                    ]
                    [1 0]
                    1
                    1
                  ]
                  1
                  1
                ]
                0
                1
              ]
              11
              [1.953.718.630 1 6.845.548 [0 7] 0]
              0
              1
            ]
            7
            [8 [1 0 0] [1 6 [8 [9 84 0 7] 9 2 10 [6 [0 28] 0 29] 0 2] [1 1] 1 0] 0 1]
            11
            [1.953.718.630 1 6.845.543 [0 7] 0]
            0
            1
          ]
          [ 7
            [ 8
              [1 0 0]
              [1 6 [8 [9 343 0 7] 9 2 10 [6 [0 28] 0 29] 0 2] [1 1] 1 0]
              0
              1
            ]
            11
            [1.953.718.630 1 6.648.935 [0 7] 0]
            0
            1
          ]
          [ 7
            [ 8
              [1 1 1]
              [ 1
                6
                [5 [1 0] 0 13]
                [0 0]
                8
                [9 47 0 7]
                9
                2
                10
                [ 6
                  [0 28]
                  7
                  [0 3]
                  8
                  [9 4 0 7]
                  9
                  2
                  10
                  [6 [0 29] 7 [0 3] 8 [9 170 0 7] 9 2 10 [6 [0 28] 0 29] 0 2]
                  0
                  2
                ]
                0
                2
              ]
              0
              1
            ]
            11
            [1.953.718.630 1 6.582.125 [0 7] 0]
            0
            1
          ]
          7
          [ 8
            [1 0 0]
            [ 1
              6
              [5 [1 0] 0 13]
              [0 12]
              9
              2
              10
              [ 6
                [8 [9 342 0 7] 9 2 10 [6 0 28] 0 2]
                8
                [9 342 0 7]
                9
                2
                10
                [6 0 29]
                0
                2
              ]
              0
              1
            ]
            0
            1
          ]
          11
          [1.953.718.630 1 6.452.595 [0 7] 0]
          0
          1
        ]
        [0 3]
        909
      ]
  """

  # evaluated at compile time.
  @stdlib_core_val Noun.Format.parse_always(stdlib_string)

  def stdlib_core do
    @stdlib_core_val
  end
end
