defmodule Nock do
  @moduledoc """
  Nock, a universal function on nouns.
  """

  require Noun

  @dialyzer :no_improper_lists

  @layer_1_context_mug 17_654_928_022_549_292_273

  # hardcoded jet registry
  @jet_registry %{
    6_149_546_978_850_415_302 =>
      {"dec", 7, @layer_1_context_mug, &Nock.Jets.dec/1},
    13_456_140_584_432_748_094 =>
      {"add", 7, @layer_1_context_mug, &Nock.Jets.add/1},
    14_801_825_384_048_474_882 =>
      {"sub", 7, @layer_1_context_mug, &Nock.Jets.sub/1},
    16_448_669_966_680_839_947 =>
      {"lth", 7, @layer_1_context_mug, &Nock.Jets.lth/1},
    9_593_574_083_496_825_465 =>
      {"lte", 7, @layer_1_context_mug, &Nock.Jets.lte/1},
    1_289_252_054_839_247_911 =>
      {"gth", 7, @layer_1_context_mug, &Nock.Jets.gth/1},
    11_860_242_327_310_494_912 =>
      {"gte", 7, @layer_1_context_mug, &Nock.Jets.gte/1},
    1_648_022_014_956_696_243 =>
      {"mul", 7, @layer_1_context_mug, &Nock.Jets.mul/1},
    9_385_224_608_484_213_408 =>
      {"div", 7, @layer_1_context_mug, &Nock.Jets.div/1},
    9_865_107_020_110_751_778 =>
      {"mod", 7, @layer_1_context_mug, &Nock.Jets.mod/1}
  }

  # temporary stub functions for jet scaffolding
  def get_jet(battery_mug) do
    Map.fetch(@jet_registry, battery_mug)
  end

  def put_jet(_battery_mug, _jet_info) do
    nil
  end

  # top-level nock 4k interpreter.

  # direct calls should be jetted
  @spec nock(Noun.t(), Noun.t()) :: {:ok, Noun.t()} | :error
  def nock(subject, formula) do
    nock(subject, formula, :jetted)
  end

  # nock 9: check if the core's battery has a jet registration first
  def nock(subject, [9, axis | core_formula], :jetted) do
    {:ok, core} = nock(subject, core_formula)

    maybe_battery_mug =
      try do
        {:ok, Noun.mug(hd(core))}
      rescue
        _ in ArgumentError -> :error
      end

    case maybe_battery_mug do
      {:ok, battery_mug} ->
        # IO.inspect(battery_mug, label: "mugged battery")
        maybe_jet = get_jet(battery_mug)

        case maybe_jet do
          # there's no jet. just use naive nock
          :error ->
            nock(core, [2 | [[0 | 1] | [0 | axis]]])

          # a jet exists. mug the parent too
          {:ok, {_label, parent_axis, parent_mug, jet_function}} ->
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

  # skip a jet once (must be called with a 9 formula for this to work)
  def nock(subject, formula, :unjetted_once) do
    naive_nock(subject, formula, :jetted)
  end

  # nock with jet-instrumentation mode.
  def nock(subject, formula, instrumentation)
      when instrumentation == :instrument or
             instrumentation == :instrument_once do
    test_jettedness =
      case instrumentation do
        :instrument -> :unjetted
        :instrument_once -> :unjetted_once
      end

    {jetted_usecs, jetted_result} =
      :timer.tc(fn -> nock(subject, formula, :jetted) end)

    {unjetted_usecs, unjetted_result} =
      :timer.tc(fn -> nock(subject, formula, test_jettedness) end)

    validity = jetted_result === unjetted_result

    %{
      jetted_usecs: jetted_usecs,
      unjetted_usecs: unjetted_usecs,
      result: unjetted_result,
      valid:
        if validity do
          validity
        else
          {validity, jetted_result}
        end
    }
  end

  # generic case: use naive nock to reduce once.
  @spec nock(Noun.t(), Noun.t(), :jetted | :unjetted_once | :unjetted) ::
          {:ok, Noun.t()} | :error
  def nock(subject, formula, jettedness) do
    naive_nock(subject, formula, jettedness)
  end

  # naive nock interpreter: reduce via the nock 4k spec rules.
  # note: this must recurse into nock/2 (or nock/3), not itself.

  # direct calls of naive_nock should be unjetted
  @spec naive_nock(Noun.t(), Noun.t()) :: {:ok, Noun.t()} | :error
  def naive_nock(subject, formula) do
    naive_nock(subject, formula, :unjetted)
  end

  @spec naive_nock(Noun.t(), Noun.t(), :jetted | :unjetted_once | :unjetted) ::
          {:ok, Noun.t()} | :error
  def naive_nock(subject, formula, jettedness) do
    try do
      case formula do
        # autocons; a cell of formulas becomes a cell of results
        # *[a [b c] d]        [*[a b c] *[a d]]
        [formula_1 = [_ | _] | formula_2] ->
          {:ok, result_1} = nock(subject, formula_1, jettedness)
          {:ok, result_2} = nock(subject, formula_2, jettedness)
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
          {:ok, new_subject} = nock(subject, subject_formula, jettedness)
          {:ok, new_formula} = nock(subject, formula_formula, jettedness)
          nock(new_subject, new_formula, jettedness)

        # 3: cell test
        # *[a 3 b]            ?*[a b]
        [3 | sub_formula] ->
          {:ok, sub_result} = nock(subject, sub_formula, jettedness)

          if Noun.is_noun_cell(sub_result) do
            {:ok, 0}
          else
            {:ok, 1}
          end

        # 4: increment
        # *[a 4 b]            +*[a b]
        [4 | sub_formula] ->
          {:ok, sub_result} = nock(subject, sub_formula, jettedness)

          if is_integer(sub_result) do
            {:ok, sub_result + 1}
          else
            :error
          end

        # 5: noun equality
        # *[a 5 b c]          =[*[a b] *[a c]]
        [5, formula_1 | formula_2] ->
          {:ok, result_1} = nock(subject, formula_1, jettedness)
          {:ok, result_2} = nock(subject, formula_2, jettedness)

          if result_1 == result_2 do
            {:ok, 0}
          else
            {:ok, 1}
          end

        # 6: if-then-else (spec macro)
        # *[a 6 b c d]        *[a *[[c d] 0 *[[2 3] 0 *[a 4 4 b]]]]
        [6, cond | branches = [_true_branch | _false_branch]] ->
          {:ok, cond_plus_two} = nock(subject, [4 | [4 | cond]], jettedness)
          {:ok, crash_guard} = nock([2 | 3], [0 | cond_plus_two], jettedness)

          {:ok, branch_formula} =
            nock(branches, [0 | crash_guard], jettedness)

          nock(subject, branch_formula, jettedness)

        # 7: with subject (spec macro)
        # *[a 7 b c]          *[*[a b] c]
        [7, subject_formula | sub_formula] ->
          {:ok, new_subject} = nock(subject, subject_formula, jettedness)
          nock(new_subject, sub_formula, jettedness)

        # 8: push on subject (spec macro)
        # *[a 8 b c]          *[[*[a b] a] c]
        [8, push_formula | sub_formula] ->
          {:ok, pushed_noun} = nock(subject, push_formula, jettedness)
          new_subject = [pushed_noun | subject]
          nock(new_subject, sub_formula, jettedness)

        # 9: arm of core (spec macro)
        # *[a 9 b c]          *[*[a c] 2 [0 1] 0 b]
        [9, axis | sub_formula] ->
          {:ok, sub_result} = nock(subject, sub_formula, jettedness)
          nock(sub_result, [2 | [[0 | 1] | [0 | axis]]], jettedness)

        # 10: replace at axis
        # *[a 10 [b c] d]     #[b *[a c] *[a d]]
        [10, [axis | replacement_formula] | sub_formula] ->
          {:ok, replacement} = nock(subject, replacement_formula, jettedness)
          {:ok, sub_result} = nock(subject, sub_formula, jettedness)
          Noun.replace(axis, replacement, sub_result)

        # 11: hint (spec macro)
        # *[a 11 [b c] d]     *[[*[a c] *[a d]] 0 3]
        [11, [_hint_noun | hint_formula] | sub_formula] ->
          # must be computed, but is discarded
          {:ok, hint_result} = nock(subject, hint_formula, jettedness)
          {:ok, real_result} = nock(subject, sub_formula, jettedness)
          nock([hint_result | real_result], [0 | 3], jettedness)

        # *[a 11 b c]         *[a c]
        [11, _hint_noun | sub_formula] ->
          nock(subject, sub_formula, jettedness)

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

  # compiled and evaluated output from hoon/anoma.hoon.
  # eventually, will be replaced with merely compiled output, with
  # evaluation to take place during nock vm bootstrapping.
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

  @spec stdlib_core :: Noun.t()
  def stdlib_core do
    @stdlib_core_val
  end
end
