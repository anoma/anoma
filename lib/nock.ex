defmodule Nock do
  @moduledoc """
  Nock, a universal function on nouns.
  """

  require Noun
  require Logger

  use TypedStruct

  alias Anoma.Node.Storage
  alias __MODULE__
  alias Anoma.Node.Ordering
  alias Anoma.Node.Router

  @typedoc """
  I contain environmental information on how Nock shall be evaluated.

  For example Ι contain information on jettedness to
  determine if we should be using jets or not

  """
  typedstruct do
    field(:ordering, Router.Addr.t() | nil, default: nil)
    field(:snapshot_path, Noun.t() | nil, default: nil)
    field(:logger, Router.Addr.t(), enforce: false)
    field(:meter_pid, pid() | nil, default: nil)
  end

  @dialyzer :no_improper_lists

  @layers 6

  @layer_1_context_mug 17_654_928_022_549_292_273
  @layer_4_context_mug 10_859_520_857_756_426_536
  @layer_5_context_mug 06_457_564_626_474_072_276
  @layer_6_context_mug 10_539_262_992_069_512_324
  @layer_4_block_context_mug 11_284_470_320_276_584_209

  # hardcoded jet registry
  # valid statuses:
  # - :enabled, jet is fully enabled
  # - :disabled, jet is fully disabled
  # - :check, check that jet and naive produce the same result
  @jet_registry %{
    6_149_546_978_850_415_302 =>
      {"dec", 7, @layer_1_context_mug, &Nock.Jets.dec/1, :enabled, 10},
    13_456_140_584_432_748_094 =>
      {"add", 7, @layer_1_context_mug, &Nock.Jets.add/1, :enabled, 10},
    14_801_825_384_048_474_882 =>
      {"sub", 7, @layer_1_context_mug, &Nock.Jets.sub/1, :enabled, 10},
    16_448_669_966_680_839_947 =>
      {"lth", 7, @layer_1_context_mug, &Nock.Jets.lth/1, :enabled, 10},
    9_593_574_083_496_825_465 =>
      {"lte", 7, @layer_1_context_mug, &Nock.Jets.lte/1, :enabled, 10},
    1_289_252_054_839_247_911 =>
      {"gth", 7, @layer_1_context_mug, &Nock.Jets.gth/1, :enabled, 10},
    11_860_242_327_310_494_912 =>
      {"gte", 7, @layer_1_context_mug, &Nock.Jets.gte/1, :enabled, 10},
    1_648_022_014_956_696_243 =>
      {"mul", 7, @layer_1_context_mug, &Nock.Jets.mul/1, :enabled, 10},
    9_385_224_608_484_213_408 =>
      {"div", 7, @layer_1_context_mug, &Nock.Jets.div/1, :enabled, 10},
    9_865_107_020_110_751_778 =>
      {"mod", 7, @layer_1_context_mug, &Nock.Jets.mod/1, :enabled, 10},
    2_171_028_045_513_367_023 =>
      {"verify", 7, @layer_6_context_mug, &Nock.Jets.verify/1, :enabled, 100},
    11_207_087_638_603_349_387 =>
      {"sign", 7, @layer_6_context_mug, &Nock.Jets.sign/1, :enabled, 100},
    14_969_209_197_206_704_945 =>
      {"verify-detatched", 7, @layer_6_context_mug,
       &Nock.Jets.verify_detatched/1, :enabled, 100},
    17_231_646_935_946_214_635 =>
      {"sign-detatched", 7, @layer_6_context_mug, &Nock.Jets.sign_detatched/1,
       :enabled, 100},
    18_035_336_398_045_859_425 =>
      {"bex", 7, @layer_4_context_mug, &Nock.Jets.bex/1, :enabled, 20},
    9_856_364_927_859_191_827 =>
      {"mix", 7, @layer_5_context_mug, &Nock.Jets.mix/1, :enabled, 20},
    15_935_656_757_697_674_383 =>
      {"jam", 7, @layer_5_context_mug, &Nock.Jets.jam/1, :enabled, 50},
    11_639_366_709_658_810_328 =>
      {"cue", 7, @layer_5_context_mug, &Nock.Jets.cue/1, :enabled, 50},
    3_043_895_017_490_773_097 =>
      {"met", 14, @layer_4_block_context_mug, &Nock.Jets.met/1, :enabled, 20},
    13_916_871_139_600_991_184 =>
      {"end", 14, @layer_4_block_context_mug, &Nock.Jets.nend/1, :enabled, 20},
    8_892_358_705_258_348_836 =>
      {"lsh", 14, @layer_4_block_context_mug, &Nock.Jets.lsh/1, :enabled, 20},
    12_605_872_635_346_981_159 =>
      {"rsh", 14, @layer_4_block_context_mug, &Nock.Jets.rsh/1, :enabled, 20}
  }

  @doc """
  Gives the total numbers of layers in the standard library
  """
  def stdlib_layers, do: @layers

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
    nock(subject, formula, %Nock{})
  end

  # nock 9: check if the core's battery has a jet registration first
  def nock(subject, [nine, axis | core_formula], env)
      when nine in [9, <<9>>] do
    {:ok, core} = nock(subject, core_formula, env)

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
            nock(core, [2 | [[0 | 1] | [0 | axis]]], env)

          # a jet exists but it's disabled, use naive nock
          {:ok,
           {_label, _parent_axis, _parent_mug, _jet_function, :disabled,
            _cost}} ->
            nock(core, [2 | [[0 | 1] | [0 | axis]]], env)

          # a jet exists. mug the parent too
          {:ok,
           {label, parent_axis, parent_mug, jet_function, jet_mode, cost}} ->
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
                  case jet_mode do
                    :enabled ->
                      if env.meter_pid != nil do
                        send(env.meter_pid, {:gas, cost})
                      end

                      jet_function.(core)

                    :check ->
                      {jet_usecs, jet_result} =
                        :timer.tc(fn -> jet_function.(core) end)

                      {naive_usecs, naive_result} =
                        :timer.tc(fn ->
                          nock(core, [2 | [[0 | 1] | [0 | axis]]], env)
                        end)

                      validity = jet_result === naive_result

                      check_result = %{
                        jet_usecs: jet_usecs,
                        naive_usecs: naive_usecs,
                        result: jet_result,
                        valid:
                          if validity do
                            validity
                          else
                            {validity, jet_result, naive_result}
                          end
                      }

                      Logger.info(
                        "jet #{label} check result: #{inspect(check_result)}"
                      )

                      if validity do
                        jet_result
                      else
                        :error
                      end
                  end
                rescue
                  # parent mug didn't match. can't use the jet
                  _ in MatchError ->
                    nock(core, [2 | [[0 | 1] | [0 | axis]]], env)
                end

              # the parent didn't even exist, the jet is bogus
              :error ->
                nock(core, [2 | [[0 | 1] | [0 | axis]]], env)
            end
        end

      :error ->
        # an atom is not a valid formula, this can only crash.
        # however, i don't want to introduce elisions of the spec yet
        nock(core, [2 | [[0 | 1] | [0 | axis]]], env)
    end
  end

  # scry: magically read from storage.
  def nock(subject, [twelve, type_formula | subformula], environemnt)
      when twelve in [12, <<12>>] do
    with {:ok, _type_result} <- nock(subject, type_formula, environemnt),
         {:ok, sub_result} <- nock(subject, subformula, environemnt) do
      read_with_id(sub_result, environemnt)
    else
      _ -> :error
    end
  end

  # generic case: use naive nock to reduce once.
  @spec nock(Noun.t(), Noun.t(), t()) :: {:ok, Noun.t()} | :error
  def nock(subject, formula, environment) do
    naive_nock(subject, formula, environment)
  end

  # metered nock: a hack until nock VMs become their own agents.
  @spec metered_nock(Noun.t(), Noun.t()) ::
          {:ok, Noun.t(), non_neg_integer()} | {:error, non_neg_integer()}
  def metered_nock(subject, formula) do
    metered_nock(subject, formula, %Nock{})
  end

  @spec metered_nock(Noun.t(), Noun.t(), t()) ::
          {:ok, Noun.t(), non_neg_integer()} | {:error, non_neg_integer()}
  def metered_nock(subject, formula, environment) do
    meter = Task.async(Nock, :gas_meter, [])
    result = nock(subject, formula, %{environment | meter_pid: meter.pid})
    send(meter.pid, :done)
    gas_cost = Task.await(meter)

    case result do
      {:ok, noun} ->
        {:ok, noun, gas_cost}

      :error ->
        {:error, gas_cost}
    end
  end

  @spec gas_meter() :: non_neg_integer()
  def gas_meter() do
    gas_meter(0)
  end

  @spec gas_meter(non_neg_integer()) :: non_neg_integer()
  def gas_meter(gas) do
    receive do
      {:gas, n} ->
        gas_meter(n + gas)

      :done ->
        gas
    end
  end

  # naive nock interpreter: reduce via the nock 4k spec rules.
  # note: this must recurse into nock/2 (or nock/3), not itself.

  # direct calls of naive_nock should be unjetted
  @spec naive_nock(Noun.t(), Noun.t()) :: {:ok, Noun.t()} | :error
  def naive_nock(subject, formula) do
    naive_nock(subject, formula, %Nock{})
  end

  @spec naive_nock(Noun.t(), Noun.t(), t()) :: {:ok, Noun.t()} | :error
  def naive_nock(subject, formula, environment) do
    if environment.meter_pid != nil do
      send(environment.meter_pid, {:gas, 1})
    end

    try do
      case formula do
        # autocons; a cell of formulas becomes a cell of results
        # *[a [b c] d]        [*[a b c] *[a d]]
        [formula_1 = [_ | _] | formula_2] ->
          {:ok, result_1} = nock(subject, formula_1, environment)
          {:ok, result_2} = nock(subject, formula_2, environment)
          {:ok, [result_1 | result_2]}

        # 0: read from subject
        # *[a 0 b]            /[b a]
        [zero | axis] when zero in [0, <<>>, []] and is_integer(axis) ->
          Noun.axis(axis, subject)

        [zero | axis] when zero in [0, <<>>, []] and is_binary(axis) ->
          Noun.axis(Noun.atom_binary_to_integer(axis), subject)

        # [0 0] is the canonical crash; so take a shortcut
        [zero | axis] when zero in [0, <<>>, []] and axis == [] ->
          :error

        # 1: constant
        # *[a 1 b]            b
        [one | constant] when one in [1, <<1>>] ->
          {:ok, constant}

        # 2: eval
        # *[a 2 b c]          *[*[a b] *[a c]]
        [two, subject_formula | formula_formula] when two in [2, <<2>>] ->
          {:ok, new_subject} = nock(subject, subject_formula, environment)
          {:ok, new_formula} = nock(subject, formula_formula, environment)
          nock(new_subject, new_formula, environment)

        # 3: cell test
        # *[a 3 b]            ?*[a b]
        [three | sub_formula] when three in [3, <<3>>] ->
          {:ok, sub_result} = nock(subject, sub_formula, environment)

          if Noun.is_noun_cell(sub_result) do
            {:ok, 0}
          else
            {:ok, 1}
          end

        # 4: increment
        # *[a 4 b]            +*[a b]
        [four | sub_formula] when four in [4, <<4>>] ->
          {:ok, sub_result} = nock(subject, sub_formula, environment)

          cond do
            sub_result == [] ->
              {:ok, 1}

            is_integer(sub_result) ->
              {:ok, sub_result + 1}

            is_binary(sub_result) ->
              {:ok, Noun.atom_binary_to_integer(sub_result) + 1}

            true ->
              :error
          end

        # 5: noun equality
        # *[a 5 b c]          =[*[a b] *[a c]]
        [five, formula_1 | formula_2] when five in [5, <<5>>] ->
          {:ok, result_1} = nock(subject, formula_1, environment)
          {:ok, result_2} = nock(subject, formula_2, environment)

          if Noun.equal(result_1, result_2) do
            {:ok, 0}
          else
            {:ok, 1}
          end

        # 6: if-then-else (spec macro)
        # *[a 6 b c d]        *[a *[[c d] 0 *[[2 3] 0 *[a 4 4 b]]]]
        [six, cond | branches = [_true_branch | _false_branch]]
        when six in [6, <<6>>] ->
          {:ok, cond_plus_two} = nock(subject, [4 | [4 | cond]], environment)
          {:ok, crash_guard} = nock([2 | 3], [0 | cond_plus_two], environment)

          {:ok, branch_formula} =
            nock(branches, [0 | crash_guard], environment)

          nock(subject, branch_formula, environment)

        # 7: with subject (spec macro)
        # *[a 7 b c]          *[*[a b] c]
        [seven, subject_formula | sub_formula] when seven in [7, <<7>>] ->
          {:ok, new_subject} = nock(subject, subject_formula, environment)
          nock(new_subject, sub_formula, environment)

        # 8: push on subject (spec macro)
        # *[a 8 b c]          *[[*[a b] a] c]
        [eight, push_formula | sub_formula] when eight in [8, <<8>>] ->
          {:ok, pushed_noun} = nock(subject, push_formula, environment)
          new_subject = [pushed_noun | subject]
          nock(new_subject, sub_formula, environment)

        # 9: arm of core (spec macro)
        # *[a 9 b c]          *[*[a c] 2 [0 1] 0 b]
        [nine, axis | sub_formula] when nine in [9, <<9>>] ->
          {:ok, sub_result} = nock(subject, sub_formula, environment)
          nock(sub_result, [2 | [[0 | 1] | [0 | axis]]], environment)

        # 10: replace at axis
        # *[a 10 [b c] d]     #[b *[a c] *[a d]]
        [ten, [axis | replacement_formula] | sub_formula]
        when ten in [10, <<10>>] ->
          {:ok, replacement} = nock(subject, replacement_formula, environment)
          {:ok, sub_result} = nock(subject, sub_formula, environment)
          Noun.replace(axis, replacement, sub_result)

        # 11: hint (spec macro)
        # *[a 11 [b c] d]     *[[*[a c] *[a d]] 0 3]
        [eleven, [_hint_noun | hint_formula] | sub_formula]
        when eleven in [11, <<11>>] ->
          # must be computed, but is discarded
          {:ok, hint_result} = nock(subject, hint_formula, environment)
          {:ok, real_result} = nock(subject, sub_formula, environment)
          nock([hint_result | real_result], [0 | 3], environment)

        # *[a 11 b c]         *[a c]
        [eleven, _hint_noun | sub_formula] when eleven in [11, <<11>>] ->
          nock(subject, sub_formula, environment)

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
      [8 [1 0 0] [1 8 [1 133.480.762.729.846] 0 0] 0 1]
      11
      [1.953.718.630 1 133.480.762.729.846 [0 7] 0]
      0
      1
    ]
    [ 7
      [8 [1 0 0] [1 8 [1 1.852.270.963] 0 0] 0 1]
      11
      [1.953.718.630 1 1.852.270.963 [0 7] 0]
      0
      1
    ]
    [ 7
      [ 8
        [1 0 0 0]
        [1 8 [1 133.449.338.811.408.550.175.386.169.016.308.622.710] 0 0]
        0
        1
      ]
      11
      [ 1.953.718.630
        1
        133.449.338.811.408.550.175.386.169.016.308.622.710
        [0 7]
        0
      ]
      0
      1
    ]
    7
    [ 8
      [1 0 0]
      [1 8 [1 2.036.275.311.453.377.535.635.164.748.015.987] 0 0]
      0
      1
    ]
    11
    [ 1.953.718.630
      1
      2.036.275.311.453.377.535.635.164.748.015.987
      [0 7]
      0
    ]
    0
    1
  ]
  [ [ 7
      [ 8
        [1 0 0]
        [ 1
          8
          [1 0 0]
          8
          [ 1
            6
            [6 [5 [1 0] 0 60] [6 [5 [1 0] 0 61] [1 0] 1 1] 1 1]
            [0 13]
            9
            2
            10
            [ 30
              [ 8
                [8 [9 10 0 63] 9 767 10 [6 7 [0 3] 1 0] 0 2]
                9
                2
                10
                [6 [7 [0 3] 1 1] 0 124]
                0
                2
              ]
              8
              [8 [9 10 0 63] 9 767 10 [6 7 [0 3] 1 0] 0 2]
              9
              2
              10
              [6 [7 [0 3] 1 1] 0 125]
              0
              2
            ]
            10
            [ 6
              [4 0 12]
              8
              [9 20 0 511]
              9
              2
              10
              [ 6
                [0 29]
                7
                [0 3]
                8
                [8 [9 10 0 63] 9 90 10 [6 7 [0 3] 1 0] 0 2]
                9
                2
                10
                [ 6
                  [0 28]
                  7
                  [0 3]
                  5
                  [ 8
                    [8 [9 10 0 63] 9 367 10 [6 7 [0 3] 1 0] 0 2]
                    9
                    2
                    10
                    [6 [7 [0 3] 1 1] 0 124]
                    0
                    2
                  ]
                  8
                  [8 [9 10 0 63] 9 367 10 [6 7 [0 3] 1 0] 0 2]
                  9
                  2
                  10
                  [6 [7 [0 3] 1 1] 0 125]
                  0
                  2
                ]
                0
                2
              ]
              0
              2
            ]
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
      [1.953.718.630 1 7.891.309 [0 7] 0]
      0
      1
    ]
    [ [ 8
        [ [8 [1 0] [1 8 [0 6] 8 [5 [0 14] 0 2] 0 6] 0 1]
          8
          [1 0]
          [1 8 [0 6] 8 [5 [0 14] 0 2] 0 6]
          0
          1
        ]
        [ 1
          8
          [[0 50] 0 54]
          [ 1
            8
            [[8 [0 60] 9 2 10 [6 0 28] 0 2] 8 [0 61] 9 2 10 [6 0 29] 0 2]
            8
            [5 [0 14] 0 2]
            0
            6
          ]
          0
          1
        ]
        0
        1
      ]
      [ 8
        [ [8 [1 0] [1 8 [0 6] 8 [5 [0 14] 0 2] 0 6] 0 1]
          8
          [1 0]
          [1 8 [0 6] 8 [5 [0 14] 0 2] 0 6]
          0
          1
        ]
        [ 1
          8
          [9 46 0 7]
          9
          2
          10
          [6 7 [0 3] 8 [9 20 0 7] 9 2 10 [6 [0 28] 0 29] 0 2]
          0
          2
        ]
        0
        1
      ]
      7
      [ 8
        [1 0]
        [ 1
          6
          [5 [1 0] 0 6]
          [1 1 1]
          8
          [ 8
            [8 [9 10 0 15] 9 190 10 [6 7 [0 3] 1 0] 0 2]
            9
            2
            10
            [6 0 14]
            0
            2
          ]
          8
          [ 8
            [8 [9 10 0 31] 9 190 10 [6 7 [0 3] 1 0] 0 2]
            9
            2
            10
            [6 0 6]
            0
            2
          ]
          [ 8
            [9 20 0 511]
            9
            2
            10
            [6 [7 [0 3] 8 [9 20 0 511] 9 2 10 [6 [0 6] 0 6] 0 2] 0 14]
            0
            2
          ]
          8
          [8 [9 10 0 63] 9 4 10 [6 7 [0 3] 1 0] 0 2]
          9
          2
          10
          [ 6
            [7 [0 3] 8 [9 4 0 63] 9 2 10 [6 0 6] 0 2]
            7
            [0 3]
            8
            [9 4 0 31]
            9
            2
            10
            [ 6
              [ 7
                [0 3]
                8
                [8 [9 10 0 63] 9 367 10 [6 7 [0 3] 1 0] 0 2]
                9
                2
                10
                [6 [7 [0 3] 8 [9 342 0 511] 9 2 10 [6 0 6] 0 2] 0 14]
                0
                2
              ]
              7
              [0 3]
              8
              [8 [9 10 0 63] 9 90 10 [6 7 [0 3] 1 0] 0 2]
              9
              2
              10
              [6 [7 [0 3] 8 [9 342 0 511] 9 2 10 [6 0 6] 0 2] 0 62]
              0
              2
            ]
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
      [1.953.718.630 1 7.627.117 [0 7] 0]
      0
      1
    ]
    [ 7
      [ 8
        [1 0]
        [ 1
          8
          [1 0]
          7
          [ 8
            [ 1
              6
              [6 [3 0 30] [1 1] 1 0]
              [ 8
                [8 [9 43 0 31] 9 2 10 [6 0 62] 0 2]
                [8 [9 20 0 1.023] 9 2 10 [6 [7 [0 3] 1 1] 0 12] 0 2]
                8
                [8 [9 10 0 127] 9 90 10 [6 7 [0 3] 1 0] 0 2]
                9
                2
                10
                [6 [7 [0 3] 1 1] 0 13]
                0
                2
              ]
              7
              [ 10
                [6 8 [9 20 0 511] 9 2 10 [6 [7 [0 3] 1 2] 0 14] 0 2]
                0
                1
              ]
              8
              [9 2 10 [30 0 60] 0 1]
              8
              [ 9
                2
                10
                [30 0 125]
                10
                [6 8 [9 20 0 1.023] 9 2 10 [6 [0 30] 0 12] 0 2]
                0
                3
              ]
              [ 8
                [9 20 0 2.047]
                9
                2
                10
                [ 6
                  [7 [0 3] 1 2]
                  7
                  [0 3]
                  8
                  [9 20 0 2.047]
                  9
                  2
                  10
                  [6 [0 28] 0 12]
                  0
                  2
                ]
                0
                2
              ]
              8
              [9 4 0 127]
              9
              2
              10
              [ 6
                [7 [0 3] 1 1]
                7
                [0 3]
                8
                [8 [9 10 0 255] 9 90 10 [6 7 [0 3] 1 0] 0 2]
                9
                2
                10
                [ 6
                  [7 [0 3] 1 2]
                  7
                  [0 3]
                  8
                  [8 [9 10 0 255] 9 4 10 [6 7 [0 3] 1 0] 0 2]
                  9
                  2
                  10
                  [6 [0 29] 0 13]
                  0
                  2
                ]
                0
                2
              ]
              0
              2
            ]
            9
            2
            0
            1
          ]
          0
          3
        ]
        0
        1
      ]
      11
      [1.953.718.630 1 7.168.362 [0 7] 0]
      0
      1
    ]
    [ 8
      [8 [1 0] [1 8 [0 6] 8 [5 [0 14] 0 2] 0 6] 0 1]
      [ 1
        8
        [1 0]
        [ 1
          8
          [ 6
            [3 0 6]
            [ [8 [0 30] 9 2 10 [6 0 28] 0 2]
              [ 8
                [7 [0 7] 8 [9 46 0 7] 9 2 10 [6 0 14] 0 2]
                9
                2
                10
                [6 0 58]
                0
                2
              ]
              8
              [7 [0 7] 8 [9 46 0 7] 9 2 10 [6 0 14] 0 2]
              9
              2
              10
              [6 0 59]
              0
              2
            ]
            6
            [5 [1 0] 0 6]
            [1 0]
            0
            0
          ]
          8
          [5 [0 14] 0 2]
          0
          6
        ]
        0
        1
      ]
      0
      1
    ]
    [ 7
      [ 8
        [1 0]
        [ 1
          8
          [1 0]
          7
          [ 8
            [ 1
              6
              [ 5
                [1 0]
                8
                [8 [9 10 0 63] 9 94 10 [6 7 [0 3] 1 0] 0 2]
                9
                2
                10
                [6 [7 [0 3] [0 6] 1 1] 0 62]
                0
                2
              ]
              [ 8
                [8 [9 95 0 31] 9 2 10 [6 [7 [0 3] 4 0 6] 0 62] 0 2]
                [4 0 4]
                0
                5
              ]
              8
              [8 [9 20 0 511] 9 2 10 [6 [7 [0 3] 1 2] 0 14] 0 2]
              6
              [ 5
                [1 0]
                8
                [8 [9 10 0 127] 9 94 10 [6 7 [0 3] 1 0] 0 2]
                9
                2
                10
                [6 [7 [0 3] [4 0 14] 1 1] 0 126]
                0
                2
              ]
              [ 8
                [9 2 10 [6 0 2] 0 3]
                8
                [ 9
                  2
                  10
                  [6 8 [9 20 0 2.047] 9 2 10 [6 [0 12] 0 14] 0 2]
                  0
                  7
                ]
                8
                [[0 13] 0 5]
                [ 8
                  [9 20 0 8.191]
                  9
                  2
                  10
                  [ 6
                    [7 [0 3] 1 2]
                    7
                    [0 3]
                    8
                    [9 20 0 8.191]
                    9
                    2
                    10
                    [6 [0 60] 0 28]
                    0
                    2
                  ]
                  0
                  2
                ]
                0
                2
              ]
              0
              0
            ]
            9
            2
            0
            1
          ]
          0
          3
        ]
        0
        1
      ]
      11
      [1.953.718.630 1 6.649.187 [0 7] 0]
      0
      1
    ]
    7
    [ 8
      [1 0 0]
      [ 1
        8
        [ 8
          [ [1 0]
            8
            [8 [9 10 0 15] 9 190 10 [6 7 [0 3] 1 0] 0 2]
            9
            2
            10
            [6 0 29]
            0
            2
          ]
          8
          [ 1
            6
            [8 [9 43 0 511] 9 2 10 [6 [0 28] 0 29] 0 2]
            [0 0]
            6
            [ 5
              [1 0]
              8
              [8 [9 10 0 63] 9 94 10 [6 7 [0 3] 1 0] 0 2]
              9
              2
              10
              [ 6
                [ 7
                  [0 3]
                  [8 [9 20 0 511] 9 2 10 [6 [0 124] 0 28] 0 2]
                  1
                  1
                ]
                0
                125
              ]
              0
              2
            ]
            [9 2 10 [12 4 0 12] 0 1]
            0
            12
          ]
          9
          2
          0
          1
        ]
        6
        [5 [1 0] 0 2]
        [1 1 0]
        8
        [8 [9 20 0 255] 9 2 10 [6 [0 60] 7 [0 3] 4 0 2] 0 2]
        8
        [ 8
          [9 20 0 511]
          9
          2
          10
          [ 6
            [ 7
              [0 3]
              8
              [9 4 0 63]
              9
              2
              10
              [6 7 [0 3] 8 [9 342 0 511] 9 2 10 [6 0 14] 0 2]
              0
              2
            ]
            7
            [0 3]
            8
            [8 [9 10 0 63] 9 94 10 [6 7 [0 3] 1 0] 0 2]
            9
            2
            10
            [6 [7 [0 3] [0 2] 8 [9 342 0 511] 9 2 10 [6 0 14] 0 2] 0 125]
            0
            2
          ]
          0
          2
        ]
        [ 8
          [9 20 0 1.023]
          9
          2
          10
          [6 [7 [0 3] 8 [9 20 0 1.023] 9 2 10 [6 [0 30] 0 30] 0 2] 0 6]
          0
          2
        ]
        8
        [8 [9 10 0 127] 9 94 10 [6 7 [0 3] 1 0] 0 2]
        9
        2
        10
        [ 6
          [ 7
            [0 3]
            [ 8
              [9 20 0 1.023]
              9
              2
              10
              [6 [0 14] 7 [0 3] 8 [9 342 0 1.023] 9 2 10 [6 0 30] 0 2]
              0
              2
            ]
            0
            2
          ]
          0
          253
        ]
        0
        2
      ]
      0
      1
    ]
    11
    [1.953.718.630 1 6.452.594 [0 7] 0]
    0
    1
  ]
  [ [ 8
      [1 0]
      [ 1
        6
        [5 [1 0] 0 6]
        [1 1]
        8
        [9 4 0 63]
        9
        2
        10
        [ 6
          [7 [0 3] 1 2]
          7
          [0 3]
          9
          2
          10
          [6 8 [9 342 0 63] 9 2 10 [6 0 14] 0 2]
          0
          1
        ]
        0
        2
      ]
      0
      1
    ]
    [ 8
      [1 0]
      [ 1
        [ 8
          [1 0 0]
          [ 1
            8
            [9 20 0 255]
            9
            2
            10
            [ 6
              [ 7
                [0 3]
                8
                [9 90 0 7]
                9
                2
                10
                [6 [7 [0 3] 8 [9 190 0 7] 9 2 10 [6 0 28] 0 2] 0 29]
                0
                2
              ]
              0
              28
            ]
            0
            2
          ]
          0
          1
        ]
        [8 [1 0] [1 8 [9 367 0 7] 9 2 10 [6 [7 [0 3] 1 1] 0 14] 0 2] 0 1]
        [ [ 8
            [1 0 0]
            [ 1
              8
              [1 0]
              7
              [10 [29 8 [9 10 0 15] 9 2 10 [6 0 61] 0 2] 0 1]
              8
              [0 29]
              8
              [ 1
                6
                [5 [0 14] 0 124]
                [8 [9 767 0 63] 9 2 10 [6 [7 [0 3] 1 1] 0 14] 0 2]
                9
                2
                10
                [14 4 0 14]
                10
                [ 6
                  8
                  [9 20 0 2.047]
                  9
                  2
                  10
                  [ 6
                    [0 253]
                    7
                    [0 3]
                    8
                    [9 90 0 63]
                    9
                    2
                    10
                    [6 [7 [0 3] 1 1] 0 14]
                    0
                    2
                  ]
                  0
                  2
                ]
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
          [ 8
            [1 0 0]
            [ 1
              8
              [9 4 0 255]
              9
              2
              10
              [ 6
                [0 29]
                7
                [0 3]
                8
                [9 4 0 31]
                9
                2
                10
                [ 6
                  7
                  [0 3]
                  8
                  [9 4 0 255]
                  9
                  2
                  10
                  [6 [7 [0 3] 9 182 0 7] 0 28]
                  0
                  2
                ]
                0
                2
              ]
              0
              2
            ]
            0
            1
          ]
          [8 [9 4 0 7] 9 2 10 [6 0 14] 0 2]
          [ 8
            [1 0]
            [ 1
              8
              [9 47 0 255]
              9
              2
              10
              [ 6
                [ 7
                  [0 3]
                  8
                  [9 342 0 255]
                  9
                  2
                  10
                  [6 7 [0 3] 9 382 0 7]
                  0
                  2
                ]
                7
                [0 3]
                8
                [9 10 0 7]
                9
                2
                10
                [6 0 14]
                0
                2
              ]
              0
              2
            ]
            0
            1
          ]
          8
          [1 0 0]
          [ 1
            8
            [9 46 0 255]
            9
            2
            10
            [ 6
              [0 29]
              7
              [0 3]
              8
              [9 4 0 31]
              9
              2
              10
              [ 6
                7
                [0 3]
                8
                [9 4 0 255]
                9
                2
                10
                [6 [7 [0 3] 9 182 0 7] 0 28]
                0
                2
              ]
              0
              2
            ]
            0
            2
          ]
          0
          1
        ]
        [ 8
          [1 0 0]
          [ 1
            8
            [9 10 0 7]
            9
            2
            10
            [6 7 [0 3] 8 [9 20 0 255] 9 2 10 [6 [0 28] 0 29] 0 2]
            0
            2
          ]
          0
          1
        ]
        [ 8
          [1 [0 0] 0]
          [ 1
            8
            [9 367 0 7]
            9
            2
            10
            [6 [0 57] 7 [0 3] 8 [9 767 0 7] 9 2 10 [6 [0 56] 0 29] 0 2]
            0
            2
          ]
          0
          1
        ]
        [ 8
          [1 0]
          [ 1
            8
            [1 0]
            8
            [ 1
              6
              [5 [1 0] 0 30]
              [0 6]
              9
              2
              10
              [30 8 [9 767 0 31] 9 2 10 [6 [7 [0 3] 1 1] 0 62] 0 2]
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
        [ 8
          [9 4 0 7]
          9
          2
          10
          [6 7 [0 3] 8 [9 4 0 7] 9 2 10 [6 0 14] 0 2]
          0
          2
        ]
        [ 8
          [1 0]
          [ 1
            8
            [9 46 0 7]
            9
            2
            10
            [6 [7 [0 3] 8 [9 366 0 7] 9 2 10 [6 0 14] 0 2] 7 [0 3] 1 1]
            0
            2
          ]
          0
          1
        ]
        8
        [1 0 0]
        [ 1
          8
          [9 170 0 255]
          9
          2
          10
          [ 6
            [0 29]
            7
            [0 3]
            8
            [9 4 0 31]
            9
            2
            10
            [ 6
              7
              [0 3]
              8
              [9 4 0 255]
              9
              2
              10
              [6 [7 [0 3] 9 182 0 7] 0 28]
              0
              2
            ]
            0
            2
          ]
          0
          2
        ]
        0
        1
      ]
      0
      1
    ]
    8
    [1 0]
    [1 8 [8 [9 10 0 7] 9 190 10 [6 7 [0 3] 1 0] 0 2] 9 2 10 [6 0 14] 0 2]
    0
    1
  ]
  [ [ 7
      [ 8
        [1 0 0]
        [ 1
          6
          [5 [1 0] 0 13]
          [1 1]
          8
          [9 4 0 31]
          9
          2
          10
          [ 6
            [0 28]
            7
            [0 3]
            9
            2
            10
            [13 8 [9 342 0 31] 9 2 10 [6 0 29] 0 2]
            0
            1
          ]
          0
          2
        ]
        0
        1
      ]
      11
      [1.953.718.630 1 7.827.312 [0 7] 0]
      0
      1
    ]
    8
    [1 0]
    [ 1
      [ 8
        [1 1 1]
        [ 1
          8
          [9 42 0 7]
          9
          2
          10
          [6 7 [0 3] 8 [9 4 0 127] 9 2 10 [6 [0 28] 0 29] 0 2]
          0
          2
        ]
        0
        1
      ]
      [ [ 8
          [1 0 0]
          [ 1
            8
            [9 42 0 7]
            9
            2
            10
            [6 7 [0 3] 8 [9 20 0 127] 9 2 10 [6 [0 28] 0 29] 0 2]
            0
            2
          ]
          0
          1
        ]
        [8 [1 0] [1 8 [9 46 0 127] 9 2 10 [6 [0 14] 0 62] 0 2] 0 1]
        [ 8
          [1 1 1]
          [ 1
            8
            [9 4 0 7]
            9
            2
            10
            [6 [0 28] 7 [0 3] 8 [9 174 0 7] 9 2 10 [6 0 29] 0 2]
            0
            2
          ]
          0
          1
        ]
        [8 [1 0] [1 0 0] 0 1]
        8
        [1 0]
        [ 1
          8
          [9 47 0 127]
          9
          2
          10
          [6 [0 62] 7 [0 3] 8 [9 42 0 7] 9 2 10 [6 0 14] 0 2]
          0
          2
        ]
        0
        1
      ]
      [ 8
        [1 0 0]
        [ 1
          5
          [8 [9 42 0 7] 9 2 10 [6 0 28] 0 2]
          8
          [9 42 0 7]
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
      [ 8
        [1 0 0]
        [ 1
          8
          [9 42 0 7]
          9
          2
          10
          [6 7 [0 3] 8 [9 4 0 31] 9 2 10 [6 [0 28] 0 29] 0 2]
          0
          2
        ]
        0
        1
      ]
      8
      [1 0 0]
      [ 1
        8
        [9 42 0 7]
        9
        2
        10
        [ 6
          7
          [0 3]
          8
          [9 47 0 127]
          9
          2
          10
          [ 6
            [7 [0 3] 8 [9 20 0 127] 9 2 10 [6 [0 62] 0 28] 0 2]
            7
            [0 3]
            8
            [9 42 0 7]
            9
            2
            10
            [6 0 29]
            0
            2
          ]
          0
          2
        ]
        0
        2
      ]
      0
      1
    ]
    0
    1
  ]
  [ [ 8
      [1 [[0 15] [0 0] [0 0] 0] 0]
      [1 8 [0 101] [1 8 [0 60] 9 2 10 [6 [0 125] 0 14] 0 2] 0 1]
      0
      1
    ]
    [ [ 8
        [8 [1 0] [1 8 [0 6] 8 [5 [0 14] 0 2] 0 6] 0 1]
        [ 1
          8
          [1 0]
          [1 8 [7 [0 7] [1 7 [0 14] 9 2 0 1] 0 1] 8 [5 [0 14] 0 2] 0 6]
          0
          1
        ]
        0
        1
      ]
      [ 8
        [8 [1 0] [1 8 [0 6] 8 [5 [0 14] 0 2] 0 6] 0 1]
        [ 1
          8
          [1 0]
          [ 1
            8
            [ 6
              [3 0 6]
              [[6 [5 [1 0] 0 12] [1 0] 0 0] 8 [0 30] 9 2 10 [6 0 29] 0 2]
              6
              [5 [1 0] 0 6]
              [1 0]
              0
              0
            ]
            8
            [5 [0 14] 0 2]
            0
            6
          ]
          0
          1
        ]
        0
        1
      ]
      [ 8
        [8 [1 0] [1 8 [0 6] 8 [5 [0 14] 0 2] 0 6] 0 1]
        [ 1
          8
          [[0 26] 7 [8 [9 47 0 7] 9 2 10 [6 0 14] 0 2] 0 6]
          [ 1
            8
            [ [8 [0 30] 9 2 10 [6 0 28] 0 2]
              8
              [7 [0 7] 8 [9 47 0 7] 9 2 10 [6 0 14] 0 2]
              9
              2
              10
              [6 0 29]
              0
              2
            ]
            8
            [5 [0 14] 0 2]
            0
            6
          ]
          0
          1
        ]
        0
        1
      ]
      [ 8
        [1 0]
        [1 8 [7 [1 0 0] 8 [0 2] [1 0 15] 0 1] 8 [5 [0 14] 0 2] 0 6]
        0
        1
      ]
      [ 8
        [ [8 [1 0] [1 8 [0 6] 8 [5 [0 14] 0 2] 0 6] 0 1]
          8
          [1 0]
          [1 8 [0 6] 8 [5 [0 14] 0 2] 0 6]
          0
          1
        ]
        [ 1
          8
          [[1 0] 0 54]
          [ 1
            8
            [ 6
              [5 [1 1] 0 12]
              [[6 [5 [0 12] 1 1] [1 1] 0 0] 8 [0 60] 9 2 10 [6 0 29] 0 2]
              [6 [5 [0 12] 1 0] [1 0] 0 0]
              8
              [0 61]
              9
              2
              10
              [6 0 29]
              0
              2
            ]
            8
            [5 [0 14] 0 2]
            0
            6
          ]
          0
          1
        ]
        0
        1
      ]
      [ 8
        [8 [1 0] [1 8 [0 6] 8 [5 [0 14] 0 2] 0 6] 0 1]
        [ 1
          8
          [1 0]
          [ 1
            8
            [ 6
              [3 0 6]
              [ [8 [0 30] 9 2 10 [6 0 28] 0 2]
                [ 8
                  [7 [0 7] 8 [9 702 0 7] 9 2 10 [6 0 14] 0 2]
                  9
                  2
                  10
                  [6 0 58]
                  0
                  2
                ]
                8
                [7 [0 7] 8 [9 702 0 7] 9 2 10 [6 0 14] 0 2]
                9
                2
                10
                [6 0 59]
                0
                2
              ]
              6
              [5 [1 0] 0 6]
              [1 0]
              0
              0
            ]
            8
            [5 [0 14] 0 2]
            0
            6
          ]
          0
          1
        ]
        0
        1
      ]
      [8 [1 0] [1 6 [5 [1 0] 0 6] [1 0] 4 9 2 10 [6 0 13] 0 1] 0 1]
      8
      [8 [1 0] [1 8 [0 6] 8 [5 [0 14] 0 2] 0 6] 0 1]
      [ 1
        8
        [8 [1 0] [1 8 [0 6] 8 [5 [0 14] 0 2] 0 6] 0 1]
        8
        [5 [0 14] 0 2]
        0
        6
      ]
      0
      1
    ]
    [ 8
      [1 0 [0 15] 0 0 0]
      [ 1
        8
        [ 1
          6
          [5 [1 0] 0 28]
          [1 0]
          [8 [0 29] 9 2 10 [6 0 120] 0 2]
          9
          2
          10
          [28 0 57]
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
    [ 8
      [1 0 [0 13] [0 0] 0]
      [ 1
        8
        [ 1
          6
          [5 [1 0] 0 28]
          [0 237]
          8
          [0 29]
          9
          2
          10
          [6 [0 120] 7 [0 3] 9 2 10 [28 0 57] 0 1]
          0
          2
        ]
        9
        2
        0
        1
      ]
      0
      1
    ]
    8
    [8 [1 0] [1 8 [0 6] 8 [5 [0 14] 0 2] 0 6] 0 1]
    [ 1
      8
      [1 0]
      [ 1
        8
        [ 6
          [3 0 6]
          [ [8 [0 30] 9 2 10 [6 0 28] 0 2]
            8
            [7 [0 7] 8 [9 47 0 7] 9 2 10 [6 0 14] 0 2]
            9
            2
            10
            [6 0 29]
            0
            2
          ]
          6
          [5 [1 0] 0 6]
          [1 0]
          0
          0
        ]
        8
        [5 [0 14] 0 2]
        0
        6
      ]
      0
      1
    ]
    0
    1
  ]
  [ [ 7
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
      [ 8
        [1 0 0]
        [1 6 [8 [9 84 0 7] 9 2 10 [6 [0 28] 0 29] 0 2] [1 1] 1 0]
        0
        1
      ]
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

  rm_string = """
  [ [1 0]
  [ 8
    [1 [[[1 0] [0 0] 0] 0 0 0 0 0 0 0] 0 0]
    [ 1
      8
      [ [8 [7 [0 7] 9 378 0 1] 9 2 10 [6 0 28] 0 2]
        [8 [7 [0 7] 8 [9 47 0 63] 9 2 10 [6 7 [0 3] 9 22 0 1] 0 2] 9 2 10 [6 0 58] 0 2]
        8
        [7 [0 7] 8 [9 47 0 63] 9 2 10 [6 7 [0 3] 9 188 0 1] 0 2]
        9
        2
        10
        [6 0 59]
        0
        2
      ]
      8
      [5 [0 14] 0 2]
      0
      6
    ]
    0
    1
  ]
  [8 [1 0] [1 8 [6 [6 [3 0 6] [1 1] 1 0] [0 6] 0 0] 8 [5 [0 14] 0 2] 0 6] 0 1]
  [ 8
    [1 0 0 0 0 0 0 0]
    [ 1
      8
      [ [8 [7 [0 7] 8 [9 47 0 63] 9 2 10 [6 7 [0 3] 8 [1 0] [1 8 [6 [6 [3 0 6] [1 1] 1 0] [0 6] 0 0] 8 [5 [0 14] 0 2] 0 6] 0 1] 0 2] 9 2 10 [6 0 28] 0 2]
        [8 [7 [0 7] 8 [9 47 0 63] 9 2 10 [6 7 [0 3] 9 22 0 1] 0 2] 9 2 10 [6 0 58] 0 2]
        [8 [7 [0 7] 8 [9 47 0 63] 9 2 10 [6 7 [0 3] 9 188 0 1] 0 2] 9 2 10 [6 0 118] 0 2]
        [8 [7 [0 7] 8 [9 47 0 63] 9 2 10 [6 7 [0 3] 9 10 0 1] 0 2] 9 2 10 [6 0 238] 0 2]
        [8 [7 [0 7] 9 190 0 1] 9 2 10 [6 0 478] 0 2]
        [6 [6 [3 0 446] [1 1] 1 0] [0 446] 0 0]
        6
        [5 [1 0] 0 447]
        [1 0]
        0
        0
      ]
      8
      [5 [0 14] 0 2]
      0
      6
    ]
    0
    1
  ]
  [ [8 [1 0] [1 8 [6 [6 [3 0 6] [1 1] 1 0] [0 6] 0 0] 8 [5 [0 14] 0 2] 0 6] 0 1]
    [8 [1 [[1 0] [0 0] 0] 0 0 0 0 0 0 0] [1 8 [8 [7 [0 7] 9 379 0 1] 9 2 10 [6 0 14] 0 2] 8 [5 [0 14] 0 2] 0 6] 0 1]
    8
    [1 [[1 0] [0 0] 0] 0 0 0 0 0 0 0]
    [ 1
      8
      [ [8 [7 [0 7] 9 191 0 1] 9 2 10 [6 0 28] 0 2]
        [6 [6 [3 0 26] [1 1] 1 0] [0 26] 0 0]
        [6 [6 [3 0 54] [1 1] 1 0] [0 54] 0 0]
        [6 [6 [3 0 110] [1 1] 1 0] [0 110] 0 0]
        [6 [5 [1 0] 0 222] [1 0] 6 [5 [1 1] 0 222] [1 1] 0 0]
        [6 [6 [3 0 446] [1 1] 1 0] [0 446] 0 0]
        [6 [6 [3 0 894] [1 1] 1 0] [0 894] 0 0]
        6
        [6 [3 0 895] [1 1] 1 0]
        [0 895]
        0
        0
      ]
      8
      [5 [0 14] 0 2]
      0
      6
    ]
    0
    1
  ]
  [ 8
    [1 0]
    [ 1
      8
      [ 8
        [ 7
          [0 7]
          8
          [9 47 0 63]
          9
          2
          10
          [ 6
            7
            [0 3]
            8
            [1 0 0 0]
            [1 8 [[6 [6 [3 0 12] [1 1] 1 0] [0 12] 0 0] [6 [5 [1 0] 0 26] [1 0] 6 [5 [1 1] 0 26] [1 1] 0 0] 6 [6 [3 0 27] [1 1] 1 0] [0 27] 0 0] 8 [5 [0 14] 0 2] 0 6]
            0
            1
          ]
          0
          2
        ]
        9
        2
        10
        [6 0 14]
        0
        2
      ]
      8
      [5 [0 14] 0 2]
      0
      6
    ]
    0
    1
  ]
  8
  [1 [1 0] [0 0] 0]
  [1 8 [7 [1 0] 8 [1 0 0] [1 1 0] 0 1] 8 [5 [0 14] 0 2] 0 6]
  0
  1
  ]
  """

  # evaluated at compile time.
  @rm_core_val [Noun.Format.parse_always(rm_string) | @stdlib_core_val]

  @spec rm_core :: Noun.t()
  def rm_core do
    @rm_core_val
  end

  logics_string = """
  [ [ 8
    [1 [[[1 0] [0 0] 0] 0 0 0 0 0 0 0] 0 0 0 0 0 0 0]
    [ 1
      6
      [5 [1 1] 8 [9 1.406 0 511] 9 2 10 [6 0 118] 0 2]
      [ 6
        [5 [1 1] 8 [9 1.406 0 511] 9 2 10 [6 0 238] 0 2]
        [6 [5 [1 1] 8 [9 1.406 0 511] 9 2 10 [6 0 958] 0 2] [6 [5 [1 0] 0 446] [0 0] 6 [0 3.570] [1 0] 1 1] 1 1]
        1
        1
      ]
      1
      1
    ]
    0
    1
  ]
  8
  [1 [[[1 0] [0 0] 0] 0 0 0 0 0 0 0] 0 0 0 0 0 0 0]
  [1 5 [1 0] 0 446]
  0
  1
  ]
  """

  # evaluated at compile time.
  @logics_core_val [Noun.Format.parse_always(logics_string) | @rm_core_val]

  @spec logics_core :: Noun.t()
  def logics_core do
    @logics_core_val
  end

  ############################################################
  #                          Helpers                         #
  ############################################################

  @spec read_with_id(Noun.t(), t()) :: {:ok, Noun.t()} | :error
  def read_with_id(id, env) do
    ordering = env.ordering

    if ordering && env.snapshot_path && id do
      with [id, key | 0] <- id,
           snap_id = [id | env.snapshot_path],
           {:ok, snap} <- Ordering.caller_blocking_read_id(ordering, snap_id),
           instrument({:snapshot, snap}),
           {:ok, value} <- Storage.get_at_snapshot(snap, key) do
        instrument({:got_value, value})
        {:ok, value}
      else
        _ -> :error
      end
    else
      :error
    end
  end

  defp instrument({:snapshot, snap}) do
    Logger.info("got snapshot: #{inspect(snap)}")
  end

  defp instrument({:got_value, value}) do
    Logger.info("got value: #{inspect(value)}")
  end
end
