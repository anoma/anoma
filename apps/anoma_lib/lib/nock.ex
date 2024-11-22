defmodule Nock do
  @moduledoc """
  Nock, a universal function on nouns.
  """

  require Noun
  require Logger

  use TypedStruct

  alias __MODULE__

  @typedoc """
  I contain environmental information on how Nock shall be evaluated.

  For example Ι contain information on jettedness to
  determine if we should be using jets or not

  """
  typedstruct do
    field(:scry_function, (term() -> {:ok, term()} | :error),
      default: &__MODULE__.scry_forbidden/1
    )

    field(:meter_pid, pid() | nil, default: nil)
    field(:stdio, any(), default: :stdio)
  end

  @dialyzer :no_improper_lists

  @layers 8

  @layer_1_context_mug 1_023_856_422
  @layer_4_context_mug 1_869_390_925
  @layer_5_context_mug 4_018_337_361
  @layer_6_context_mug 3_932_234_981
  @layer_7_context_mug 4_202_542_228
  @layer_8_context_mug 1_736_366_676
  @layer_4_block_context_mug 2_756_805_836

  # hardcoded jet registry
  # valid statuses:
  # - :enabled, jet is fully enabled
  # - :disabled, jet is fully disabled
  # - :check, check that jet and naive produce the same result
  @jet_registry %{
    3_739_349_216 =>
      {"dec", 7, @layer_1_context_mug, &Nock.Jets.dec/1, :enabled, 10},
    3_819_277_753 =>
      {"add", 7, @layer_1_context_mug, &Nock.Jets.add/1, :enabled, 10},
    2_374_874_615 =>
      {"sub", 7, @layer_1_context_mug, &Nock.Jets.sub/1, :enabled, 10},
    1_130_480_894 =>
      {"lth", 7, @layer_1_context_mug, &Nock.Jets.lth/1, :enabled, 10},
    2_271_972_775 =>
      {"lte", 7, @layer_1_context_mug, &Nock.Jets.lte/1, :enabled, 10},
    3_066_075_584 =>
      {"gth", 7, @layer_1_context_mug, &Nock.Jets.gth/1, :enabled, 10},
    2_449_621_320 =>
      {"gte", 7, @layer_1_context_mug, &Nock.Jets.gte/1, :enabled, 10},
    2_208_363_748 =>
      {"mul", 7, @layer_1_context_mug, &Nock.Jets.mul/1, :enabled, 10},
    91_135_323 =>
      {"div", 7, @layer_1_context_mug, &Nock.Jets.div/1, :enabled, 10},
    567_449_323 =>
      {"mod", 7, @layer_1_context_mug, &Nock.Jets.mod/1, :enabled, 10},
    1_531_543_893 =>
      {"verify", 7, @layer_6_context_mug, &Nock.Jets.verify/1, :enabled, 100},
    3_991_451_804 =>
      {"sign", 7, @layer_6_context_mug, &Nock.Jets.sign/1, :enabled, 100},
    3_656_885_839 =>
      {"verify-detatched", 7, @layer_6_context_mug,
       &Nock.Jets.verify_detatched/1, :enabled, 100},
    1_699_002_748 =>
      {"sign-detatched", 7, @layer_6_context_mug, &Nock.Jets.sign_detatched/1,
       :enabled, 100},
    1_585_653_763 =>
      {"bex", 7, @layer_4_context_mug, &Nock.Jets.bex/1, :enabled, 20},
    1_023_807_257 =>
      {"mix", 7, @layer_5_context_mug, &Nock.Jets.mix/1, :enabled, 20},
    2_968_763_525 =>
      {"jam", 7, @layer_5_context_mug, &Nock.Jets.jam/1, :enabled, 50},
    3_271_615_052 =>
      {"cue", 7, @layer_5_context_mug, &Nock.Jets.cue/1, :enabled, 50},
    1_423_749_879 =>
      {"shax", 7, @layer_7_context_mug, &Nock.Jets.shax/1, :enabled, 100},
    1_761_078_299 =>
      {"met", 14, @layer_4_block_context_mug, &Nock.Jets.met/1, :enabled, 20},
    3_976_423_375 =>
      {"end", 14, @layer_4_block_context_mug, &Nock.Jets.nend/1, :enabled, 20},
    3_534_989_962 =>
      {"lsh", 14, @layer_4_block_context_mug, &Nock.Jets.lsh/1, :enabled, 20},
    3_410_895_654 =>
      {"rsh", 14, @layer_4_block_context_mug, &Nock.Jets.rsh/1, :enabled, 20},
    724_462_226 =>
      {"abs", 7, @layer_8_context_mug, &Nock.Jets.abs/1, :enabled, 30},
    2_668_782_675 =>
      {"dif", 7, @layer_8_context_mug, &Nock.Jets.dif/1, :enabled, 30},
    1_814_685_155 =>
      {"dul", 7, @layer_8_context_mug, &Nock.Jets.dul/1, :enabled, 30},
    2_357_319_448 =>
      {"fra", 7, @layer_8_context_mug, &Nock.Jets.fra/1, :enabled, 30},
    2_272_237_948 =>
      {"pro", 7, @layer_8_context_mug, &Nock.Jets.pro/1, :enabled, 30},
    2_517_398_177 =>
      {"rem", 7, @layer_8_context_mug, &Nock.Jets.rem/1, :enabled, 30},
    2_325_836_748 =>
      {"sum", 7, @layer_8_context_mug, &Nock.Jets.sum/1, :enabled, 30},
    244_446_486 =>
      {"sun", 7, @layer_8_context_mug, &Nock.Jets.sun/1, :enabled, 30},
    1_720_910_226 =>
      {"syn", 7, @layer_8_context_mug, &Nock.Jets.syn/1, :enabled, 30},
    3_800_851_664 =>
      {"cmp", 7, @layer_8_context_mug, &Nock.Jets.cmp/1, :enabled, 30}
  }

  @doc """
  Gives the total numbers of layers in the standard library
  """
  @spec stdlib_layers() :: non_neg_integer()
  def stdlib_layers, do: @layers

  # temporary stub functions for jet scaffolding
  @spec get_jet(Noun.t()) ::
          {:ok,
           {String.t(), non_neg_integer(), non_neg_integer(),
            (Noun.t() -> :error | {:ok, Noun.t()}), atom(),
            non_neg_integer()}}
          | :error
  def get_jet(battery_mug) do
    Map.fetch(@jet_registry, battery_mug)
  end

  @spec put_jet(Noun.t(), any()) :: any()
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
  def nock(subject, [twelve, type_formula | sub_formula], env)
      when twelve in [12, <<12>>] do
    with {:ok, _type_result} <- nock(subject, type_formula, env),
         {:ok, sub_result} <- nock(subject, sub_formula, env) do
      read_with_id(sub_result, env)
    else
      _ -> :error
    end
  end

  # generic case: use naive nock to reduce once.
  @spec nock(Noun.t(), Noun.t(), t()) :: {:ok, Noun.t()} | :error
  def nock(subject, formula, environment) do
    naive_nock(subject, formula, environment)
  end

  @spec read_with_id(Noun.t(), t()) :: {:ok, Noun.t()} | :error
  def read_with_id(id_key_list, env) do
    if id_key_list do
      with [id, key] <- id_key_list |> Noun.list_nock_to_erlang(),
           {:ok, value} <-
             env.scry_function.({id, key}) do
        {:ok, value}
      else
        _ -> :error
      end
    else
      :error
    end
  end

  @spec scry_forbidden(Noun.t()) :: :error | {:ok, Noun.t()}
  def scry_forbidden(_) do
    :error
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
        [eleven, [hint_noun | hint_formula] | sub_formula]
        when eleven in [11, <<11>>] ->
          # must be computed, but is discarded
          {:ok, hint_result} = nock(subject, hint_formula, environment)
          process_hint(hint_noun, hint_result, environment)
          {:ok, real_result} = nock(subject, sub_formula, environment)
          nock([hint_result | real_result], [0 | 3], environment)

        # *[a 11 b c]         *[a c]
        [eleven, hint_noun | sub_formula] when eleven in [11, <<11>>] ->
          process_hint(hint_noun)
          nock(subject, sub_formula, environment)

        # else, error
        _ ->
          :error
      end
    rescue
      _ in MatchError -> :error
    end
  end

  # process_hint helper: noncontextual, but enough for %puts
  @spec process_hint(Noun.t()) :: term()
  def process_hint(_) do
  end

  # %puts hint: print an atom during evaluation
  @spec process_hint(Noun.t(), Noun.t(), t()) :: term()
  def process_hint(puts, hint_result, environment)
      when puts in [0x73747570, "puts"] do
    # if the output is not stdio, then the hint is jammed.
    # right now there is no way to reliably turn a noun into a string and read it back
    # if it has binaries. So this is a temporary workaround.
    if environment.stdio == :stdio do
      IO.write(environment.stdio, "#{inspect(hint_result)}\n")
    else
      # hint_str = Noun.Format.print(hint_result)
      hint_str = Nock.Jam.jam(hint_result) |> Base.encode64()
      IO.write(environment.stdio, hint_str)
    end
  end

  def process_hint(_puts, _hint_result, _environment) do
  end

  @spec cons(Noun.t(), Noun.t()) :: Noun.t()
  def cons(a, b) do
    [a | b]
  end

  @spec nock_0(Noun.t()) :: Noun.t()
  def nock_0(axis) do
    [0 | axis]
  end

  @spec nock_1(Noun.t()) :: Noun.t()
  def nock_1(constant) do
    [1 | constant]
  end

  @spec nock_2(Noun.t(), Noun.t()) :: Noun.t()
  def nock_2(subject_formula, formula_formula) do
    [2, subject_formula | formula_formula]
  end

  @spec nock_3(Noun.t()) :: Noun.t()
  def nock_3(sub_formula) do
    [3 | sub_formula]
  end

  @spec nock_4(Noun.t()) :: Noun.t()
  def nock_4(sub_formula) do
    [4 | sub_formula]
  end

  @spec nock_5(Noun.t(), Noun.t()) :: Noun.t()
  def nock_5(formula_1, formula_2) do
    [5, formula_1 | formula_2]
  end

  @spec nock_6(Noun.t(), Noun.t(), Noun.t()) :: Noun.t()
  def nock_6(cond, true_branch, false_branch) do
    [6, cond, true_branch | false_branch]
  end

  @spec nock_7(Noun.t(), Noun.t()) :: Noun.t()
  def nock_7(subject_formula, sub_formula) do
    [7, subject_formula | sub_formula]
  end

  @spec nock_8(Noun.t(), Noun.t()) :: Noun.t()
  def nock_8(push_formula, sub_formula) do
    [8, push_formula | sub_formula]
  end

  @spec nock_9(Noun.t(), Noun.t()) :: Noun.t()
  def nock_9(axis, sub_formula) do
    [9, axis | sub_formula]
  end

  @spec nock_10(Noun.t(), Noun.t(), Noun.t()) :: Noun.t()
  def nock_10(axis, replacement_formula, sub_formula) do
    [10, [axis | replacement_formula] | sub_formula]
  end

  @spec nock_11(Noun.t(), Noun.t(), Noun.t()) :: Noun.t()
  def nock_11(hint_noun, hint_formula, sub_formula) do
    [11, [hint_noun | hint_formula] | sub_formula]
  end

  @spec nock_11(Noun.t(), Noun.t()) :: Noun.t()
  def nock_11(hint_noun, sub_formula) do
    [11, hint_noun | sub_formula]
  end

  @spec decrement_arm() :: Noun.t()
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

  @spec decrement_core() :: Noun.t()
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
        [1 0 0]
        [ 1
          8
          [[8 [9 756 0 7] 9 2 10 [6 0 28] 0 2] 8 [9 756 0 7] 9 2 10 [6 0 29] 0 2]
          6
          [0 8]
          [ 6
            [0 10]
            [8 [9 758 0 15] 9 2 10 [6 [7 [0 3] 1 0] 7 [0 3] 8 [9 20 0 2.047] 9 2 10 [6 [0 25] 0 27] 0 2] 0 2]
            6
            [8 [9 22 0 2.047] 9 2 10 [6 [0 25] 0 27] 0 2]
            [8 [9 758 0 15] 9 2 10 [6 [7 [0 3] 1 0] 7 [0 3] 8 [9 47 0 2.047] 9 2 10 [6 [0 25] 0 27] 0 2] 0 2]
            8
            [9 758 0 15]
            9
            2
            10
            [6 [7 [0 3] 1 1] 7 [0 3] 8 [9 47 0 2.047] 9 2 10 [6 [0 27] 0 25] 0 2]
            0
            2
          ]
          6
          [0 10]
          [ 6
            [8 [9 22 0 2.047] 9 2 10 [6 [0 25] 0 27] 0 2]
            [8 [9 758 0 15] 9 2 10 [6 [7 [0 3] 1 1] 7 [0 3] 8 [9 47 0 2.047] 9 2 10 [6 [0 25] 0 27] 0 2] 0 2]
            8
            [9 758 0 15]
            9
            2
            10
            [6 [7 [0 3] 1 0] 7 [0 3] 8 [9 47 0 2.047] 9 2 10 [6 [0 27] 0 25] 0 2]
            0
            2
          ]
          8
          [9 758 0 15]
          9
          2
          10
          [6 [7 [0 3] 1 1] 7 [0 3] 8 [9 20 0 2.047] 9 2 10 [6 [0 25] 0 27] 0 2]
          0
          2
        ]
        0
        1
      ]
      11
      [1.953.718.630 1 7.173.491 [0 7] 0]
      0
      1
    ]
    [7 [8 [1 0] [1 8 [9 4 0 1.023] 9 2 10 [6 [7 [0 3] 1 2] 0 14] 0 2] 0 1] 11 [1.953.718.630 1 7.239.027 [0 7] 0] 0 1]
    [ 7
      [ 8
        [1 0 0]
        [ 1
          8
          [8 [9 756 0 7] 9 2 10 [6 0 28] 0 2]
          6
          [0 4]
          [8 [9 46 0 2.047] 9 2 10 [6 [0 13] 0 61] 0 2]
          8
          [9 47 0 2.047]
          9
          2
          10
          [6 [0 61] 0 13]
          0
          2
        ]
        0
        1
      ]
      11
      [1.953.718.630 1 7.107.940 [0 7] 0]
      0
      1
    ]
    [ 7
      [ 8
        [1 0 0]
        [ 1
          8
          [9 758 0 7]
          9
          2
          10
          [ 6
            [7 [0 3] 5 [1 0] 8 [9 4 0 63] 9 2 10 [6 [7 [0 3] 8 [9 188 0 7] 9 2 10 [6 0 28] 0 2] 7 [0 3] 8 [9 188 0 7] 9 2 10 [6 0 29] 0 2] 0 2]
            7
            [0 3]
            8
            [9 4 0 1.023]
            9
            2
            10
            [6 [7 [0 3] 8 [9 1.515 0 7] 9 2 10 [6 0 28] 0 2] 7 [0 3] 8 [9 1.515 0 7] 9 2 10 [6 0 29] 0 2]
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
      [1.953.718.630 1 7.303.792 [0 7] 0]
      0
      1
    ]
    [ [ 7
        [8 [1 0] [1 5 [1 0] 8 [7 [7 [0 7] 9 4 0 3] 9 1.375 0 1] 9 2 10 [6 [7 [0 3] 1 0] 0 14] 0 2] 0 1]
        11
        [1.953.718.630 1 7.240.051 [0 7] 0]
        0
        1
      ]
      [ [7 [8 [1 0] [1 [8 [9 188 0 7] 9 2 10 [6 0 14] 0 2] 8 [9 1.515 0 7] 9 2 10 [6 0 14] 0 2] 0 1] 11 [1.953.718.630 1 6.581.359 [0 7] 0] 0 1]
        [ 7
          [ 8
            [1 0 0]
            [1 8 [9 759 0 7] 9 2 10 [6 [0 28] 7 [0 3] 8 [9 46 0 7] 9 2 10 [6 [0 29] 7 [0 3] 8 [9 190 0 7] 9 2 10 [6 [0 28] 0 29] 0 2] 0 2] 0 2]
            0
            1
          ]
          11
          [1.953.718.630 1 7.169.394 [0 7] 0]
          0
          1
        ]
        7
        [ 8
          [1 0]
          [ 1
            8
            [7 [7 [0 7] 9 4 0 3] 9 20 0 255]
            9
            2
            10
            [ 6
              [7 [0 3] 8 [7 [7 [0 7] 9 4 0 3] 9 1.375 0 1] 9 2 10 [6 [7 [0 3] 1 0] 0 14] 0 2]
              7
              [0 3]
              8
              [7 [7 [0 7] 9 4 0 3] 9 87 0 1]
              9
              2
              10
              [6 [7 [0 3] 1 0] 0 14]
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
        [1.953.718.630 1 7.561.825 [0 7] 0]
        0
        1
      ]
      [ 7
        [ 8
          [1 0 0]
          [ 1
            6
            [0 12]
            [8 [9 4 0 1.023] 9 2 10 [6 [7 [0 3] 1 2] 0 29] 0 2]
            6
            [5 [1 0] 0 13]
            [1 0]
            4
            8
            [9 4 0 1.023]
            9
            2
            10
            [6 [7 [0 3] 1 2] 7 [0 3] 8 [9 342 0 1.023] 9 2 10 [6 0 29] 0 2]
            0
            2
          ]
          0
          1
        ]
        11
        [1.953.718.630 1 7.824.750 [0 7] 0]
        0
        1
      ]
      7
      [ 8
        [1 0 0]
        [ 1
          8
          [9 4 0 7]
          9
          2
          10
          [ 6
            [0 28]
            7
            [0 3]
            8
            [9 758 0 7]
            9
            2
            10
            [6 [7 [0 3] 6 [8 [9 188 0 7] 9 2 10 [6 0 29] 0 2] [1 1] 1 0] 7 [0 3] 8 [9 1.515 0 7] 9 2 10 [6 0 29] 0 2]
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
      [1.953.718.630 1 6.711.652 [0 7] 0]
      0
      1
    ]
    [ 7
      [ 8
        [1 0 0]
        [ 1
          8
          [9 758 0 7]
          9
          2
          10
          [ 6
            [7 [0 3] 5 [1 0] 8 [9 4 0 63] 9 2 10 [6 [7 [0 3] 8 [9 188 0 7] 9 2 10 [6 0 28] 0 2] 7 [0 3] 8 [9 188 0 7] 9 2 10 [6 0 29] 0 2] 0 2]
            7
            [0 3]
            8
            [9 170 0 1.023]
            9
            2
            10
            [6 [7 [0 3] 8 [9 1.515 0 7] 9 2 10 [6 0 28] 0 2] 7 [0 3] 8 [9 1.515 0 7] 9 2 10 [6 0 29] 0 2]
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
      [1.953.718.630 1 6.386.278 [0 7] 0]
      0
      1
    ]
    7
    [ 8
      [1 0 0]
      [ 1
        6
        [5 [0 12] 0 13]
        [1 0]
        6
        [8 [9 188 0 7] 9 2 10 [6 0 28] 0 2]
        [6 [8 [9 188 0 7] 9 2 10 [6 0 29] 0 2] [6 [8 [9 43 0 1.023] 9 2 10 [6 [0 28] 0 29] 0 2] [1 2] 1 1] 1 2]
        6
        [8 [9 188 0 7] 9 2 10 [6 0 29] 0 2]
        [1 1]
        6
        [8 [9 43 0 1.023] 9 2 10 [6 [0 28] 0 29] 0 2]
        [1 1]
        1
        2
      ]
      0
      1
    ]
    11
    [1.953.718.630 1 7.368.035 [0 7] 0]
    0
    1
  ]
  [ [ [ 1
        [ 7
          [ 8
            [1 0 0]
            [ 1
              8
              [1 0 0]
              8
              [ 1
                6
                [6 [5 [1 0] 0 60] [1 0] 6 [5 [1 0] 0 61] [1 0] 1 1]
                [0 13]
                9
                2
                10
                [30 [8 [9 87 0 31] 9 2 10 [6 [7 [0 3] 1 0] 0 124] 0 2] 8 [9 87 0 31] 9 2 10 [6 [7 [0 3] 1 0] 0 125] 0 2]
                10
                [ 6
                  [4 0 12]
                  8
                  [9 20 0 4.095]
                  9
                  2
                  10
                  [ 6
                    [0 29]
                    7
                    [0 3]
                    8
                    [9 341 0 31]
                    9
                    2
                    10
                    [ 6
                      [7 [0 3] [1 0] 0 12]
                      7
                      [0 3]
                      6
                      [5 [1 0] 8 [9 1.375 0 31] 9 2 10 [6 [7 [0 3] 1 0] 0 124] 0 2]
                      [1 0]
                      6
                      [5 [1 0] 8 [9 1.375 0 31] 9 2 10 [6 [7 [0 3] 1 0] 0 125] 0 2]
                      [1 0]
                      1
                      1
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
          [1.953.718.630 1 7.563.620 [0 7] 0]
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
                  [30 [8 [9 87 0 31] 9 2 10 [6 [7 [0 3] 1 0] 0 124] 0 2] 8 [9 87 0 31] 9 2 10 [6 [7 [0 3] 1 0] 0 125] 0 2]
                  10
                  [ 6
                    [4 0 12]
                    8
                    [9 20 0 4.095]
                    9
                    2
                    10
                    [ 6
                      [0 29]
                      7
                      [0 3]
                      8
                      [9 341 0 31]
                      9
                      2
                      10
                      [ 6
                        [7 [0 3] [1 0] 0 12]
                        7
                        [0 3]
                        6
                        [5 [1 0] 8 [9 1.375 0 31] 9 2 10 [6 [7 [0 3] 1 0] 0 124] 0 2]
                        [6 [5 [1 0] 8 [9 1.375 0 31] 9 2 10 [6 [7 [0 3] 1 0] 0 125] 0 2] [1 0] 1 1]
                        1
                        1
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
            [1.953.718.630 1 7.237.475 [0 7] 0]
            0
            1
          ]
          [ [ 8
              [1 0]
              [ 1
                8
                [ 6
                  [3 0 6]
                  [[8 [7 [0 7] 9 342 0 1] 9 2 10 [6 0 28] 0 2] 8 [7 [0 7] 9 1.374 0 1] 9 2 10 [6 0 29] 0 2]
                  8
                  [7 [0 7] 9 342 0 1]
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
            [ [ 7
                [ 8
                  [1 0 0]
                  [1 6 [5 [1 0] 0 13] [1 0] [8 [9 1.375 0 7] 9 2 10 [6 [0 28] 0 29] 0 2] 9 2 10 [13 8 [9 87 0 7] 9 2 10 [6 [0 28] 0 29] 0 2] 0 1]
                  0
                  1
                ]
                11
                [1.953.718.630 1 7.367.026 [0 7] 0]
                0
                1
              ]
              7
              [ 8
                [1 0 0]
                [1 8 [6 [6 [3 0 12] [1 1] 1 0] [[0 12] 1 1] 0 12] 8 [8 [9 10 0 255] 9 90 10 [6 0 12] 0 2] 9 2 10 [6 [0 13] 0 61] 0 2]
                0
                1
              ]
              11
              [1.953.718.630 1 6.845.292 [0 7] 0]
              0
              1
            ]
            [8 [1 0] [1 8 [6 [6 [3 0 6] [1 1] 1 0] [0 6] 0 0] 8 [5 [0 14] 0 2] 0 6] 0 1]
            [8 [1 0 0] [1 8 [8 [9 10 0 127] 9 190 10 [6 0 28] 0 2] 9 2 10 [6 0 29] 0 2] 0 1]
            [8 [1 0] [1 8 [7 [0 7] 1 1] 8 [5 [0 14] 0 2] 0 6] 0 1]
            7
            [ 8
              [1 0 0]
              [1 8 [6 [6 [3 0 12] [1 1] 1 0] [[0 12] 1 1] 0 12] 8 [8 [9 10 0 255] 9 367 10 [6 0 12] 0 2] 9 2 10 [6 [0 13] 0 61] 0 2]
              0
              1
            ]
            11
            [1.953.718.630 1 6.581.861 [0 7] 0]
            0
            1
          ]
          [ 7
            [ 8
              [1 0 [1 1] 0]
              [ 1
                8
                [8 [9 10 0 127] 9 367 10 [6 0 28] 0 2]
                9
                2
                10
                [6 [0 117] 7 [0 3] 8 [8 [9 10 0 127] 9 767 10 [6 0 28] 0 2] 9 2 10 [6 [0 116] 0 59] 0 2]
                0
                2
              ]
              0
              1
            ]
            11
            [1.953.718.630 1 7.632.227 [0 7] 0]
            0
            1
          ]
          7
          [8 [1 0 0] [1 8 [6 [6 [3 0 12] [1 1] 1 0] [[0 12] 1 1] 0 12] 8 [8 [9 10 0 255] 9 767 10 [6 0 12] 0 2] 9 2 10 [6 [0 13] 0 61] 0 2] 0 1]
          11
          [1.953.718.630 1 6.845.298 [0 7] 0]
          0
          1
        ]
        [ 8
          [1 0]
          [ 1
            [8 [1 0 0] [1 8 [9 44 0 7] 9 2 10 [6 7 [0 3] 8 [9 20 0 4.095] 9 2 10 [6 [0 28] 0 29] 0 2] 0 2] 0 1]
            [8 [9 4 0 127] 9 2 10 [6 7 [0 3] 8 [9 4 0 127] 9 2 10 [6 0 14] 0 2] 0 2]
            [ [8 [1 0] [1 8 [9 1.375 0 31] 9 2 10 [6 [0 62] 0 14] 0 2] 0 1]
              8
              [1 0]
              [ 1
                8
                [9 47 0 4.095]
                9
                2
                10
                [6 [7 [0 3] 8 [9 342 0 4.095] 9 2 10 [6 7 [0 3] 9 10 0 7] 0 2] 7 [0 3] 8 [9 44 0 7] 9 2 10 [6 0 14] 0 2]
                0
                2
              ]
              0
              1
            ]
            [ 8
              [1 0 0 0]
              [ 1
                8
                [8 [9 44 0 7] 9 2 10 [6 0 59] 0 2]
                8
                [8 [9 4 0 1.023] 9 2 10 [6 7 [0 3] 8 [9 47 0 8.191] 9 2 10 [6 [0 126] 0 60] 0 2] 0 2]
                8
                [8 [9 46 0 16.383] 9 2 10 [6 [0 250] 0 6] 0 2]
                8
                [9 44 0 63]
                9
                2
                10
                [ 6
                  7
                  [0 3]
                  8
                  [9 20 0 255]
                  9
                  2
                  10
                  [ 6
                    [7 [0 3] 8 [9 87 0 255] 9 2 10 [6 [7 [0 3] [0 124] 0 2] 0 30] 0 2]
                    7
                    [0 3]
                    8
                    [9 341 0 255]
                    9
                    2
                    10
                    [6 [7 [0 3] [0 124] 8 [9 47 0 32.767] 9 2 10 [6 [0 14] 0 6] 0 2] 0 30]
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
            8
            [1 0]
            [ 1
              7
              [10 [6 8 [9 44 0 7] 9 2 10 [6 0 14] 0 2] 0 1]
              6
              [8 [9 84 0 4.095] 9 2 10 [6 [0 62] 7 [0 3] 1 3] 0 2]
              [0 6]
              8
              [8 [9 342 0 4.095] 9 2 10 [6 0 62] 0 2]
              8
              [9 20 0 63]
              9
              2
              10
              [ 6
                [ 7
                  [0 3]
                  8
                  [9 341 0 63]
                  9
                  2
                  10
                  [6 [0 6] 7 [0 3] 9 2 10 [30 0 2] 10 [6 8 [9 86 0 63] 9 2 10 [6 [0 6] [7 [0 3] 1 0 1] 0 30] 0 2] 0 3]
                  0
                  2
                ]
                7
                [0 3]
                9
                2
                10
                [30 0 2]
                10
                [6 8 [9 86 0 63] 9 2 10 [6 [0 6] [7 [0 3] 1 1 1] 0 30] 0 2]
                0
                3
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
        [ 7
          [ 8
            [1 0 0]
            [ 1
              6
              [5 [1 0] 0 13]
              [1 0]
              8
              [9 20 0 1.023]
              9
              2
              10
              [ 6
                [7 [0 3] 8 [9 1.375 0 7] 9 2 10 [6 [7 [0 3] [0 12] 0 52] 0 117] 0 2]
                7
                [0 3]
                8
                [9 341 0 7]
                9
                2
                10
                [6 [7 [0 3] [0 12] 0 52] 7 [0 3] 9 2 10 [13 0 27] 0 1]
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
          [1.953.718.630 1 7.233.891 [0 7] 0]
          0
          1
        ]
        [ 7
          [ 8
            [1 0 0 [0 15] 0 0 0]
            [1 8 [9 95 0 7] 9 2 10 [6 [0 28] 7 [0 3] 8 [9 22 0 511] 9 2 10 [6 [7 [0 3] 8 [9 340 0 7] 9 2 10 [6 [0 28] 0 58] 0 2] 0 59] 0 2] 0 2]
            0
            1
          ]
          11
          [1.953.718.630 1 7.239.026 [0 7] 0]
          0
          1
        ]
        7
        [ 8
          [1 0 0]
          [ 1
            8
            [6 [6 [3 0 12] [1 1] 1 0] [[0 12] 1 1] 0 12]
            8
            [1 0]
            8
            [ 1
              6
              [5 [1 0] 0 125]
              [1 0]
              8
              [9 20 0 8.191]
              9
              2
              10
              [ 6
                [7 [0 3] 9 2 10 [125 0 251] 10 [6 4 0 6] 0 1]
                7
                [0 3]
                8
                [9 341 0 63]
                9
                2
                10
                [ 6
                  [7 [0 3] [0 28] 8 [9 4 0 8.191] 9 2 10 [6 [0 61] 0 14] 0 2]
                  7
                  [0 3]
                  8
                  [9 1.375 0 63]
                  9
                  2
                  10
                  [6 [7 [0 3] [0 28] 0 29] 0 506]
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
          1
        ]
        11
        [1.953.718.630 1 7.366.002 [0 7] 0]
        0
        1
      ]
      0
      1
    ]
    [ 7
      [8 [1 0 0] [1 8 [9 22 0 7] 9 2 10 [6 7 [0 3] 8 [9 4 0 31] 9 2 10 [6 [0 28] 7 [0 3] 8 [9 22 0 7] 9 2 10 [6 0 29] 0 2] 0 2] 0 2] 0 1]
      11
      [1.953.718.630 1 1.935.763.571 [0 7] 0]
      0
      1
    ]
    [ 7
      [ 8
        [1 0]
        [1 8 [7 [7 [0 1] 9 4 0 7] 9 46 0 3] 9 2 10 [6 7 [0 3] [8 [7 [7 [0 1] 9 4 0 7] 9 686 0 1] 9 2 10 [6 [7 [0 3] 1 3] 0 14] 0 2] 0 6] 0 2]
        0
        1
      ]
      11
      [1.953.718.630 1 2.019.649.651 [0 7] 0]
      0
      1
    ]
    [ 7
      [ 8
        [1 0 0]
        [ 1
          7
          [10 [13 8 [7 [7 [0 1] 9 4 0 7] 9 86 0 1] 9 2 10 [6 [7 [0 3] 1 3] [7 [0 3] [1 0] 0 12] 0 29] 0 2] 0 1]
          8
          [ [7 [7 [7 [0 1] 9 4 0 7] 9 22 0 1] 10 [6 1 5] 0 1]
            8
            [1 0 0]
            [1 8 [7 [7 [0 7] 9 4 0 7] 9 86 0 1] 9 2 10 [6 [7 [0 3] 1 5] [7 [0 3] [0 12] 1 1] 0 29] 0 2]
            0
            1
          ]
          8
          [[9 4 0 4] [9 46 0 4] [9 47 0 4] 9 45 0 4]
          8
          [8 [7 [7 [0 7] 9 4 0 7] 9 341 0 1] 9 2 10 [6 [7 [0 3] 1 0 3] 0 124] 0 2]
          8
          [ 8
            [7 [7 [0 15] 9 4 0 7] 9 46 0 1]
            9
            2
            10
            [ 6
              [7 [0 3] 1 0]
              7
              [0 3]
              [[0 2] 0 125]
              [1 8 128]
              [ [ 8
                  [7 [7 [0 15] 9 4 0 7] 9 46 0 255]
                  9
                  2
                  10
                  [ 6
                    [ 7
                      [0 3]
                      8
                      [7 [7 [0 15] 9 4 0 7] 9 47 0 255]
                      9
                      2
                      10
                      [ 6
                        [7 [0 3] 1 960]
                        7
                        [0 3]
                        8
                        [7 [7 [0 15] 9 4 0 7] 9 46 0 255]
                        9
                        2
                        10
                        [6 [7 [0 3] 8 [7 [7 [0 15] 9 4 0 7] 9 20 0 255] 9 2 10 [6 [7 [0 3] 1 8] 0 6] 0 2] 7 [0 3] 1 512]
                        0
                        2
                      ]
                      0
                      2
                    ]
                    7
                    [0 3]
                    1
                    512
                  ]
                  0
                  2
                ]
                1
                0
              ]
              [[1 64] 8 [8 [7 [7 [0 15] 9 4 0 7] 9 22 0 1] 9 47 10 [6 7 [0 3] 1 6] 0 2] 9 2 10 [6 0 6] 0 2]
              1
              0
            ]
            0
            2
          ]
          8
          [8 [7 [7 [0 31] 9 4 0 7] 9 686 0 1] 9 2 10 [6 [7 [0 3] 1 9] 0 6] 0 2]
          8
          [ 1
            25.051.139.735.836.467.913.601.071.899.189.108.501.681.633.092.613.316.132.753.366.958.947.502.841.281.110.980.347.811.086.624.969.305.314.199.562.795.868.290.539.880.502.533.646.505.489.797.110.349.007.385.020.507.326.982.043.050.618.112.420.254.613.810.441.709.594.605.123.090.411.975.756.494.285.771.699.200.369.901.479.195.251.226.368.983.824.020.564.277.645.963.860.728.452.821.576.845.901.498.668.417.244.438.721.537.663.670.541.944.820.957.180.957.595.559.282.976.806.173.113.161.068.298.822.071.065.329.290.006.052.849.814.285.001.949.914.564.097.058.408.480.133.985.233.335.799.884.203.712.730.341.384.999.677.089.997.083.749.077.591.931.498.939.520.449.886.954.646.413.138.343.858.395.935.213.418.018.409.268.340.744.776.361.518.554.939.863.400.075.967.197.509.182.087.778.881.547.827.184.266.701.615.699.472.280
          ]
          8
          [1 41.557.658.498.906.279.274.860.226.408.925.318.911.382.702.236.748.615.442.085.426.012.007.055.353.447]
          8
          [1 0]
          8
          [ 1
            6
            [5 [0 6] 0 62]
            [8 [7 [7 [0 1.023] 9 4 0 7] 9 94 0 1] 9 2 10 [6 [7 [0 3] 1 5] [0 30] 0 8.182] 0 2]
            8
            [ 8
              [8 [7 [7 [0 1.023] 9 4 0 7] 9 86 0 1] 9 2 10 [6 [7 [0 3] 1 9] [7 [0 3] [0 6] 1 1] 0 254] 0 2]
              8
              [8 [7 [7 [0 2.047] 9 4 0 7] 9 94 0 1] 9 2 10 [6 [7 [0 3] 1 5] [0 6] 0 16.374] 0 2]
              8
              [1 16]
              8
              [ 1
                6
                [5 [1 64] 0 6]
                [0 14]
                8
                [ [8 [0 32.765] 9 2 10 [6 [7 [0 3] 8 [7 [7 [0 16.383] 9 4 0 7] 9 47 0 255] 9 2 10 [6 [0 14] 7 [0 3] 1 15] 0 2] 0 30] 0 2]
                  [8 [0 32.765] 9 2 10 [6 [7 [0 3] 8 [7 [7 [0 16.383] 9 4 0 7] 9 47 0 255] 9 2 10 [6 [0 14] 7 [0 3] 1 2] 0 2] 0 30] 0 2]
                  [8 [0 32.765] 9 2 10 [6 [7 [0 3] 8 [7 [7 [0 16.383] 9 4 0 7] 9 47 0 255] 9 2 10 [6 [0 14] 7 [0 3] 1 16] 0 2] 0 30] 0 2]
                  8
                  [0 32.765]
                  9
                  2
                  10
                  [6 [7 [0 3] 8 [7 [7 [0 16.383] 9 4 0 7] 9 47 0 255] 9 2 10 [6 [0 14] 7 [0 3] 1 7] 0 2] 0 30]
                  0
                  2
                ]
                8
                [ 8
                  [7 [7 [0 32.767] 9 4 0 7] 9 4 0 15]
                  8
                  [0 2]
                  9
                  2
                  10
                  [ 6
                    [7 [0 3] 7 [0 3] 8 [0 65.530] 9 2 10 [6 [7 [0 3] 1 0] [7 [0 3] 1 7] 0 12] 0 2]
                    7
                    [0 3]
                    8
                    [0 2]
                    9
                    2
                    10
                    [ 6
                      [7 [0 3] 7 [0 3] 8 [0 65.530] 9 2 10 [6 [7 [0 3] 1 0] [7 [0 3] 1 18] 0 12] 0 2]
                      7
                      [0 3]
                      7
                      [0 3]
                      8
                      [7 [7 [0 32.767] 9 4 0 7] 9 87 0 1]
                      9
                      2
                      10
                      [6 [7 [0 3] 1 0 3] 0 12]
                      0
                      2
                    ]
                    0
                    2
                  ]
                  0
                  2
                ]
                8
                [ 8
                  [7 [7 [0 65.535] 9 4 0 7] 9 4 0 15]
                  8
                  [0 2]
                  9
                  2
                  10
                  [ 6
                    [7 [0 3] 7 [0 3] 8 [0 131.066] 9 2 10 [6 [7 [0 3] 1 0] [7 [0 3] 1 17] 0 58] 0 2]
                    7
                    [0 3]
                    8
                    [0 2]
                    9
                    2
                    10
                    [ 6
                      [7 [0 3] 7 [0 3] 8 [0 131.066] 9 2 10 [6 [7 [0 3] 1 0] [7 [0 3] 1 19] 0 58] 0 2]
                      7
                      [0 3]
                      7
                      [0 3]
                      8
                      [7 [7 [0 65.535] 9 4 0 7] 9 87 0 1]
                      9
                      2
                      10
                      [6 [7 [0 3] 1 0 10] 0 58]
                      0
                      2
                    ]
                    0
                    2
                  ]
                  0
                  2
                ]
                8
                [8 [0 131.068] 8 [0 2] 9 2 10 [6 [0 502] 7 [0 3] 8 [0 2] 9 2 10 [6 [0 30] 7 [0 3] 8 [0 2] 9 2 10 [6 [0 503] 0 14] 0 2] 0 2] 0 2]
                9
                2
                10
                [ 14
                  8
                  [7 [7 [0 262.143] 9 4 0 7] 9 20 0 1]
                  9
                  2
                  10
                  [6 [7 [0 3] 8 [7 [7 [0 262.143] 9 4 0 7] 9 341 0 1] 9 2 10 [6 [7 [0 3] [1 5] 0 126] 0 6] 0 2] 0 510]
                  0
                  2
                ]
                10
                [6 4 0 126]
                0
                31
              ]
              9
              2
              0
              1
            ]
            8
            [1 0]
            8
            [ [8 [0 8.189] 9 2 10 [6 [7 [0 3] 1 0] 0 126] 0 2]
              [8 [0 8.189] 9 2 10 [6 [7 [0 3] 1 1] 0 126] 0 2]
              [8 [0 8.189] 9 2 10 [6 [7 [0 3] 1 2] 0 126] 0 2]
              [8 [0 8.189] 9 2 10 [6 [7 [0 3] 1 3] 0 126] 0 2]
              [8 [0 8.189] 9 2 10 [6 [7 [0 3] 1 4] 0 126] 0 2]
              [8 [0 8.189] 9 2 10 [6 [7 [0 3] 1 5] 0 126] 0 2]
              [8 [0 8.189] 9 2 10 [6 [7 [0 3] 1 6] 0 126] 0 2]
              8
              [0 8.189]
              9
              2
              10
              [6 [7 [0 3] 1 7] 0 126]
              0
              2
            ]
            8
            [ 1
              6
              [5 [1 64] 0 14]
              [ 9
                2
                10
                [ 14
                  8
                  [7 [7 [0 16.383] 9 4 0 7] 9 95 0 1]
                  9
                  2
                  10
                  [ 6
                    [7 [0 3] 1 5]
                    7
                    [0 3]
                    [8 [0 16.380] 9 2 10 [6 [0 28] 7 [0 3] 8 [0 32.765] 9 2 10 [6 [7 [0 3] 1 0] 0 510] 0 2] 0 2]
                    [8 [0 16.380] 9 2 10 [6 [0 58] 7 [0 3] 8 [0 32.765] 9 2 10 [6 [7 [0 3] 1 1] 0 510] 0 2] 0 2]
                    [8 [0 16.380] 9 2 10 [6 [0 118] 7 [0 3] 8 [0 32.765] 9 2 10 [6 [7 [0 3] 1 2] 0 510] 0 2] 0 2]
                    [8 [0 16.380] 9 2 10 [6 [0 238] 7 [0 3] 8 [0 32.765] 9 2 10 [6 [7 [0 3] 1 3] 0 510] 0 2] 0 2]
                    [8 [0 16.380] 9 2 10 [6 [0 478] 7 [0 3] 8 [0 32.765] 9 2 10 [6 [7 [0 3] 1 4] 0 510] 0 2] 0 2]
                    [8 [0 16.380] 9 2 10 [6 [0 958] 7 [0 3] 8 [0 32.765] 9 2 10 [6 [7 [0 3] 1 5] 0 510] 0 2] 0 2]
                    [8 [0 16.380] 9 2 10 [6 [0 1.918] 7 [0 3] 8 [0 32.765] 9 2 10 [6 [7 [0 3] 1 6] 0 510] 0 2] 0 2]
                    [8 [0 16.380] 9 2 10 [6 [0 1.919] 7 [0 3] 8 [0 32.765] 9 2 10 [6 [7 [0 3] 1 7] 0 510] 0 2] 0 2]
                    1
                    0
                  ]
                  0
                  2
                ]
                10
                [6 4 0 126]
                0
                31
              ]
              8
              [ 8
                [7 [7 [0 16.383] 9 4 0 7] 9 4 0 15]
                8
                [0 2]
                9
                2
                10
                [ 6
                  [7 [0 3] 7 [0 3] 8 [0 32.762] 9 2 10 [6 [7 [0 3] 1 0] [7 [0 3] 1 2] 0 28] 0 2]
                  7
                  [0 3]
                  8
                  [0 2]
                  9
                  2
                  10
                  [ 6
                    [7 [0 3] 7 [0 3] 8 [0 32.762] 9 2 10 [6 [7 [0 3] 1 0] [7 [0 3] 1 13] 0 28] 0 2]
                    7
                    [0 3]
                    7
                    [0 3]
                    8
                    [0 32.762]
                    9
                    2
                    10
                    [6 [7 [0 3] 1 0] [7 [0 3] 1 22] 0 28]
                    0
                    2
                  ]
                  0
                  2
                ]
                0
                2
              ]
              8
              [ 8
                [7 [7 [0 32.767] 9 4 0 7] 9 4 0 15]
                8
                [0 2]
                9
                2
                10
                [ 6
                  [7 [0 3] 7 [0 3] 8 [7 [7 [0 32.767] 9 4 0 7] 9 4 0 1] 9 2 10 [6 [0 60] 0 122] 0 2]
                  7
                  [0 3]
                  8
                  [0 2]
                  9
                  2
                  10
                  [ 6
                    [7 [0 3] 7 [0 3] 8 [7 [7 [0 32.767] 9 4 0 7] 9 4 0 1] 9 2 10 [6 [0 60] 0 246] 0 2]
                    7
                    [0 3]
                    7
                    [0 3]
                    8
                    [7 [7 [0 32.767] 9 4 0 7] 9 4 0 1]
                    9
                    2
                    10
                    [6 [0 122] 0 246]
                    0
                    2
                  ]
                  0
                  2
                ]
                0
                2
              ]
              8
              [8 [0 65.532] 9 2 10 [6 [0 14] 0 6] 0 2]
              8
              [ 8
                [7 [7 [0 131.071] 9 4 0 7] 9 4 0 15]
                8
                [0 2]
                9
                2
                10
                [ 6
                  [7 [0 3] 7 [0 3] 8 [0 262.138] 9 2 10 [6 [7 [0 3] 1 0] [7 [0 3] 1 6] 0 4.062] 0 2]
                  7
                  [0 3]
                  8
                  [0 2]
                  9
                  2
                  10
                  [ 6
                    [7 [0 3] 7 [0 3] 8 [0 262.138] 9 2 10 [6 [7 [0 3] 1 0] [7 [0 3] 1 11] 0 4.062] 0 2]
                    7
                    [0 3]
                    7
                    [0 3]
                    8
                    [0 262.138]
                    9
                    2
                    10
                    [6 [7 [0 3] 1 0] [7 [0 3] 1 25] 0 4.062]
                    0
                    2
                  ]
                  0
                  2
                ]
                0
                2
              ]
              8
              [ 8
                [7 [7 [0 262.143] 9 4 0 7] 9 4 0 15]
                9
                2
                10
                [ 6
                  [7 [0 3] 8 [7 [7 [0 262.143] 9 4 0 7] 9 4 0 1] 9 2 10 [6 [0 8.158] 0 16.318] 0 2]
                  7
                  [0 3]
                  8
                  [7 [7 [0 262.143] 9 4 0 7] 9 4 0 1]
                  9
                  2
                  10
                  [6 [7 [0 3] 8 [0 1.048.567] 9 2 10 [6 0 8.158] 0 2] 0 32.638]
                  0
                  2
                ]
                0
                2
              ]
              8
              [ 8
                [0 524.284]
                8
                [0 2]
                9
                2
                10
                [ 6
                  [0 130.943]
                  7
                  [0 3]
                  8
                  [0 2]
                  9
                  2
                  10
                  [ 6
                    [0 30]
                    7
                    [0 3]
                    8
                    [0 2]
                    9
                    2
                    10
                    [ 6
                      [0 14]
                      7
                      [0 3]
                      8
                      [0 2]
                      9
                      2
                      10
                      [ 6
                        [7 [0 3] 7 [0 3] 8 [0 1.048.573] 9 2 10 [6 [0 1.022] 0 32.766] 0 2]
                        7
                        [0 3]
                        7
                        [0 3]
                        8
                        [0 1.048.573]
                        9
                        2
                        10
                        [6 [0 1.022] 0 2.046]
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
                  2
                ]
                0
                2
              ]
              9
              2
              10
              [14 4 0 1.022]
              10
              [ 6
                [8 [0 1.048.572] 9 2 10 [6 [0 6] 0 62] 0 2]
                [0 1.020]
                [0 2.042]
                [0 4.086]
                [8 [0 1.048.572] 9 2 10 [6 [0 16.366] 0 6] 0 2]
                [0 16.350]
                [0 32.702]
                0
                65.406
              ]
              0
              127
            ]
            9
            2
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
      [1.953.718.630 1 2.036.426.867 [0 7] 0]
      0
      1
    ]
    7
    [ 8
      [1 0]
      [ 1
        [ 8
          [1 0]
          [ 1
            8
            [8 [9 23 0 7] 9 2 10 [6 0 14] 0 2]
            [0 2]
            10
            [6 8 [9 10 0 63] 9 2 10 [6 [7 [0 3] 1 1.932.355.439] 7 [0 3] 8 [9 4 0 255] 9 2 10 [6 [0 126] 0 6] 0 2] 0 2]
            0
            15
          ]
          0
          1
        ]
        [ [ 8
            [1 0]
            [ 1
              11
              [1.851.876.717 [1 [1 1.717.658.988] 7 [0 1] 8 [1 1 114 97 100 45 122 101 114 111 0] 9 2 0 1] 0 1]
              6
              [5 [1 0] 0 6]
              [0 0]
              8
              [8 [9 23 0 7] 9 2 10 [6 7 [0 3] 8 [7 [7 [0 1] 9 4 0 31] 9 686 0 1] 9 2 10 [6 [7 [0 3] 1 0] 0 14] 0 2] 0 2]
              6
              [8 [7 [7 [0 3] 9 4 0 31] 9 343 0 255] 9 2 10 [6 [0 6] 0 30] 0 2]
              [0 2]
              9
              2
              10
              [30 4 0 62]
              0
              3
            ]
            0
            1
          ]
          [10 [6 8 [9 10 0 7] 9 2 10 [6 [7 [0 3] 1 30.449.275.492.921.459] 0 14] 0 2] 0 1]
          10
          [6 8 [9 10 0 7] 9 2 10 [6 [7 [0 3] 1 32.138.125.353.185.395] 0 14] 0 2]
          0
          1
        ]
        [ 8
          [1 0]
          [ 1
            8
            [8 [9 20 0 7] 9 2 10 [6 0 14] 0 2]
            [0 2]
            10
            [6 8 [9 10 0 63] 9 2 10 [6 [7 [0 3] 1 1.932.355.439] 7 [0 3] 8 [9 4 0 255] 9 2 10 [6 [0 126] 0 6] 0 2] 0 2]
            0
            15
          ]
          0
          1
        ]
        7
        [ 8
          [1 0]
          [ 1
            8
            [7 [7 [0 1] 9 4 0 31] 9 46 0 1]
            9
            2
            10
            [ 6
              [7 [0 3] 1 0]
              7
              [0 3]
              8
              [ 8
                [7 [7 [0 1] 9 4 0 31] 9 10 0 3]
                9
                2
                10
                [6 [7 [0 3] 1 1.630.365.551] 7 [0 3] 8 [7 [7 [0 1] 9 4 0 31] 9 4 0 15] 9 2 10 [6 [0 14] 0 62] 0 2]
                0
                2
              ]
              8
              [ 1
                6
                [5 [1 0] 0 30]
                [1 0]
                8
                [ 8
                  [7 [7 [0 7] 9 4 0 31] 9 10 0 3]
                  9
                  2
                  10
                  [ 6
                    [7 [0 3] 1 1.647.142.767]
                    7
                    [0 3]
                    8
                    [7 [7 [0 7] 9 4 0 31] 9 4 0 15]
                    9
                    2
                    10
                    [6 [0 62] 7 [0 3] 8 [7 [7 [0 7] 9 4 0 31] 9 4 0 15] 9 2 10 [6 [0 254] 0 14] 0 2]
                    0
                    2
                  ]
                  0
                  2
                ]
                6
                [8 [7 [7 [0 15] 9 4 0 31] 9 343 0 255] 9 2 10 [6 [0 126] 7 [0 3] 1 256] 0 2]
                [[[0 62] 8 [7 [7 [0 15] 9 4 0 31] 9 1.375 0 1] 9 2 10 [6 [7 [0 3] [1 0] 0 62] 0 6] 0 2] 1 0]
                [[1 256] 0 2]
                9
                2
                10
                [30 8 [7 [7 [0 15] 9 4 0 31] 9 47 0 255] 9 2 10 [6 [0 126] 7 [0 3] 1 256] 0 2]
                10
                [6 0 2]
                0
                3
              ]
              9
              2
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
        [1.953.718.630 1 7.823.730 [0 7] 0]
        0
        1
      ]
      0
      1
    ]
    11
    [1.953.718.630 1 26.479 [0 7] 0]
    0
    1
  ]
  [ [7 [8 [1 0 0] [1 8 [1 133.480.762.729.846] 0 0] 0 1] 11 [1.953.718.630 1 133.480.762.729.846 [0 7] 0] 0 1]
    [7 [8 [1 0 0] [1 8 [1 1.852.270.963] 0 0] 0 1] 11 [1.953.718.630 1 1.852.270.963 [0 7] 0] 0 1]
    [ 7
      [8 [1 0 0 0] [1 8 [1 133.449.338.811.408.550.175.386.169.016.308.622.710] 0 0] 0 1]
      11
      [1.953.718.630 1 133.449.338.811.408.550.175.386.169.016.308.622.710 [0 7] 0]
      0
      1
    ]
    7
    [8 [1 0 0] [1 8 [1 2.036.275.311.453.377.535.635.164.748.015.987] 0 0] 0 1]
    11
    [1.953.718.630 1 2.036.275.311.453.377.535.635.164.748.015.987 [0 7] 0]
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
              [8 [8 [9 10 0 63] 9 767 10 [6 7 [0 3] 1 0] 0 2] 9 2 10 [6 [7 [0 3] 1 1] 0 124] 0 2]
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
                  [8 [8 [9 10 0 63] 9 367 10 [6 7 [0 3] 1 0] 0 2] 9 2 10 [6 [7 [0 3] 1 1] 0 124] 0 2]
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
        [[8 [1 0] [1 8 [0 6] 8 [5 [0 14] 0 2] 0 6] 0 1] 8 [1 0] [1 8 [0 6] 8 [5 [0 14] 0 2] 0 6] 0 1]
        [1 8 [[0 50] 0 54] [1 8 [[8 [0 60] 9 2 10 [6 0 28] 0 2] 8 [0 61] 9 2 10 [6 0 29] 0 2] 8 [5 [0 14] 0 2] 0 6] 0 1]
        0
        1
      ]
      [ 8
        [[8 [1 0] [1 8 [0 6] 8 [5 [0 14] 0 2] 0 6] 0 1] 8 [1 0] [1 8 [0 6] 8 [5 [0 14] 0 2] 0 6] 0 1]
        [1 8 [9 46 0 7] 9 2 10 [6 7 [0 3] 8 [9 20 0 7] 9 2 10 [6 [0 28] 0 29] 0 2] 0 2]
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
          [8 [8 [9 10 0 15] 9 190 10 [6 7 [0 3] 1 0] 0 2] 9 2 10 [6 0 14] 0 2]
          8
          [8 [8 [9 10 0 31] 9 190 10 [6 7 [0 3] 1 0] 0 2] 9 2 10 [6 0 6] 0 2]
          [8 [9 20 0 511] 9 2 10 [6 [7 [0 3] 8 [9 20 0 511] 9 2 10 [6 [0 6] 0 6] 0 2] 0 14] 0 2]
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
              [7 [0 3] 8 [8 [9 10 0 63] 9 367 10 [6 7 [0 3] 1 0] 0 2] 9 2 10 [6 [7 [0 3] 8 [9 342 0 511] 9 2 10 [6 0 6] 0 2] 0 14] 0 2]
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
              [10 [6 8 [9 20 0 511] 9 2 10 [6 [7 [0 3] 1 2] 0 14] 0 2] 0 1]
              8
              [9 2 10 [30 0 60] 0 1]
              8
              [9 2 10 [30 0 125] 10 [6 8 [9 20 0 1.023] 9 2 10 [6 [0 30] 0 12] 0 2] 0 3]
              [8 [9 20 0 2.047] 9 2 10 [6 [7 [0 3] 1 2] 7 [0 3] 8 [9 20 0 2.047] 9 2 10 [6 [0 28] 0 12] 0 2] 0 2]
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
                [6 [7 [0 3] 1 2] 7 [0 3] 8 [8 [9 10 0 255] 9 4 10 [6 7 [0 3] 1 0] 0 2] 9 2 10 [6 [0 29] 0 13] 0 2]
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
              [8 [7 [0 7] 8 [9 46 0 7] 9 2 10 [6 0 14] 0 2] 9 2 10 [6 0 58] 0 2]
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
              [5 [1 0] 8 [8 [9 10 0 63] 9 94 10 [6 7 [0 3] 1 0] 0 2] 9 2 10 [6 [7 [0 3] [0 6] 1 1] 0 62] 0 2]
              [8 [8 [9 95 0 31] 9 2 10 [6 [7 [0 3] 4 0 6] 0 62] 0 2] [4 0 4] 0 5]
              8
              [8 [9 20 0 511] 9 2 10 [6 [7 [0 3] 1 2] 0 14] 0 2]
              6
              [5 [1 0] 8 [8 [9 10 0 127] 9 94 10 [6 7 [0 3] 1 0] 0 2] 9 2 10 [6 [7 [0 3] [4 0 14] 1 1] 0 126] 0 2]
              [ 8
                [9 2 10 [6 0 2] 0 3]
                8
                [9 2 10 [6 8 [9 20 0 2.047] 9 2 10 [6 [0 12] 0 14] 0 2] 0 7]
                8
                [[0 13] 0 5]
                [8 [9 20 0 8.191] 9 2 10 [6 [7 [0 3] 1 2] 7 [0 3] 8 [9 20 0 8.191] 9 2 10 [6 [0 60] 0 28] 0 2] 0 2]
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
          [[1 0] 8 [8 [9 10 0 15] 9 190 10 [6 7 [0 3] 1 0] 0 2] 9 2 10 [6 0 29] 0 2]
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
              [6 [7 [0 3] [8 [9 20 0 511] 9 2 10 [6 [0 124] 0 28] 0 2] 1 1] 0 125]
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
            [7 [0 3] 8 [9 4 0 63] 9 2 10 [6 7 [0 3] 8 [9 342 0 511] 9 2 10 [6 0 14] 0 2] 0 2]
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
        [8 [9 20 0 1.023] 9 2 10 [6 [7 [0 3] 8 [9 20 0 1.023] 9 2 10 [6 [0 30] 0 30] 0 2] 0 6] 0 2]
        8
        [8 [9 10 0 127] 9 94 10 [6 7 [0 3] 1 0] 0 2]
        9
        2
        10
        [6 [7 [0 3] [8 [9 20 0 1.023] 9 2 10 [6 [0 14] 7 [0 3] 8 [9 342 0 1.023] 9 2 10 [6 0 30] 0 2] 0 2] 0 2] 0 253]
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
  [ [8 [1 0] [1 6 [5 [1 0] 0 6] [1 1] 8 [9 4 0 63] 9 2 10 [6 [7 [0 3] 1 2] 7 [0 3] 9 2 10 [6 8 [9 342 0 63] 9 2 10 [6 0 14] 0 2] 0 1] 0 2] 0 1]
    [ 8
      [1 0]
      [ 1
        [ 8
          [1 0 0]
          [1 8 [9 20 0 255] 9 2 10 [6 [7 [0 3] 8 [9 90 0 7] 9 2 10 [6 [7 [0 3] 8 [9 190 0 7] 9 2 10 [6 0 28] 0 2] 0 29] 0 2] 0 28] 0 2]
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
                [6 8 [9 20 0 2.047] 9 2 10 [6 [0 253] 7 [0 3] 8 [9 90 0 63] 9 2 10 [6 [7 [0 3] 1 1] 0 14] 0 2] 0 2]
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
              [6 [0 29] 7 [0 3] 8 [9 4 0 31] 9 2 10 [6 7 [0 3] 8 [9 4 0 255] 9 2 10 [6 [7 [0 3] 9 182 0 7] 0 28] 0 2] 0 2]
              0
              2
            ]
            0
            1
          ]
          [8 [9 4 0 7] 9 2 10 [6 0 14] 0 2]
          [ 8
            [1 0]
            [1 8 [9 47 0 255] 9 2 10 [6 [7 [0 3] 8 [9 342 0 255] 9 2 10 [6 7 [0 3] 9 382 0 7] 0 2] 7 [0 3] 8 [9 10 0 7] 9 2 10 [6 0 14] 0 2] 0 2]
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
            [6 [0 29] 7 [0 3] 8 [9 4 0 31] 9 2 10 [6 7 [0 3] 8 [9 4 0 255] 9 2 10 [6 [7 [0 3] 9 182 0 7] 0 28] 0 2] 0 2]
            0
            2
          ]
          0
          1
        ]
        [8 [1 0 0] [1 8 [9 10 0 7] 9 2 10 [6 7 [0 3] 8 [9 20 0 255] 9 2 10 [6 [0 28] 0 29] 0 2] 0 2] 0 1]
        [8 [1 [0 0] 0] [1 8 [9 367 0 7] 9 2 10 [6 [0 57] 7 [0 3] 8 [9 767 0 7] 9 2 10 [6 [0 56] 0 29] 0 2] 0 2] 0 1]
        [ 8
          [1 0]
          [1 8 [1 0] 8 [1 6 [5 [1 0] 0 30] [0 6] 9 2 10 [30 8 [9 767 0 31] 9 2 10 [6 [7 [0 3] 1 1] 0 62] 0 2] 10 [6 4 0 6] 0 1] 9 2 0 1]
          0
          1
        ]
        [8 [9 4 0 7] 9 2 10 [6 7 [0 3] 8 [9 4 0 7] 9 2 10 [6 0 14] 0 2] 0 2]
        [8 [1 0] [1 8 [9 46 0 7] 9 2 10 [6 [7 [0 3] 8 [9 366 0 7] 9 2 10 [6 0 14] 0 2] 7 [0 3] 1 1] 0 2] 0 1]
        8
        [1 0 0]
        [ 1
          8
          [9 170 0 255]
          9
          2
          10
          [6 [0 29] 7 [0 3] 8 [9 4 0 31] 9 2 10 [6 7 [0 3] 8 [9 4 0 255] 9 2 10 [6 [7 [0 3] 9 182 0 7] 0 28] 0 2] 0 2]
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
      [8 [1 0 0] [1 6 [5 [1 0] 0 13] [1 1] 8 [9 4 0 31] 9 2 10 [6 [0 28] 7 [0 3] 9 2 10 [13 8 [9 342 0 31] 9 2 10 [6 0 29] 0 2] 0 1] 0 2] 0 1]
      11
      [1.953.718.630 1 7.827.312 [0 7] 0]
      0
      1
    ]
    8
    [1 0]
    [ 1
      [8 [1 1 1] [1 8 [9 42 0 7] 9 2 10 [6 7 [0 3] 8 [9 4 0 127] 9 2 10 [6 [0 28] 0 29] 0 2] 0 2] 0 1]
      [ [8 [1 0 0] [1 8 [9 42 0 7] 9 2 10 [6 7 [0 3] 8 [9 20 0 127] 9 2 10 [6 [0 28] 0 29] 0 2] 0 2] 0 1]
        [8 [1 0] [1 8 [9 46 0 127] 9 2 10 [6 [0 14] 0 62] 0 2] 0 1]
        [8 [1 1 1] [1 8 [9 4 0 7] 9 2 10 [6 [0 28] 7 [0 3] 8 [9 174 0 7] 9 2 10 [6 0 29] 0 2] 0 2] 0 1]
        [8 [1 0] [1 0 0] 0 1]
        8
        [1 0]
        [1 8 [9 47 0 127] 9 2 10 [6 [0 62] 7 [0 3] 8 [9 42 0 7] 9 2 10 [6 0 14] 0 2] 0 2]
        0
        1
      ]
      [8 [1 0 0] [1 5 [8 [9 42 0 7] 9 2 10 [6 0 28] 0 2] 8 [9 42 0 7] 9 2 10 [6 0 29] 0 2] 0 1]
      [8 [1 0 0] [1 8 [9 42 0 7] 9 2 10 [6 7 [0 3] 8 [9 4 0 31] 9 2 10 [6 [0 28] 0 29] 0 2] 0 2] 0 1]
      8
      [1 0 0]
      [ 1
        8
        [9 42 0 7]
        9
        2
        10
        [6 7 [0 3] 8 [9 47 0 127] 9 2 10 [6 [7 [0 3] 8 [9 20 0 127] 9 2 10 [6 [0 62] 0 28] 0 2] 7 [0 3] 8 [9 42 0 7] 9 2 10 [6 0 29] 0 2] 0 2]
        0
        2
      ]
      0
      1
    ]
    0
    1
  ]
  [ [8 [1 [[0 15] [0 0] [0 0] 0] 0] [1 8 [0 101] [1 8 [0 60] 9 2 10 [6 [0 125] 0 14] 0 2] 0 1] 0 1]
    [ [8 [8 [1 0] [1 8 [0 6] 8 [5 [0 14] 0 2] 0 6] 0 1] [1 8 [1 0] [1 8 [7 [0 7] [1 7 [0 14] 9 2 0 1] 0 1] 8 [5 [0 14] 0 2] 0 6] 0 1] 0 1]
      [ 8
        [8 [1 0] [1 8 [0 6] 8 [5 [0 14] 0 2] 0 6] 0 1]
        [ 1
          8
          [1 0]
          [1 8 [6 [3 0 6] [[6 [5 [1 0] 0 12] [1 0] 0 0] 8 [0 30] 9 2 10 [6 0 29] 0 2] 6 [5 [1 0] 0 6] [1 0] 0 0] 8 [5 [0 14] 0 2] 0 6]
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
          [1 8 [[8 [0 30] 9 2 10 [6 0 28] 0 2] 8 [7 [0 7] 8 [9 47 0 7] 9 2 10 [6 0 14] 0 2] 9 2 10 [6 0 29] 0 2] 8 [5 [0 14] 0 2] 0 6]
          0
          1
        ]
        0
        1
      ]
      [8 [1 0] [1 8 [7 [1 0 0] 8 [0 2] [1 0 15] 0 1] 8 [5 [0 14] 0 2] 0 6] 0 1]
      [ 8
        [[8 [1 0] [1 8 [0 6] 8 [5 [0 14] 0 2] 0 6] 0 1] 8 [1 0] [1 8 [0 6] 8 [5 [0 14] 0 2] 0 6] 0 1]
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
                [8 [7 [0 7] 8 [9 702 0 7] 9 2 10 [6 0 14] 0 2] 9 2 10 [6 0 58] 0 2]
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
      [1 8 [8 [1 0] [1 8 [0 6] 8 [5 [0 14] 0 2] 0 6] 0 1] 8 [5 [0 14] 0 2] 0 6]
      0
      1
    ]
    [8 [1 0 [0 15] 0 0 0] [1 8 [1 6 [5 [1 0] 0 28] [1 0] [8 [0 29] 9 2 10 [6 0 120] 0 2] 9 2 10 [28 0 57] 0 1] 9 2 0 1] 0 1]
    [8 [1 0 [0 13] [0 0] 0] [1 8 [1 6 [5 [1 0] 0 28] [0 237] 8 [0 29] 9 2 10 [6 [0 120] 7 [0 3] 9 2 10 [28 0 57] 0 1] 0 2] 9 2 0 1] 0 1]
    8
    [8 [1 0] [1 8 [0 6] 8 [5 [0 14] 0 2] 0 6] 0 1]
    [ 1
      8
      [1 0]
      [ 1
        8
        [6 [3 0 6] [[8 [0 30] 9 2 10 [6 0 28] 0 2] 8 [7 [0 7] 8 [9 47 0 7] 9 2 10 [6 0 14] 0 2] 9 2 10 [6 0 29] 0 2] 6 [5 [1 0] 0 6] [1 0] 0 0]
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
          [1 6 [5 [1 0] 0 60] [0 6] 9 2 10 [60 8 [9 342 0 31] 9 2 10 [6 0 124] 0 2] 10 [6 8 [9 20 0 31] 9 2 10 [6 [0 125] 0 14] 0 2] 0 1]
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
        [8 [1 0 0] [1 6 [5 [1 0] 0 12] [0 13] 9 2 10 [6 [8 [9 342 0 7] 9 2 10 [6 0 28] 0 2] 4 0 13] 0 1] 0 1]
        11
        [1.953.718.630 1 6.579.297 [0 7] 0]
        0
        1
      ]
      [ [ 7
          [8 [1 0 0] [1 6 [5 [0 12] 0 13] [1 0] 6 [8 [9 343 0 7] 9 2 10 [6 [0 28] 0 29] 0 2] [1 0] 1 1] 0 1]
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
              [1 6 [8 [9 343 0 31] 9 2 10 [6 [0 124] 0 125] 0 2] [0 6] 9 2 10 [60 8 [9 47 0 31] 9 2 10 [6 [0 124] 0 125] 0 2] 10 [6 4 0 6] 0 1]
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
          [8 [1 0] [1 6 [5 [1 0] 0 6] [0 0] 8 [1 0] 8 [1 6 [5 [0 30] 4 0 6] [0 6] 9 2 10 [6 4 0 6] 0 1] 9 2 0 1] 0 1]
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
                    [6 [9 2 10 [14 [8 [9 342 0 15] 9 2 10 [6 0 60] 0 2] 8 [9 342 0 15] 9 2 10 [6 0 61] 0 2] 0 1] [1 0] 1 1]
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
    [7 [8 [1 0 0] [1 6 [8 [9 343 0 7] 9 2 10 [6 [0 28] 0 29] 0 2] [1 1] 1 0] 0 1] 11 [1.953.718.630 1 6.648.935 [0 7] 0] 0 1]
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
          [6 [0 28] 7 [0 3] 8 [9 4 0 7] 9 2 10 [6 [0 29] 7 [0 3] 8 [9 170 0 7] 9 2 10 [6 [0 28] 0 29] 0 2] 0 2]
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
    [8 [1 0 0] [1 6 [5 [1 0] 0 13] [0 12] 9 2 10 [6 [8 [9 342 0 7] 9 2 10 [6 0 28] 0 2] 8 [9 342 0 7] 9 2 10 [6 0 29] 0 2] 0 1] 0 1]
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
  [ [8 [1 0 0 0 0] [1 0 0] 0 1]
  [ [1 0]
    [ [ 8
        [1 0 0 0]
        [ 1
          8
          [ [8 [7 [0 7] 8 [9 47 0 255] 9 2 10 [6 7 [0 3] 9 751 0 1] 0 2] 9 2 10 [6 0 28] 0 2]
            [8 [7 [0 7] 8 [9 47 0 255] 9 2 10 [6 7 [0 3] 9 751 0 1] 0 2] 9 2 10 [6 0 58] 0 2]
            0
            27
          ]
          8
          [5 [0 14] 0 2]
          0
          6
        ]
        0
        1
      ]
      [8 [1 0 [1 0] 0 0] [1 8 [[6 [6 [3 0 12] [1 1] 1 0] [0 12] 0 0] 8 [7 [0 7] 9 1.534 0 1] 9 2 10 [6 0 29] 0 2] 8 [5 [0 14] 0 2] 0 6] 0 1]
      [8 [1 [0 [[1 0] 0 0] 1 1 [0 0] 0 0 1.701.536.102] [0 0 0 0] 0 0 0] [1 8 [9 22 0 127] 9 2 10 [6 0 14] 0 2] 0 1]
      8
      [1 0 [[1 0] 0 0] 1 1 [0 0] 0 0 1.701.536.102]
      [1 [[8 [9 1.492 0 7] 9 2 10 [6 0 14] 0 2] 8 [9 10 0 15] 9 2 10 [6 0 238] 0 2] 1 0]
      0
      1
    ]
    [8 [1 0] [1 8 [6 [6 [3 0 6] [1 1] 1 0] [0 6] 0 0] 8 [5 [0 14] 0 2] 0 6] 0 1]
    [ 8
      [1 478.793.196.187.462.788.804.451]
      [1 8 [6 [5 [0 6] 1 478.793.196.187.462.788.804.451] [1 478.793.196.187.462.788.804.451] 0 0] 8 [5 [0 14] 0 2] 0 6]
      0
      1
    ]
    [ 8
      [1 0 [[1 0] 0 0] 1 1 [0 0] 0 0 1.701.536.102]
      [1 8 [8 [9 10 0 255] 9 4 10 [6 7 [0 3] 1 3] 0 2] 9 2 10 [6 [7 [0 3] 1 6.243.918] 7 [0 3] 8 [9 22 0 127] 9 2 10 [6 0 14] 0 2] 0 2]
      0
      1
    ]
    8
    [1 0]
    [1 8 [6 [6 [3 0 6] [1 1] 1 0] [0 6] 0 0] 8 [5 [0 14] 0 2] 0 6]
    0
    1
  ]
  [8 [1 0] [1 1 478.793.196.187.462.788.804.451] 0 1]
  [ [8 [1 0 0] [1 0 0] 0 1]
    [ [8 [1 0] [1 0 0] 0 1]
      [ [8 [1 0 [[1 0] 0 0] 1 1 [0 0] 0 0 1.701.536.102] [1 [0 12] 0 26] 0 1]
        [ 8
          [1 [0 [[1 0] 0 0] 1 1 [0 0] 0 0 1.701.536.102] [0 0 0 0] 0 0 0]
          [ 1
            8
            [6 [5 [1 478.793.196.187.462.788.804.451] 0 6] [8 [7 [0 7] 9 174 0 1] 9 2 10 [6 0 14] 0 2] 8 [7 [0 7] 9 762 0 1] 9 2 10 [6 0 14] 0 2]
            8
            [5 [0 14] 0 2]
            0
            6
          ]
          0
          1
        ]
        8
        [1 0 0 0 0]
        [ 1
          8
          [ [8 [7 [0 7] 8 [9 47 0 255] 9 2 10 [6 7 [0 3] 9 351 0 1] 0 2] 9 2 10 [6 0 28] 0 2]
            [8 [7 [0 7] 8 [9 47 0 255] 9 2 10 [6 7 [0 3] 9 374 0 1] 0 2] 9 2 10 [6 0 58] 0 2]
            [6 [6 [3 0 54] [1 1] 1 0] [0 54] 0 0]
            0
            55
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
      [1 0 0 0 0]
      [ 1
        8
        [ [8 [7 [0 7] 8 [9 47 0 255] 9 2 10 [6 7 [0 3] 9 351 0 1] 0 2] 9 2 10 [6 0 28] 0 2]
          [8 [7 [0 7] 8 [9 47 0 255] 9 2 10 [6 7 [0 3] 9 374 0 1] 0 2] 9 2 10 [6 0 58] 0 2]
          [8 [7 [0 7] 8 [9 47 0 255] 9 2 10 [6 7 [0 3] 9 2.986 0 1] 0 2] 9 2 10 [6 0 118] 0 2]
          0
          55
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
    [8 [1 [0 [1 0] 0 0] 0] [1 8 [[8 [7 [0 7] 9 170 0 1] 9 2 10 [6 0 28] 0 2] 6 [6 [3 0 13] [1 1] 1 0] [0 13] 0 0] 8 [5 [0 14] 0 2] 0 6] 0 1]
    8
    [1 0 [[1 0] 0 0] 1 1 [0 0] 0 0 1.701.536.102]
    [ 1
      8
      [ [6 [6 [3 0 12] [1 1] 1 0] [0 12] 0 0]
        [8 [7 [0 7] 9 1.534 0 1] 9 2 10 [6 0 58] 0 2]
        [6 [5 [1 0] 0 54] [1 0] 6 [5 [1 1] 0 54] [1 1] 0 0]
        [6 [6 [3 0 110] [1 1] 1 0] [0 110] 0 0]
        [[6 [6 [3 0 444] [1 1] 1 0] [0 444] 0 0] 6 [6 [3 0 445] [1 1] 1 0] [0 445] 0 0]
        [6 [6 [3 0 446] [1 1] 1 0] [0 446] 0 0]
        [6 [6 [3 0 894] [1 1] 1 0] [0 894] 0 0]
        6
        [5 [0 895] 1 1.701.536.102]
        [1 1.701.536.102]
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
    [1 0 [[1 0] 0 0] 1 1 [0 0] 0 0 1.701.536.102]
    [1 8 [8 [9 10 0 255] 9 4 10 [6 7 [0 3] 1 3] 0 2] 9 2 10 [6 [7 [0 3] 1 6.245.699] 7 [0 3] 8 [9 22 0 127] 9 2 10 [6 0 14] 0 2] 0 2]
    0
    1
  ]
  [ [ [ 1
        [ 8
          [1 0]
          [ 1
            8
            [9 751 0 15]
            9
            2
            10
            [6 7 [0 3] 8 [9 94 0 255] 9 2 10 [6 7 [0 3] 8 [8 [9 10 0 511] 9 767 10 [6 7 [0 3] 1 3] 0 2] 9 2 10 [6 [7 [0 3] 1 3] 0 14] 0 2] 0 2]
            0
            2
          ]
          0
          1
        ]
        8
        [1 0]
        [ 1
          8
          [9 751 0 15]
          9
          2
          10
          [6 7 [0 3] 8 [9 94 0 255] 9 2 10 [6 7 [0 3] 8 [8 [9 10 0 511] 9 767 10 [6 7 [0 3] 1 3] 0 2] 9 2 10 [6 [7 [0 3] 1 3] 0 14] 0 2] 0 2]
          0
          2
        ]
        0
        1
      ]
      0
      1
    ]
    [ 8
      [1 [0 [[1 0] 0 0] 1 1 [0 0] 0 0 1.701.536.102] [0 0 0 0] 0 0 0]
      [ 1
        8
        [[8 [7 [0 7] 9 751 0 1] 9 2 10 [6 0 28] 0 2] [8 [7 [0 7] 9 2.987 0 1] 9 2 10 [6 0 58] 0 2] 8 [7 [0 7] 9 84 0 1] 9 2 10 [6 0 59] 0 2]
        8
        [5 [0 14] 0 2]
        0
        6
      ]
      0
      1
    ]
    8
    [1 0 0]
    [1 0 0]
    0
    1
  ]
  [ 8
    [1 0 0 0 418.565.088.612]
    [ 1
      8
      [ [8 [7 [0 7] 8 [9 47 0 255] 9 2 10 [6 7 [0 3] 9 86 0 1] 0 2] 9 2 10 [6 0 28] 0 2]
        [8 [7 [0 7] 8 [9 47 0 255] 9 2 10 [6 7 [0 3] 9 747 0 1] 0 2] 9 2 10 [6 0 58] 0 2]
        [8 [7 [0 7] 9 766 0 1] 9 2 10 [6 0 118] 0 2]
        6
        [5 [0 55] 1 418.565.088.612]
        [1 418.565.088.612]
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
  [8 [1 0] [1 8 [8 [7 [0 7] 8 [9 47 0 255] 9 2 10 [6 7 [0 3] 9 750 0 1] 0 2] 9 2 10 [6 0 14] 0 2] 8 [5 [0 14] 0 2] 0 6] 0 1]
  [8 [1 [1 0] 0 0] [1 8 [7 [1 0] 8 [1 0] [1 1 0] 0 1] 8 [5 [0 14] 0 2] 0 6] 0 1]
  8
  [1 0]
  [1 1 418.565.088.612]
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

  # temporarily stubbed out
  logics_string = """
  [123 456]
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

  # this section temporarily empty
end
