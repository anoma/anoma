defmodule Nock do
  @moduledoc """
  Nock, a universal function on nouns.
  """

  alias __MODULE__

  require Noun
  require Logger

  use TypedStruct

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
    field(:gas_limit, non_neg_integer() | nil, default: nil)
    field(:stdio, any(), default: :logger)
  end

  @dialyzer :no_improper_lists

  # temporary stub functions for jet scaffolding
  @spec get_jet(Noun.t()) ::
          {:ok,
           {String.t(), non_neg_integer(), non_neg_integer(),
            (Noun.t() -> :error | {:ok, Noun.t()}), atom(),
            non_neg_integer()}}
          | :error
  def get_jet(battery_mug) do
    case Enum.find(Nock.Jets.Mugs.jet_registry(), fn {head, _etc} ->
           Noun.equal?(head, battery_mug)
         end) do
      nil -> :error
      {_hd, tail} -> {:ok, tail}
    end
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

    # IO.inspect(battery, label: "battery")
    maybe_jet = get_jet(hd(core))

    case maybe_jet do
      # there's no jet. just use naive nock
      :error ->
        nock(core, [2 | [[0 | 1] | [0 | axis]]], env)

      # a jet exists but it's disabled, use naive nock
      {:ok,
       {_label, _parent_axis, _parent_mug, _jet_function, :disabled, _cost}} ->
        nock(core, [2 | [[0 | 1] | [0 | axis]]], env)

      # a jet exists. mug the parent too
      {:ok,
       {label, parent_axis, parent_registry, jet_function, jet_mode, cost}} ->
        maybe_parent = Noun.axis(parent_axis, core)

        case maybe_parent do
          {:ok, parent} ->
            if Noun.equal?(parent, parent_registry) do
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
            else
              nock(core, [2 | [[0 | 1] | [0 | axis]]], env)
            end

          # the parent didn't even exist, the jet is bogus
          :error ->
            nock(core, [2 | [[0 | 1] | [0 | axis]]], env)
        end
    end
  end

  # scry: magically read from storage.
  def nock(subject, [twelve, type_formula | sub_formula], env)
      when twelve in [12, <<12>>] do
    with {:ok, _type_result} <- nock(subject, type_formula, env),
         {:ok, sub_result} <- nock(subject, sub_formula, env) do
      try do
        env.scry_function.(sub_result)
      rescue
        _ -> :error
      end
    else
      _ -> :error
    end
  end

  # generic case: use naive nock to reduce once.
  @spec nock(Noun.t(), Noun.t(), t()) :: {:ok, Noun.t()} | :error
  def nock(subject, formula, environment) do
    naive_nock(subject, formula, environment)
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
    meter =
      if environment.gas_limit do
        Task.async(Nock, :gas_limit, [environment.gas_limit, self(), 0])
      else
        Task.async(Nock, :gas_meter, [0])
      end

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

  def gas_limit(limit, pid, gas) do
    receive do
      {:gas, n} ->
        if n + gas > limit do
          Process.exit(pid, :kill)
          gas
        else
          gas_meter(n + gas)
        end

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

          if Noun.equal?(result_1, result_2) do
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

          Noun.replace(
            Noun.atom_binary_to_integer(axis),
            replacement,
            sub_result
          )

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
    case environment.stdio do
      :logger ->
        Logger.debug("nock hint: #{inspect(hint_result)}")

      :stdio ->
        IO.write(environment.stdio, "#{inspect(hint_result)}\n")

      _ ->
        # hint_str = Noun.Format.print(hint_result)
        hint_str = Noun.Jam.jam(hint_result) |> Base.encode64()
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

  ############################################################
  #                          Helpers                         #
  ############################################################

  # this section temporarily empty
end
