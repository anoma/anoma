defmodule NockVM.VM do
  @moduledoc """
  I am a Nock VM.
  """

  use GenStateMachine, callback_mode: :state_functions
  use TypedStruct
  require Noun

  typedstruct enforce: true do
    field(:stack, list(), default: [])
    field(:current, term(), default: nil)
    field(:caller, term(), default: nil)
    field(:result, term(), default: nil)
  end

  typedstruct enforce: true, module: VMError do
    field(:message, String.t())
    field(:current, term())
    field(:result, term())
    field(:stack, list())
  end

  def start_link(_opts \\ []) do
    GenStateMachine.start_link(__MODULE__, {:idle, %__MODULE__{}}, name: __MODULE__)
  end

  @doc """
  I send a Nock evaluation to the VM.
  """
  def nock(subject, formula) do
    GenStateMachine.call(__MODULE__, {:nock, subject, formula})
  end

  # State: idle - waiting for evaluation requests
  def idle({:call, from}, {:nock, subject, formula}, data) do
    # Push initial computation onto stack and transition to dispatch state
    new_data = %{data |
      stack: [],
      current: {subject, formula},
      result: nil,
      caller: from
    }
    {:next_state, :dispatch, new_data, {:next_event, :internal, :process}}
  end

  # State: dispatch - determine which operation to perform based on formula
  def dispatch(:internal, :process, %{current: {subject, formula}} = data) do
    case formula do
      # Autocons - cell of formulas becomes cell of results
      [formula_1 = [_ | _] | formula_2] ->
        new_data = %{
          data |
          stack: [{:autocons_cont, formula_2} | data.stack],
          current: {subject, formula_1}
        }
        {:next_state, :dispatch, new_data, {:next_event, :internal, :process}}

      # 0: Read from subject
      [zero | axis] when zero in [0, <<>>, []] and axis in [0, <<>>, []] ->
        # Crash - handle error
        handle_error("axis 0 canonical crash", data)

      [zero | axis] when zero in [0, <<>>, []] and is_integer(axis) ->
        with {:ok, result} <- Noun.axis(axis, subject) do
          finish_computation(data, result)
        else
          _ -> handle_error("invalid axis", data)
        end

      [zero | axis] when zero in [0, <<>>, []] and is_binary(axis) ->
        with {:ok, result} <- Noun.axis(Noun.atom_binary_to_integer(axis), subject) do
          finish_computation(data, result)
        else
          _ -> handle_error("invalid axis", data)
        end

      # 1: Constant
      [one | constant] when one in [1, <<1>>] ->
        finish_computation(data, constant)

      # 2: Eval
      [two, subject_formula | formula_formula] when two in [2, <<2>>] ->
        new_data = %{
          data |
          stack: [{:eval_subject, formula_formula} | data.stack],
          current: {subject, subject_formula}
        }
        {:next_state, :dispatch, new_data, {:next_event, :internal, :process}}

      # 3: Cell test
      [three | sub_formula] when three in [3, <<3>>] ->
        new_data = %{
          data |
          stack: [{:cell_test} | data.stack],
          current: {subject, sub_formula}
        }
        {:next_state, :dispatch, new_data, {:next_event, :internal, :process}}

      # 4: Increment
      [four | sub_formula] when four in [4, <<4>>] ->
        new_data = %{
          data |
          stack: [{:increment} | data.stack],
          current: {subject, sub_formula}
        }
        {:next_state, :dispatch, new_data, {:next_event, :internal, :process}}

      # 5: Noun equality
      [five, formula_1 | formula_2] when five in [5, <<5>>] ->
        new_data = %{
          data |
          stack: [{:equality, formula_2} | data.stack],
          current: {subject, formula_1}
        }
        {:next_state, :dispatch, new_data, {:next_event, :internal, :process}}

      # 6: If-then-else
      [six, cond | branches = [_true_branch | _false_branch]] when six in [6, <<6>>] ->
        # This complex case is broken down into steps
        new_data = %{
          data |
          stack: [{:if_then_else, branches} | data.stack],
          current: {subject, [4 | [4 | cond]]}
        }
        {:next_state, :dispatch, new_data, {:next_event, :internal, :process}}

      # 7: With subject
      [seven, subject_formula | sub_formula] when seven in [7, <<7>>] ->
        new_data = %{
          data |
          stack: [{:with_subject, sub_formula} | data.stack],
          current: {subject, subject_formula}
        }
        {:next_state, :dispatch, new_data, {:next_event, :internal, :process}}

      # 8: Push on subject
      [eight, push_formula | sub_formula] when eight in [8, <<8>>] ->
        new_data = %{
          data |
          stack: [{:push_on_subject, sub_formula} | data.stack],
          current: {subject, push_formula}
        }
        {:next_state, :dispatch, new_data, {:next_event, :internal, :process}}

      # 9: Arm of core
      [nine, axis | sub_formula] when nine in [9, <<9>>] ->
        new_data = %{
          data |
          stack: [{:arm_of_core, axis} | data.stack],
          current: {subject, sub_formula}
        }
        {:next_state, :dispatch, new_data, {:next_event, :internal, :process}}

      # 10: Replace at axis
      [ten, [axis | replacement_formula] | sub_formula] when ten in [10, <<10>>] ->
        new_data = %{
          data |
          stack: [{:replace_at_axis, axis, sub_formula} | data.stack],
          current: {subject, replacement_formula}
        }
        {:next_state, :dispatch, new_data, {:next_event, :internal, :process}}

      # 11: Hint (with formula)
      [eleven, [hint_noun | hint_formula] | sub_formula] when eleven in [11, <<11>>] ->
        new_data = %{
          data |
          stack: [{:hint_with_formula, hint_noun, sub_formula} | data.stack],
          current: {subject, hint_formula}
        }
        {:next_state, :dispatch, new_data, {:next_event, :internal, :process}}

      # 11: Hint (simple)
      [eleven, hint_noun | sub_formula] when eleven in [11, <<11>>] ->
        # Process hint but discard result
        process_hint(hint_noun)
        new_data = %{
          data |
          current: {subject, sub_formula}
        }
        {:next_state, :dispatch, new_data, {:next_event, :internal, :process}}

      # Error case
      _ ->
        handle_error("invalid opcode", data)
    end
  end

  # Process continuation states

  # Handle autocons continuation
  def process(:internal, {:computation_done, result_1}, %{stack: [{:autocons_cont, formula_2} | rest_stack]} = data) do
    subject = data.current |> elem(0)
    new_data = %{
      data |
      stack: [{:autocons_finish, result_1} | rest_stack],
      current: {subject, formula_2}
    }
    {:next_state, :dispatch, new_data, {:next_event, :internal, :process}}
  end

  def process(:internal, {:computation_done, result_2}, %{stack: [{:autocons_finish, result_1} | rest_stack]} = data) do
    finish_computation(%{data | stack: rest_stack}, [result_1 | result_2])
  end

  # Handle eval continuation
  def process(:internal, {:computation_done, new_subject}, %{stack: [{:eval_subject, formula_formula} | rest_stack]} = data) do
    subject = data.current |> elem(0)
    new_data = %{
      data |
      stack: [{:eval_formula, new_subject} | rest_stack],
      current: {subject, formula_formula}
    }
    {:next_state, :dispatch, new_data, {:next_event, :internal, :process}}
  end

  def process(:internal, {:computation_done, new_formula}, %{stack: [{:eval_formula, new_subject} | rest_stack]} = data) do
    new_data = %{
      data |
      stack: rest_stack,
      current: {new_subject, new_formula}
    }
    {:next_state, :dispatch, new_data, {:next_event, :internal, :process}}
  end

  # Handle cell test continuation
  def process(:internal, {:computation_done, sub_result}, %{stack: [{:cell_test} | rest_stack]} = data) do
    result = if Noun.is_noun_cell(sub_result), do: 0, else: 1
    finish_computation(%{data | stack: rest_stack}, result)
  end

  # Handle increment continuation
  def process(:internal, {:computation_done, sub_result}, %{stack: [{:increment} | rest_stack]} = data) do
    result = cond do
      sub_result == [] ->
        1
      is_integer(sub_result) ->
        sub_result + 1
      is_binary(sub_result) ->
        Noun.atom_binary_to_integer(sub_result) + 1
      true ->
        :error
    end

    if result == :error do
      handle_error("increment error", %{data | stack: rest_stack})
    else
      finish_computation(%{data | stack: rest_stack}, result)
    end
  end

  # Handle equality continuation
  def process(:internal, {:computation_done, result_1}, %{stack: [{:equality, formula_2} | rest_stack]} = data) do
    subject = data.current |> elem(0)
    new_data = %{
      data |
      stack: [{:equality_finish, result_1} | rest_stack],
      current: {subject, formula_2}
    }
    {:next_state, :dispatch, new_data, {:next_event, :internal, :process}}
  end

  def process(:internal, {:computation_done, result_2}, %{stack: [{:equality_finish, result_1} | rest_stack]} = data) do
    result = if Noun.equal?(result_1, result_2), do: 0, else: 1
    finish_computation(%{data | stack: rest_stack}, result)
  end

  # Handle if-then-else continuation (multiple steps)
  def process(:internal, {:computation_done, cond_plus_two}, %{stack: [{:if_then_else, branches} | rest_stack]} = data) do
    new_data = %{
      data |
      stack: [{:if_then_else_step2, branches} | rest_stack],
      current: {[2 | 3], [0 | cond_plus_two]}
    }
    {:next_state, :dispatch, new_data, {:next_event, :internal, :process}}
  end

  def process(:internal, {:computation_done, crash_guard}, %{stack: [{:if_then_else_step2, branches} | rest_stack]} = data) do
    new_data = %{
      data |
      stack: [{:if_then_else_step3} | rest_stack],
      current: {branches, [0 | crash_guard]}
    }
    {:next_state, :dispatch, new_data, {:next_event, :internal, :process}}
  end

  def process(:internal, {:computation_done, branch_formula}, %{stack: [{:if_then_else_step3} | rest_stack]} = data) do
    subject = data.current |> elem(0)
    new_data = %{
      data |
      stack: rest_stack,
      current: {subject, branch_formula}
    }
    {:next_state, :dispatch, new_data, {:next_event, :internal, :process}}
  end

  # Handle with subject continuation
  def process(:internal, {:computation_done, new_subject}, %{stack: [{:with_subject, sub_formula} | rest_stack]} = data) do
    new_data = %{
      data |
      stack: rest_stack,
      current: {new_subject, sub_formula}
    }
    {:next_state, :dispatch, new_data, {:next_event, :internal, :process}}
  end

  # Handle push on subject continuation
  def process(:internal, {:computation_done, pushed_noun}, %{stack: [{:push_on_subject, sub_formula} | rest_stack]} = data) do
    subject = data.current |> elem(0)
    new_subject = [pushed_noun | subject]
    new_data = %{
      data |
      stack: rest_stack,
      current: {new_subject, sub_formula}
    }
    {:next_state, :dispatch, new_data, {:next_event, :internal, :process}}
  end

  # Handle arm of core continuation
  def process(:internal, {:computation_done, sub_result}, %{stack: [{:arm_of_core, axis} | rest_stack]} = data) do
    new_data = %{
      data |
      stack: rest_stack,
      current: {sub_result, [2 | [[0 | 1] | [0 | axis]]]}
    }
    {:next_state, :dispatch, new_data, {:next_event, :internal, :process}}
  end

  # Handle replace at axis continuation
  def process(:internal, {:computation_done, replacement}, %{stack: [{:replace_at_axis, axis, sub_formula} | rest_stack]} = data) do
    subject = data.current |> elem(0)
    new_data = %{
      data |
      stack: [{:replace_at_axis_finish, axis, replacement} | rest_stack],
      current: {subject, sub_formula}
    }
    {:next_state, :dispatch, new_data, {:next_event, :internal, :process}}
  end

  def process(:internal, {:computation_done, sub_result}, %{stack: [{:replace_at_axis_finish, axis, replacement} | rest_stack]} = data) do
    result = Noun.replace(
      Noun.atom_binary_to_integer(axis),
      replacement,
      sub_result
    )

    case result do
      {:ok, value} -> finish_computation(%{data | stack: rest_stack}, value)
      :error -> handle_error("replace error", %{data | stack: rest_stack})
    end
  end

  # Handle hint with formula continuation
  def process(:internal, {:computation_done, hint_result}, %{stack: [{:hint_with_formula, hint_noun, sub_formula} | rest_stack]} = data) do
    subject = data.current |> elem(0)
    # Process the hint but don't use the result for computation flow
    process_hint(hint_noun, hint_result)

    new_data = %{
      data |
      stack: [{:hint_with_formula_finish, hint_result} | rest_stack],
      current: {subject, sub_formula}
    }
    {:next_state, :dispatch, new_data, {:next_event, :internal, :process}}
  end

  def process(:internal, {:computation_done, real_result}, %{stack: [{:hint_with_formula_finish, hint_result} | rest_stack]} = data) do
    new_data = %{
      data |
      stack: rest_stack,
      current: {[hint_result | real_result], [0 | 3]}
    }
    {:next_state, :dispatch, new_data, {:next_event, :internal, :process}}
  end

  # Helper functions

  # Process a hint (stub implementation)
  defp process_hint(_hint_noun) do
    :ok
  end

  defp process_hint(_hint_noun, _hint_result) do
    :ok
  end

  # Finish the current computation and either return to caller or continue processing
  defp finish_computation(data, result) do
    case data.stack do
      [] ->
        # We're done with the entire computation
        {:next_state, :idle, %__MODULE__{},
          [{:reply, data.caller, {:ok, result}}]}

      _stack ->
        # Continue with the next computation in the stack
        {:next_state, :process, data, {:next_event, :internal, {:computation_done, result}}}
    end
  end

  # Handle error cases
  defp handle_error(message, data) do
    # Return error to caller
    {:next_state, :idle, %__MODULE__{},
      [{:reply, data.caller, {:error, %VMError{
        message: message,
        current: data.current,
        result: data.result,
        stack: data.stack
      }}}]}
  end
end