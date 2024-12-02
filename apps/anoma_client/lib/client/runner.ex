defmodule Anoma.Client.Runner do
  @doc """
  I run the given Nock program with its inputs and return the result.
  """
  @spec prove(Noun.t(), [Noun.t()]) ::
          {:ok, Noun.t(), [Noun.t()]} | {:error, :failed_to_prove}
  def prove(program, inputs) do
    core =
      (Noun.list_nock_to_erlang(program) ++ [Nock.rm_core()])
      |> to_improper_list()

    eval_call =
      if inputs == [] do
        [9, 2, 0 | 1]
      else
        sample_replace = to_improper_list([6, 1] ++ inputs)
        [9, 2, 10, sample_replace, 0 | 1]
      end

    io_sink = open_io_sink()

    case Nock.nock(core, eval_call, %Nock{stdio: io_sink}) do
      {:ok, noun} ->
        {:ok, result} = close_io_sink(io_sink)

        {:ok, noun, result}

      :error ->
        {:error, :failed_to_prove}
    end
  end

  ############################################################
  #                           Helpers                        #
  ############################################################

  @doc """
  I turn a list into an improper list.
  E.g., [1,2,3] -> [1,2|3]
  """
  @spec to_improper_list([any()]) :: maybe_improper_list(any(), any())
  def to_improper_list([]), do: []
  def to_improper_list([x]), do: [x]
  def to_improper_list([x, y]), do: [x | y]
  def to_improper_list([h | t]), do: [h | to_improper_list(t)]

  # ----------------------------------------------------------------------------
  # Small helper process to gather IO data.

  @spec open_io_sink() :: pid()
  def open_io_sink() do
    spawn(&capture/0)
  end

  @spec close_io_sink(pid()) :: {:error, :timeout} | {:ok, term()}
  def close_io_sink(io) do
    send(io, {:quit, self()})

    receive do
      output ->
        {:ok, output}
    after
      10 ->
        {:error, :timeout}
    end
  end

  @spec capture(list(term())) :: any()
  defp capture(acc \\ []) do
    receive do
      {:io_request, from, ref, {:put_chars, _, noun_str}} ->
        send(from, {:io_reply, ref, :ok})
        # noun = Noun.Format.parse_always(noun_str)
        noun = Base.decode64!(noun_str) |> Noun.Jam.cue!()
        capture([noun | acc])

      {:quit, from} ->
        output = acc |> Enum.reverse()
        send(from, output)

      _ ->
        capture(acc)
    end
  end
end
