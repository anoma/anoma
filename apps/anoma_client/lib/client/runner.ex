defmodule Anoma.Client.Runner do
  alias Anoma.Client.Storage
  alias Anoma.Client.Connection.GRPCProxy
  alias Anoma.TransparentResource.Transaction

  @doc """
  I run the given Nock program with its inputs and return the result.
  """
  @spec prove(Noun.t(), [Noun.t()]) ::
          {:ok, Noun.t(), [Noun.t()]} | {:error, :failed_to_prove}
  def prove(program, inputs) do
    core =
      (Noun.list_nock_to_erlang(program) ++ [Nock.Lib.rm_core()])
      |> to_improper_list()

    eval_call =
      if inputs == [] do
        [9, 2, 0 | 1]
      else
        sample_replace = to_improper_list([6, 1] ++ inputs)
        [9, 2, 10, sample_replace, 0 | 1]
      end

    io_sink = open_io_sink()

    scry = fn [id | ref] ->
      case Storage.read_with_id({id, ref}) do
        {:ok, val} ->
          {:ok, val}

        :absent ->
          tx_candidate = ref |> ro_tx_candidate() |> Noun.Jam.jam()

          with {:ok, reply} <-
                 tx_candidate |> GRPCProxy.add_read_only_transaction() do
            case reply.result do
              {:success, res} ->
                value = res.result |> Noun.Jam.cue!()
                key = Noun.list_nock_to_erlang(ref)
                Storage.write({key, value})
                {:ok, value}

              _ ->
                :error
            end
          end
      end
    end

    case Nock.nock(core, eval_call, %Nock{stdio: io_sink, scry_function: scry}) do
      {:ok, noun} ->
        try do
          {:ok, tx} = noun |> Transaction.from_noun()

          for {key, {value, bool}} <- Transaction.app_data(tx) do
            if bool do
              Storage.write({key, value})
            end
          end
        rescue
          _ -> :ok
        end

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

  @spec ro_tx_candidate(binary()) :: Noun.t()
  def ro_tx_candidate(ref) do
    sample = 0
    keyspace = 0

    arm = [12, [1], [0 | 6] | [1, ref]]

    [[8, [1 | sample], [1 | keyspace], [1 | arm], 0 | 1] | 999]
  end
end
