defmodule Anoma.Client.Runner do
  @moduledoc """
  I contain logic to run nock code.
  """
  alias Anoma.Client.Node.GRPCProxy
  alias Anoma.Client.Storage
  alias Anoma.RM.Transparent.Transaction
  alias Anoma.Node

  alias Anoma.Node.Transaction.Backends.Events

  @doc """
  I run the given Nock program with its inputs and return the result.
  """
  @spec prove(Noun.t(), [Noun.t()]) ::
          {:ok, Noun.t(), [Noun.t()]}
          | {:error, :failed_to_prove, Nock.error(), [Noun.t()]}
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

    nock_environment = %Nock{stdio: io_sink, scry_function: &client_scry/1}

    result = Nock.nock(core, eval_call, nock_environment)

    # close the IO sink
    {:ok, hints} = close_io_sink(io_sink)

    case result do
      {:ok, noun} ->
        # if the result of the nock program is a transaction, write out its appdata.
        # if it doesnt happen to be a transaction, ignore it and return the result as is.
        :ok = write_transaction_app_data(noun)

        {:ok, noun, hints}

      {:error, reason} ->
        {:error, :failed_to_prove, reason, hints}
    end
  end

  ############################################################
  #                           Helpers                        #
  ############################################################

  @spec write_transaction_app_data(Noun.t()) :: :ok
  defp write_transaction_app_data(noun = [_, _ | _]) do
    case Transaction.from_noun(noun) do
      {:ok, tx} ->
        tx
        |> Transaction.app_data()
        |> Enum.filter(fn {_, bool} -> bool end)
        |> Enum.each(fn {bin, _} ->
          Storage.write({["anoma", "blob", :crypto.hash(:sha256, bin)], bin})
        end)

        :ok

      :error ->
        :ok
    end
  end

  defp write_transaction_app_data(_), do: :ok

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
    ref = make_ref()
    send(io, {:quit, self(), ref})

    receive do
      {^ref, output} ->
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

      {:quit, from, ref} ->
        output = acc |> Enum.reverse()
        send(from, {ref, output})

      _ ->
        capture(acc)
    end
  end

  @doc """
  I am the client-side scry function.

  Given a blob keyspace, I look for a value locally at the given ID-related
  timestamp. If not found, send a read-only transaction to the Node for the
  same blob.

  For RM-reserved keyspaces, I fetch data from the Node directly.
  """
  @spec client_scry(Noun.t()) :: :error | {:ok, Noun.t()}
  def client_scry([id | space]) do
    space_list = space |> Noun.list_nock_to_erlang()

    case space_list do
      ["anoma", "blob" | _ref] ->
        case Storage.read_with_id({id, space_list}) do
          {:ok, val} ->
            {:ok, val}

          :absent ->
            case send_candidate(space_list) do
              :error ->
                :error

              {:ok, value} ->
                Storage.write({space_list, value})
                {:ok, value}
            end
        end

      _ ->
        send_candidate(space_list)
    end
  end

  @spec ro_tx_candidate(Noun.t()) :: Noun.t()
  def ro_tx_candidate(ref) do
    sample = 0
    keyspace = [[ref] | 0]

    arm = [12, [1], [0 | 6] | [1, ref]]

    [[8, [1 | sample], [1 | keyspace], [1 | arm], 0 | 1] | 999]
  end

  @spec send_candidate(Noun.t()) :: {:ok, Noun.t()} | :error
  defp send_candidate(space) do
    # subscribe to local events coming from the node on this process
    # to catch the result event from the transaction.

    tx_candidate = space |> ro_tx_candidate() |> Noun.Jam.jam()

    # wait for the event of the transaction result
    {:ok, :added, id} = GRPCProxy.add_transaction(tx_candidate, :read_only)

    receive do
      {:event,
       {_,
        %EventBroker.Event{
          body: %Node.Event{
            body: %Events.ROEvent{tx_id: ^id, read_result: {:ok, res}}
          }
        }}} ->
        {:ok, res}

      {:event,
       {_,
        %EventBroker.Event{
          body: %Node.Event{
            body: %Events.ROEvent{tx_id: ^id, read_result: :error}
          }
        }}} ->
        :error
    end
  end
end
