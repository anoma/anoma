defmodule Anoma.Client.Api.Server do
  alias Anoma.Protobuf.Input
  alias Anoma.Client.Connection.GRPCProxy
  alias GRPC.Server.Stream
  alias Anoma.Protobuf.Indexer.Nullifiers
  alias Anoma.Protobuf.Indexer.UnrevealedCommits
  alias Anoma.Protobuf.Indexer.UnspentResources
  alias Anoma.Protobuf.IntentPool.AddIntent
  alias Anoma.Protobuf.IntentPool.ListIntents
  alias Anoma.Protobuf.Intents
  alias Anoma.Protobuf.Prove
  alias Anoma.Protobuf.RunNock
  alias Anoma.Client.Runner

  require Logger

  use GRPC.Server, service: Intents.Service

  @spec list_intents(ListIntents.Request.t(), Stream.t()) ::
          ListIntents.Response.t()
  def list_intents(request, _stream) do
    Logger.debug("GRPC #{inspect(__ENV__.function)}: #{inspect(request)}")
    {:ok, intents} = GRPCProxy.list_intents()
    intents
  end

  @spec add_intent(AddIntent.Request.t(), Stream.t()) ::
          AddIntent.Response.t()
  def add_intent(request, _stream) do
    Logger.debug("GRPC #{inspect(__ENV__.function)}: #{inspect(request)}")

    {:ok, response} = GRPCProxy.add_intent(request.intent)

    response
  end

  @spec list_nullifiers(Nullifiers.Request.t(), Stream.t()) ::
          Nullifiers.Response.t()
  def list_nullifiers(_request, _stream) do
    %Nullifiers.Response{nullifiers: ["null", "ifier"]}
  end

  @spec list_unrevealed_commits(UnrevealedCommits.Request.t(), Stream.t()) ::
          UnrevealedCommits.Response.t()
  def list_unrevealed_commits(_request, _stream) do
    %UnrevealedCommits.Response{commits: ["commit1", "commit2"]}
  end

  @spec list_unspent_resources(UnspentResources.Request.t(), Stream.t()) ::
          UnspentResources.Response.t()
  def list_unspent_resources(_request, _stream) do
    %UnspentResources.Response{
      unspent_resources: ["unspent resource 1", "unspent resource 2"]
    }
  end

  @spec prove(Prove.Request.t(), Stream.t()) :: Prove.Response.t()
  def prove(request, _stream) do
    result =
      with {:ok, program} <- program_to_noun(request.program),
           {pub_inpt, []} <- inputs_to_noun(request.public_inputs),
           {prv_inpt, []} <- inputs_to_noun(request.private_inputs),
           {:ok, result} <- Runner.prove(program, pub_inpt ++ prv_inpt),
           jammed <- Noun.Jam.jam(result) do
        {:proof, jammed}
      else
        {_, invalid_inputs} when is_list(invalid_inputs) ->
          {:error, "invalid inputs: #{inspect(invalid_inputs)}"}

        {:error, :invalid_program} ->
          {:error, "invalid program"}

        {:error, :failed_to_prove} ->
          {:error, "failed to prove"}

        _ ->
          {:error, "failed to evaluate"}
      end

    %Prove.Response{result: result}
  end

  @spec run_nock(RunNock.Request.t(), Stream.t()) :: RunNock.Response.t()
  def run_nock(request, _stream) do
    result =
      with {:ok, program} <- program_to_noun(request.program),
           {pub_inpt, []} <- inputs_to_noun(request.public_inputs),
           {prv_inpt, []} <- inputs_to_noun(request.private_inputs),
           {:ok, result} <- Runner.prove(program, pub_inpt ++ prv_inpt),
           jammed <- Noun.Jam.jam(result) do
        {:proof, jammed}
      else
        {_, invalid_inputs} when is_list(invalid_inputs) ->
          {:error, "invalid inputs: #{inspect(invalid_inputs)}"}

        {:error, :invalid_program} ->
          {:error, "invalid program"}

        {:error, :failed_to_prove} ->
          {:error, "failed to prove"}

        _ ->
          {:error, "failed to evaluate"}
      end

    %RunNock.Response{result: result}
  end

  #############################################################################
  #                                Helpers                                    #
  #############################################################################

  # @doc """
  # I convert the program parameter to a noun.
  # """
  @spec program_to_noun(
          {:jammed_program, binary()}
          | {:text_program, String.t()}
        ) :: {:ok, Noun.t()} | {:error, :invalid_program}
  defp program_to_noun({:text_program, program}) do
    case Noun.Format.parse(program) do
      {:ok, noun} ->
        {:ok, noun}

      _ ->
        {:error, :invalid_program}
    end
  end

  defp program_to_noun({:jammed_program, program}) do
    case Noun.Jam.cue(program) do
      {:ok, noun} ->
        {:ok, noun}

      _ ->
        {:error, :invalid_program}
    end
  end

  # @doc """
  # I convert a list of inputs to nouns.

  # I return a tuple with the successful and failed conversions.
  # """
  @spec inputs_to_noun([Input.t()]) :: {list(Noun.t()), list(Noun.t())}
  defp inputs_to_noun(inputs) do
    inputs
    |> Enum.map(&input_to_noun/1)
    |> Enum.reduce({[], []}, fn input, {valid, invalid} ->
      case input do
        {:ok, input} ->
          {[input | valid], invalid}

        {:error, _, input} ->
          {valid, [input | invalid]}
      end
    end)
  end

  # @doc """
  # I turn an input into a noun.
  # """
  @spec input_to_noun(Input.t()) ::
          {:ok, Noun.t()} | {:error, :invalid_input, any()}
  defp input_to_noun(%{input: {:jammed, input}}) do
    case Noun.Jam.cue(input) do
      {:ok, noun} ->
        {:ok, noun}

      _ ->
        {:error, :invalid_input, input}
    end
  end

  defp input_to_noun(%{input: {:text, input}}) do
    case Noun.Format.parse(input) do
      {:ok, noun} ->
        {:ok, noun}

      _ ->
        {:error, :invalid_input, input}
    end
  end
end
