defmodule Anoma.Client.Api.Server do
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

  @spec prove(Prove.Request.t(), Stream.t()) ::
          Prove.Response.t()
  def prove(request, _stream) do
    # depending if the program is plain text or jammed, unpack it.
    program = parse_program(request.program)
    pub_inputs = parse_inputs(request.public_inputs)
    priv_inputs = parse_inputs(request.private_inputs)

    result =
      case run_nock_program(program, [pub_inputs | priv_inputs]) do
        {:ok, result} ->
          {:proof, Nock.Jam.jam(result)}

        :error ->
          {:error, "failed to prove the nock program"}
      end

    %Prove.Response{result: result}
  end

  @spec run_nock(RunNock.Request.t(), Stream.t()) ::
          RunNock.Response.t()
  def run_nock(request, _stream) do
    # depending if the program is plain text or jammed, unpack it.
    program = parse_program(request.program)
    inputs = parse_inputs(request.inputs)

    result =
      case run_nock_program(program, inputs) do
        {:ok, result} ->
          {:output, Nock.Jam.jam(result)}

        :error ->
          {:error, "failed to run the nock program"}
      end

    %RunNock.Response{result: result}
  end

  #############################################################################
  #                                Helpers                                    #
  #############################################################################

  # @doc """
  # I parse a program from the request into a noun.
  # """
  @spec parse_program(
          {:jammed_program, binary()}
          | {:text_program, String.t()}
        ) :: Noun.t()
  defp parse_program(program) do
    case program do
      {:jammed_program, program} ->
        {:ok, nock_cued} = Nock.Cue.cue(program)
        nock_cued

      {:text_program, program} ->
        Noun.Format.parse_always(program)
    end
  end

  # @doc """
  # I parse inputs from the protobuf request into a noun.
  # """
  @spec parse_inputs([%{input: {:jammed | :text, any()}}]) ::
          maybe_improper_list(Noun.t(), Noun.t())
  defp parse_inputs(inputs) do
    inputs
    |> Enum.map(fn
      %{input: {:jammed, input}} ->
        Nock.Cue.cue(input)

      %{input: {:text, input}} ->
        Noun.Format.parse_always(input)
    end)
    |> to_improper_list()
  end

  # @doc """
  # I run a nock program with public and private inputs.
  # If no private inputs are given, they are replaced with an empty list.
  # """
  @spec run_nock_program(Noun.t(), Noun.t()) ::
          {:ok, Noun.t()} | :error
  defp run_nock_program(program, arguments) do
    core =
      ((program |> Noun.list_nock_to_erlang()) ++ [Nock.rm_core()])
      |> to_improper_list()

    Nock.nock(core, [9, 2, 10, [6, 1 | arguments], 0 | 1])
  end

  # @doc """
  # I turn a list into an improper list.
  # E.g., [1,2,3] -> [1,2|3]
  # """
  @spec to_improper_list([any()]) :: maybe_improper_list(any(), any())
  def to_improper_list([]), do: []
  def to_improper_list([x]), do: [x]
  def to_improper_list([x, y]), do: [x | y]
  def to_improper_list([h | t]), do: [h | to_improper_list(t)]
end
