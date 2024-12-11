defmodule Anoma.Client.Api.Servers.Intents do
  alias Anoma.Client.Connection.GRPCProxy
  alias Anoma.Protobuf.Intents.Add
  alias Anoma.Protobuf.Intents.Compose
  alias Anoma.Protobuf.Intents.Intent
  alias Anoma.Protobuf.Intents.List
  alias Anoma.Protobuf.Intents.Verify
  alias Anoma.TransparentResource.Transaction
  alias GRPC.Server.Stream

  require Logger

  use GRPC.Server, service: Anoma.Protobuf.IntentsService.Service

  @spec list_intents(List.Request.t(), Stream.t()) ::
          List.Response.t()
  def list_intents(request, _stream) do
    Logger.debug("GRPC #{inspect(__ENV__.function)}: #{inspect(request)}")
    {:ok, intents} = GRPCProxy.list_intents()
    intents
  end

  @spec add_intent(Add.Request.t(), Stream.t()) ::
          Add.Response.t()
  def add_intent(request, _stream) do
    Logger.debug("GRPC #{inspect(__ENV__.function)}: #{inspect(request)}")

    {:ok, response} = GRPCProxy.add_intent(request.intent)

    response
  end

  @spec compose(Compose.Request.t(), Stream.t()) :: Compose.Response.t()
  def compose(request, _stream) do
    Logger.debug(
      "GRPC #{inspect(__ENV__.function)} request: #{inspect(request)}"
    )

    with intents <- unpack_intents(request),
         {:ok, nouns} <- cue_nouns(intents),
         {:ok, [t1, t2]} <- nouns_to_transactions(nouns),
         composed <- Transaction.compose(t1, t2),
         noun <- Noun.Nounable.to_noun(composed),
         jammed <- Nock.Jam.jam(noun) do
      %Compose.Response{intent: %Intent{intent: jammed}}
    else
      e -> handle_error(e)
    end
  end

  @spec verify(Verify.Request.t(), Stream.t()) :: Verify.Response.t()
  def verify(request, _stream) do
    Logger.debug(
      "GRPC #{inspect(__ENV__.function)} request: #{inspect(request)}"
    )

    with intent <- request.intent.intent,
         {:ok, noun} <- cue_noun(intent),
         {:ok, tx} <- noun_to_transaction(noun),
         verified <- Transaction.verify(tx) do
      %Verify.Response{valid: verified}
    else
      e -> handle_error(e)
    end
  end

  ############################################################
  #                           Helpers                        #
  ############################################################

  # @doc """
  # Function to handle any error values coming from the domain.
  # If no clause matches, a generic undefined error is raised.
  # """
  @spec handle_error(any()) :: any()
  defp handle_error({:error, :cue_noun_failed, _}) do
    raise GRPC.RPCError,
      status: GRPC.Status.invalid_argument(),
      message: "failed to cue intent input"
  end

  defp handle_error({:error, :noun_is_not_valid_transaction}) do
    raise GRPC.RPCError,
      status: GRPC.Status.invalid_argument(),
      message: "binary was valid noun, but not a valid transaction"
  end

  defp handle_error(e) do
    Logger.error(inspect(e))

    raise GRPC.RPCError,
      status: GRPC.Status.unknown(),
      message: "undefined error"
  end

  @doc """
  I unpack the intent binaries from a compose request.
  I.e., %Request{intents: [%Intent{intent: <<>>}]} -> [<<>>]
  """
  @spec unpack_intents(Compose.Request.t()) :: [binary()]
  def unpack_intents(request) do
    Enum.map(request.intents, &unpack_intent/1)
  end

  @doc """
  I unpack the jammed noun intent from a request.
  """
  @spec unpack_intents(Compose.Request.t()) :: binary()
  def unpack_intent(request) do
    request.intent.intent
  end

  @doc """
  Given a list of jammed nouns, tries to unjam all of them.
  If any of them failed, an error is returned with the failed noun.
  """
  @spec cue_nouns([binary()]) ::
          {:ok, [Noun.t()]} | {:error, :cue_nouns_failed, binary()}
  def cue_nouns(nouns) do
    Enum.reduce_while(nouns, {:ok, []}, fn noun, {:ok, nouns} ->
      case cue_noun(noun) do
        {:ok, noun} ->
          {:cont, {:ok, [noun | nouns]}}

        {:error, :cue_noun_failed, noun} ->
          {:halt, {:error, :cue_nouns_failed, noun}}
      end
    end)
  end

  # @doc """
  # I try to cue a jammed noun.
  # I return an error if i fail.
  # """
  @spec cue_noun(binary()) ::
          {:ok, Noun.t()} | {:error, :cue_noun_failed, binary()}
  defp cue_noun(noun) do
    case Nock.Cue.cue(noun) do
      :error ->
        {:error, :cue_noun_failed, noun}

      {:ok, noun} ->
        {:ok, noun}
    end
  end

  # @doc """
  # I try to convert a given list of nouns into `Transaction` structs.
  # If any of the nouns are not a valid transaction I return it as an error.
  # """
  @spec nouns_to_transactions([Noun.t()]) ::
          {:ok, [Transaction.t()]}
          | {:error, :noun_is_not_valid_transaction, any()}
  def nouns_to_transactions(nouns) do
    Enum.reduce_while(nouns, {:ok, []}, fn noun, transactions ->
      case noun_to_transaction(noun) do
        {:ok, transaction} ->
          {:cont, {:ok, [transaction | transactions]}}

        {:error, :noun_is_not_valid_transaction} ->
          {:halt, {:error, :noun_is_not_valid_transaction}}
      end
    end)
  end

  # @doc """
  # I try and convert a given noun into a transaction.
  # """
  @spec noun_to_transaction(Noun.t()) ::
          {:ok, Transaction.t()} | {:error, :noun_is_not_valid_transaction}
  defp noun_to_transaction(noun) do
    case Transaction.from_noun(noun) do
      {:ok, transaction} ->
        {:ok, transaction}

      :error ->
        {:error, :noun_is_not_valid_transaction}
    end
  end
end
