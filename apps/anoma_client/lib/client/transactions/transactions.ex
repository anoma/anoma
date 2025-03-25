defmodule Anoma.Client.Transactions do
  alias Anoma.TransparentResource.Transaction
  alias Noun.Nounable
  alias Noun.Jam

  @doc """
  Given two transactions, I compose them into a new transaction.
  """
  @spec compose([binary()]) ::
          {:ok, binary()}
          | {:error, :invalid_input, term()}
          | {:error, :noun_not_a_valid_transaction}
          | {:error, :not_enough_transactions}
  def compose(transactions) do
    # fetch the jammed intents from the request
    with {:ok, nouns} <- cue_transactions(transactions),
         {:ok, transactions} <- nouns_to_transactions(nouns),
         {:ok, composed} <- compose_transactions(transactions),
         noun <- Nounable.to_noun(composed),
         jammed <- Jam.jam(noun) do
      {:ok, jammed}
    else
      {:error, :cue_failed, err} ->
        {:error, :invalid_input, err}

      {:error, :noun_not_a_valid_transaction} ->
        {:error, :noun_not_a_valid_transaction}

      {:error, :not_enough_transactions} ->
        {:error, :not_enough_transactions}
    end
  end

  @doc """
  Given a jammed transaction, I verify it.
  """
  @spec verify(binary()) ::
          {:ok, boolean()}
          | {:error, :noun_not_a_valid_transaction | :verify_failed}
          | {:error, :cue_failed, term()}
  def verify(transaction) do
    with {:ok, noun} <- cue_transaction(transaction),
         {:ok, transaction} <- noun_to_transaction(noun),
         valid? when is_boolean(valid?) <- Transaction.verify(transaction) do
      {:ok, valid?}
    else
      {:error, :cue_failed, err} ->
        {:error, :cue_failed, err}

      {:error, :noun_not_a_valid_transaction} ->
        {:error, :noun_not_a_valid_transaction}

      {:error, _str} ->
        {:error, :verify_failed}
    end
  end

  ############################################################
  #                       Helpers                            #
  ############################################################

  # @doc """
  # I compose a list of transactions into a single transaction.
  # """
  @spec compose_transactions([Transaction.t()]) ::
          {:ok, Transaction.t()} | {:error, :not_enough_transactions}
  defp compose_transactions(transactions) do
    case transactions do
      [_, _ | _] ->
        {:ok, Enum.reduce(transactions, &Transaction.compose/2)}

      _ ->
        {:error, :not_enough_transactions}
    end
  end

  # @doc """
  # I take in jammed noun and cue it.
  # """
  @spec cue_transaction(binary()) ::
          {:ok, Noun.t()} | {:error, :cue_failed, term()}
  defp cue_transaction(transaction) do
    case Jam.cue(transaction) do
      {:ok, noun} ->
        {:ok, noun}

      {:error, %{message: err}} ->
        {:error, :cue_failed, err}
    end
  end

  # @doc """
  # I take in a list of jammed nouns and turn them into nouns.
  # """
  @spec cue_transactions([binary()]) ::
          {:ok, [Noun.t()]} | {:error, :cue_failed, term()}
  defp cue_transactions(transactions) do
    Enum.reduce_while(transactions, [], fn tx, acc ->
      case cue_transaction(tx) do
        {:ok, noun} ->
          {:cont, [noun | acc]}

        {:error, :cue_failed, err} ->
          {:halt, {:error, :cue_failed, err}}
      end
    end)
    |> case do
      {:error, :cue_failed, err} ->
        {:error, :cue_failed, err}

      txs ->
        {:ok, txs}
    end
  end

  # @doc """
  # I take in a noun and turn it into a transaction if possible.
  # """
  @spec noun_to_transaction(Noun.t()) ::
          {:ok, Transaction.t()} | {:error, :noun_not_a_valid_transaction}
  defp noun_to_transaction(noun) do
    case Transaction.from_noun(noun) do
      {:ok, transaction} ->
        {:ok, transaction}

      :error ->
        {:error, :noun_not_a_valid_transaction}
    end
  end

  # @doc """
  # I take in a list of nouns, and turn them into transactions.
  # """
  @spec nouns_to_transactions([Noun.t()]) ::
          {:ok, [Transaction.t()]} | {:error, :noun_not_a_valid_transaction}
  defp nouns_to_transactions(nouns) do
    Enum.reduce_while(nouns, [], fn tx, acc ->
      case noun_to_transaction(tx) do
        {:ok, transaction} ->
          {:cont, [transaction | acc]}

        {:error, :noun_not_a_valid_transaction} ->
          {:halt, {:error, :noun_not_a_valid_transaction}}
      end
    end)
    |> case do
      {:error, :noun_not_a_valid_transaction} ->
        {:error, :noun_not_a_valid_transaction}

      txs ->
        {:ok, txs}
    end
  end
end
