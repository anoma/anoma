defmodule Anoma.Transaction do
  @moduledoc """
  I represent an Anoma Transaction

  I can be viewed as a wrapper over `Anoma.Intent` where I contain the
  intents used in a transaction

  """
  alias __MODULE__
  use TypedStruct

  @type execution() :: {:kv | :rm, Noun.t()}

  typedstruct enforce: true do
    field(:id, Noun.t())
    field(:pid, pid())
    field(:transaction, execution())
  end

  @spec new(Noun.t(), pid(), execution()) :: t()
  def new(id, pid, transaction) do
    %Transaction{id: id, pid: pid, transaction: transaction}
  end

  @spec id(t()) :: pid()
  def pid(t), do: t.pid

  @spec id(t()) :: Noun.t()
  def id(t), do: t.id

  @spec transaction(t()) :: execution()
  def transaction(t), do: t.transaction
end
