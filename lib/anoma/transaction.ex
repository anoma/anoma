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
    field(:addr, Anoma.Node.Router.Addr.t())
    field(:transaction, execution())
  end

  @spec new(non_neg_integer(), Anoma.Node.Router.Addr.t(), execution()) :: t()
  def new(id, addr, transaction) do
    %Transaction{id: id, addr: addr, transaction: transaction}
  end

  @spec addr(t()) :: Anoma.Node.Router.Addr.t()
  def addr(t), do: t.addr

  @spec id(t()) :: Noun.t()
  def id(t), do: t.id

  @spec transaction(t()) :: execution()
  def transaction(t), do: t.transaction
end
