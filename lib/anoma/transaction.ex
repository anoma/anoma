defmodule Anoma.Transaction do
  @moduledoc """
  I represent an Anoma Transaction

  I contain the intents used in a transaction
  """
  alias __MODULE__
  use TypedStruct

  @type execution() :: {:kv | :rm, Noun.t()}

  typedstruct do
    field(:index, non_neg_integer, enforce: false)
    field(:id, Noun.t(), enforce: true)
    field(:addr, Anoma.Node.Router.Addr.t(), enforce: true)
    field(:transaction, execution(), enforce: false)
  end

  @spec new(non_neg_integer(), Anoma.Node.Router.Addr.t(), execution()) :: t()
  def new(id, addr, transaction) do
    %Transaction{id: id, addr: addr, transaction: transaction}
  end

  @spec new_with_order(
          non_neg_integer,
          non_neg_integer(),
          Anoma.Node.Router.Addr.t()
        ) :: t()
  def new_with_order(order, id, addr) do
    %Transaction{index: order, id: id, addr: addr}
  end

  @spec index(t()) :: non_neg_integer
  def index(t), do: t.index

  @spec addr(t()) :: Anoma.Node.Router.Addr.t()
  def addr(t), do: t.addr

  @spec id(t()) :: Noun.t()
  def id(t), do: t.id

  @spec transaction(t()) :: execution()
  def transaction(t), do: t.transaction
end
