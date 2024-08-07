defmodule Anoma.Transaction do
  @moduledoc """
  I represent an Anoma Transaction at the execution level.

  Namely I contain the info on how the information gets processed at the
  engine level. See `Resource.Transaction` for the transaction
  specification at the level of the Resource Machine.
  """
  alias Anoma.Node.Executor.Worker
  alias __MODULE__
  use TypedStruct

  @type execution() :: Worker.transaction()

  @derive {Inspect, only: [:index, :id, :addr]}
  typedstruct do
    @typedoc """
    I am the type of the Anoma Transaction at the Engine level.

    I contain info necessary for the Mempool, Ordering, and Workers to
    properly go through a transaction lifecycle.

    A transaction structure may not contain transaction code or
    index information.

    ### Fields

    - `:index` - An order given to the transaction by the Ordering Engine.
                 Enforced: false
    - `:id` - The ID of a transaction. Usually a random integer.
    - `:addr` - The address of the Worker responsible for the transaction.
    - `:transaction` - The backend transaction code the Worker uses.
    """

    field(:index, non_neg_integer, enforce: false)
    field(:id, Noun.t(), enforce: true)
    field(:addr, Anoma.Node.Router.Addr.t(), enforce: true)
    field(:transaction, execution(), enforce: false)
  end

  @spec new(non_neg_integer(), Anoma.Node.Router.Addr.t(), execution()) :: t()
  def new(id, addr, transaction) do
    %Transaction{id: id, addr: addr, transaction: transaction}
  end

  @doc """
  I create a new transaction structure with order field but no transaction
  code.

  My main purpose is to be used for testing. Generally, the index of the
  transaction comes added to the structure bearing transaction backend code.
  """

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
