defmodule Examples.ESerialisation do
  require ExUnit.Assertions
  import ExUnit.Assertions

  alias Examples.EResource
  alias Examples.ETransaction
  alias Anoma.Serialise

  @spec empty_tx() :: :msgpack.object()
  def empty_tx() do
    etx = ETransaction.empty_transaction()
    tx = Serialise.to_msgpack(etx)

    pack_unpack_id(tx, etx)

    tx
  end

  @spec simple_map() :: :msgpack.object()
  def simple_map() do
    map = %{[1, 2 | 3] => 5}
    serialised = Serialise.to_msgpack(map)

    pack_unpack_id(serialised, map)

    serialised
  end

  @spec aresource() :: :msgpack.object()
  def aresource() do
    res = EResource.a_resource()
    serialised = Serialise.to_msgpack(res)

    pack_unpack_id(serialised, res)

    serialised
  end

  @spec pack_unpack_id(:msgpack.object(), any()) :: :msgpack.object()
  def pack_unpack_id(term, original_term) do
    assert {:ok, new_term} = :msgpack.unpack(:msgpack.pack(term))
    assert {:ok, original_term} == Serialise.from_msgpack(term)
    assert {:ok, original_term} == Serialise.from_msgpack(new_term)
    new_term
  end
end
