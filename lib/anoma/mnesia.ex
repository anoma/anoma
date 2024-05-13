defmodule Anoma.Mnesia do
  @moduledoc """

  I help with various queries around *Mnesia*.

  ### Usage
      i> Anoma.Mnesia.init()

  ### Initialization

  I can help with initializing *Mnesia*. calling `init/0` should setup
  the project to be compatible with

  Note that the Erlang Node I reside in can only have one Mnesia
  database open at a time.

  ### Querying

  I have the following functions that can help query data

    - `dirt_dump/1`
  """

  def init() do
    :mnesia.stop()
    # Create the Schema
    :mnesia.create_schema([node()])
    # Startup Mnesia
    :mnesia.start()
    # Register rocksdb
    :mnesia_rocksdb.register()

    # we may want to plant upgrades in the future via the response
    Anoma.Block.create_table()
  end

  @doc """
  I ensure storage is freshly setup
  """
  def fresh_storage() do
    :mnesia.stop()
    :mnesia.delete_schema([node()])
    init()
  end

  def attach() do
    :mnesia.stop()
    :mnesia.start()
    :mnesia_rocksdb.register()
  end

  # TODO Present the table nicely
  @doc """

  I help dump all data in a given table

  ### Example

       iex(15)> :mnesia.create_table(:test, [attributes: [:id, :name, :job]])
       {:atomic, :ok}
       iex(16)> :mnesia.dirty_write({:test, 1, "G'kar", "Ambassador"})
       :ok
       iex(17)> Anoma.Mnesia.dirty_dump(:test)
       [[{:test, 1, "G'kar", "Ambassador"}]]

  """
  def dirty_dump(table) do
    catch_all = [{:"$1", [], [:"$$"]}]
    :mnesia.dirty_select(table, catch_all)
  end

  @doc """
  I help dump all data in a given table in a non-dirty way.

  See `dirty_dump/1`
  """

  def dump(table) do
    catch_all = [{:"$1", [], [:"$$"]}]
    select = fn -> :mnesia.select(table, catch_all) end

    case :mnesia.transaction(select) do
      {:atomic, res} -> {:ok, res}
      _ -> :error
    end
  end
end
