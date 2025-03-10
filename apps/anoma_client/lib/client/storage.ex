defmodule Anoma.Client.Storage do
  @moduledoc """
  I am the Client Storage module.

  I represent the local timestamped cache of a client akin to the Node
  Storage.

  In contrast to the Node Storage I
  - never block and return an error on evident semantics failures
  - use os time for timestamps
  - don't have in-progress storage and commit directly to default
    tables

  ### Public API

  I have the following public functionality:

  - read/1
  - write/2
  """

  alias __MODULE__
  use TypedStruct

  @doc """
  I am the Client Storage write function.

  I allow to write only at the current OS time.
  """
  @spec write({any(), any()}) :: :ok | :error
  def write({key, value}) do
    time = System.os_time()

    case :mnesia.transaction(fn ->
           case :mnesia.read({Storage.Updates, key}) do
             [{Storage.Updates, ^key, list}] ->
               write_to_tables(time, key, value, list)

             _ ->
               write_to_tables(time, key, value)
           end
         end) do
      {:atomic, :ok} -> :ok
      {:aborted, _} -> :error
    end
  end

  @doc """
  I am the Client Storage function for reading with IDs.

  Given an existing id in the stored table, I read the key at the associated
  timestamp. Otherwise, I read at current time and store said time as value
  to the provided id.
  """
  @spec read_with_id({any(), any()}) :: {:ok, any()} | :absent | :error
  def read_with_id({id, key}) do
    case :mnesia.transaction(fn -> :mnesia.read({Storage.Ids, id}) end) do
      {:atomic, []} ->
        time = System.os_time()
        :mnesia.transaction(fn -> :mnesia.write({Storage.Ids, id, time}) end)
        read({time, key})

      {:atomic, [{Storage.Ids, ^id, old_time}]} ->
        read({old_time, key})

      {:aborted, _} ->
        :error
    end
  end

  @doc """
  I am the Client Storage read function.

  If the time is in the future in comparisson to OS time, I error.
  If we read before any value was writte, I error.

  Otherwise I read the closest value in the past.
  """
  @spec read({non_neg_integer(), any()}) :: {:ok, any()} | :absent | :error
  def read({time, key}) do
    current_time = System.os_time()

    if time > current_time do
      :error
    else
      case :mnesia.transaction(fn ->
             case :mnesia.read({Storage.Updates, key}) do
               [{Storage.Updates, ^key, list}] ->
                 case list |> Enum.find(fn a -> a <= time end) do
                   nil ->
                     :absent

                   closest_time ->
                     [{_, _, value}] =
                       :mnesia.read({Storage.Values, {closest_time, key}})

                     value
                 end

               [] ->
                 :absent
             end
           end) do
        {:atomic, :absent} -> :absent
        {:atomic, val} -> {:ok, val}
        {:aborted, _} -> :error
      end
    end
  end

  @spec write_to_tables(non_neg_integer(), any(), any(), list()) :: :ok
  defp write_to_tables(time, key, value, list \\ []) do
    :mnesia.write({Storage.Updates, key, [time | list]})
    :mnesia.write({Storage.Values, {time, key}, value})
  end
end
