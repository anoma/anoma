defmodule Anoma.Client.Examples.EStorage do
  alias Anoma.Client.Storage

  @type storage_result :: list({:key, any()} | {:value, any()})

  ############################################################
  #                    Helpers                               #
  ############################################################

  def setup() do
    :mnesia.clear_table(Storage.Updates)
    :mnesia.clear_table(Storage.Values)
  end

  ############################################################
  #                    Examples                              #
  ############################################################

  @spec write_value(any(), any()) :: storage_result
  def write_value(key \\ "key", value \\ "value") do
    setup()
    Storage.write({key, value})

    [key: key, value: value]
  end

  @spec read_value(any(), any()) :: storage_result
  def read_value(key \\ "key", value \\ "value") do
    write_value(key, value)

    {:ok, value} = Storage.read({System.os_time(), key})
    [key: key, value: value]
  end

  @spec overwrite_value(any(), any()) :: storage_result
  def overwrite_value(key \\ "key", value \\ "value") do
    write_value({key, value})
    another_value = "another_" <> value
    Storage.write({key, another_value})
    [key: key, value: another_value]
  end

  @spec read_overwritten_value(any(), any()) :: storage_result
  def read_overwritten_value(key \\ "key", value \\ "value") do
    args = overwrite_value(key, value)
    new_value = args[:value]
    {:ok, ^new_value} = Storage.read({System.os_time(), key})
    [key: key, value: new_value]
  end

  @spec read_absent(any()) :: storage_result
  def read_absent(key \\ "key") do
    setup()

    :absent = Storage.read({System.os_time(), key})
    [key: key, value: :absent]
  end

  @spec read_in_future_and_fail(any()) :: storage_result
  def read_in_future_and_fail(key \\ "key") do
    setup()

    :error = Storage.read({System.os_time() * 2, key})
    [key: key, value: :error]
  end
end
