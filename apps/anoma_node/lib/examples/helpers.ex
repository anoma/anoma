defmodule Anoma.Node.Examples.Helpers do
  @moduledoc """
  I contain generic helper functions to write examples.
  """

  # @doc """
  # I consume an mnesia table event and remember it for later consultation. I also
  # understand `:quit` to exit, and `{:seen, event, from}` to consult for the
  # presence of a particular event.
  # """
  @spec receive_event([term()]) :: :ok
  defp receive_event(records) do
    receive do
      {:seen, event, from} ->
        send(from, {:seen, event in records})

      :quit ->
        :ok

      {:mnesia_table_event, {:write, record, _activity}} ->
        receive_event([record | records])

      _e ->
        receive_event(records)
    end
  end

  @doc """
  I spawn a new process that will ingest all events from an mnesia table.
  I return the pid of this process.
  """
  @spec table_events_logger(atom()) :: {:ok, pid()}
  def table_events_logger(table) do
    # make sure the process is live before returning from this function.
    this = self()
    ref = make_ref()

    # start the process
    logger =
      spawn_link(fn ->
        # subscribe to all the table events for the given table
        :mnesia.subscribe({:table, table, :simple})
        # acknowledge im subscribed to my creator
        send(this, ref)

        # loop for events
        receive_event([])

        # unsubscribe when done
        :mnesia.unsubscribe({:table, table, :simple})
      end)

    # make sure the process is live before returning from this function
    # by waiting for its message with the ref
    receive do
      ^ref ->
        {:ok, logger}
    end
  end

  @doc """
  Given the pid of a logger process, I check return whether the given event was
  emitted by mnesia or not.
  """
  def seen_event?(logger_pid, event) do
    send(logger_pid, {:seen, event, self()})

    receive do
      {:seen, boolean} ->
        boolean
    end
  end
end
