defmodule Examples.EEVentBroker.EFilter do
  @moduledoc """
  I define examples on how to use the deffilter macro to create filters for the event broker.
  """
  alias EventBroker.Event

  use EventBroker.DefFilter

  # @doc """
  # I am a filter that subscribes to all messages.
  # I don't filter on any patterns, so all messages are valid.
  # """
  deffilter AcceptAll do
    _ -> true
  end

  # @doc """
  # I am a filter that rejects all messages.
  # """
  deffilter RejectAll do
    _ -> false
  end

  # @doc """
  # I am a filter that accept evnets that have a body which is a map and has a key
  # `:level` with value `:error`.
  # """
  deffilter Error do
    %Event{body: %{level: :error}} -> true
    _ -> false
  end
end
