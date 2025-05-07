defmodule Anoma.Node.Transaction.Storage.Events do
  @moduledoc """
  I define all events that are being sent by the storage engine.

  I also define the filters that can be used to subscribe to these events.
  """

  alias Anoma.Node.Event

  use EventBroker.DefFilter
  use TypedStruct

  ############################################################
  #                           Events                         #
  ############################################################

  typedstruct enforce: true, module: WriteEvent do
    @typedoc """
    I am the type of a write event.

    I am sent whenever something has been written at a particular height.

    ### Fields

    - `:height` - The height at which something was just written.
    - `:writes` - A list of tuples {key, value}
    """

    field(:height, non_neg_integer())
    field(:writes, list({Anoma.Node.Transaction.Storage.bare_key(), term()}))
  end

  ############################################################
  #                           Filters                        #
  ############################################################

  deffilter HeightFilter, height: non_neg_integer() do
    %EventBroker.Event{body: %Event{body: %{height: ^height}}} -> true
    _ -> false
  end

  ############################################################
  #                           Json Encoding                  #
  ############################################################

  defimpl Jason.Encoder, for: WriteEvent do
    defp encode_term(term) do
      case term do
        term when is_bitstring(term) ->
          Base.encode64(term)

        term when is_integer(term) ->
          term

        term when is_binary(term) ->
          term

        %MapSet{} ->
          Enum.map(term, &encode_term/1)

        term when is_list(term) ->
          Enum.map(term, &encode_term/1)
      end
    end

    defp encode_key(key) do
      case key do
        key when is_list(key) ->
          Enum.map(key, &encode_key/1)

        key when is_bitstring(key) ->
          Base.encode64(key)

        key when is_binary(key) ->
          key
      end
    end

    def encode(%WriteEvent{} = event, opts) do
      writes =
        event.writes
        |> Enum.map(fn {key, term} ->
          term = encode_term(term)
          key = encode_key(key)
          %{key: key, term: term}
        end)

      event
      |> Map.put(:writes, writes)
      |> Jason.Encode.map(opts)
    end
  end
end
