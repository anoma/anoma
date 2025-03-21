defprotocol Validate do
  @moduledoc """
  I am a protocol that can be used to validate a struct.
  I am used to validate GRPC requests.

  GRPC requests can contain nil instead of a value, where the codebase expects a struct.

  For example, the request below is valid GRPC.

  ```
  %Anoma.Protobuf.Intents.Add.Request{
    node_info: nil,
    intent: nil,
    __unknown_fields__: []
  }
  ```

  However, if the node_info field is not expected to be nil, this will cause crashes.

  These types of errors can be caught early by using this protocol.
  """

  @typedoc """
  An error_map map is a map that contains error data about a request.

  E.g.,

  ```
  %{
    nil: [:intent],
    invalid: [node_info: %{nil: [:node_id], invalid: []}]}
  }
  """
  @type error_map :: %{nil: [atom()], invalid: [{atom(), invalid_error}]}

  @typedoc "An atom that was nil in a map."
  @type nil_error :: atom()

  @typedoc "A key in a map that was invalid."
  @type invalid_error :: {atom(), error_map}

  @fallback_to_any true
  @spec valid?(t) :: {:ok, :valid} | {:error, :invalid, error_map}
  def valid?(request)
end

defmodule Validate.Helpers do
  @moduledoc """
  I contain helper functions for the `Validate` protocol.
  I implement genertic data traversal and constraint checking.
  """

  @doc """
  I traverse a given data structure and return the values that are nil or
  invalid according to their protocol.

  For any value that is not enumerable, I return no errors.

  Note: we assume that there are no missing keys, otherwise the GRPC request
  would fail anyway. So it's only necessary to check for existing keys that are
  nil.
  """
  @spec constraints(any(), Keyword.t()) :: Validate.error_map()
  def constraints(v, opts \\ [])

  def constraints(s, opts) when is_struct(s) do
    constraints(Map.from_struct(s), opts)
  end

  def constraints(m, opts) when is_map(m) do
    opts = Keyword.validate!(opts, non_nil: [])
    acc = %{invalid: [], nil: []}

    m
    |> Enum.reduce(acc, fn {k, v}, acc ->
      case {k, v, Validate.valid?(v)} do
        {k, nil, _} ->
          if k in opts[:non_nil] do
            Map.update!(acc, nil, &[k | &1])
          else
            acc
          end

        {k, _v, {:error, :invalid, err}} ->
          Map.update!(acc, :invalid, &[{k, err} | &1])

        {_k, _v, _} ->
          acc
      end
    end)
  end

  def constraints(_, _), do: %{invalid: [], nil: []}

  @doc """
  Validates a value with the given options.

  This function will call the constraints function on the value and translate
  that result into a result.
  """
  @spec validate(any(), Keyword.t()) ::
          {:ok, :valid} | {:error, :invalid, Validate.error_map()}
  def validate(value, opts) do
    value
    |> constraints(opts)
    |> case do
      %{nil: [], invalid: []} ->
        {:ok, :valid}

      %{nil: _, invalid: _} = errs ->
        {:error, :invalid, errs}
    end
  end
end

############################################################
#                       Intentpool                         #
############################################################

defimpl Validate, for: Anoma.Proto.Intentpool.List.Request do
  @not_nil [:node]

  def valid?(request) do
    Validate.Helpers.validate(request, non_nil: @not_nil)
  end
end

defimpl Validate, for: Anoma.Proto.Intentpool.Add.Request do
  @not_nil [:node, :intent]

  def valid?(request) do
    Validate.Helpers.validate(request, non_nil: @not_nil)
  end
end

############################################################
#                       Mempool                            #
############################################################

defimpl Validate, for: Anoma.Proto.Mempool.Add.Request do
  @not_nil [:node, :transaction]

  def valid?(request) do
    Validate.Helpers.validate(request, non_nil: @not_nil)
  end
end

############################################################
#                       Node Info                          #
############################################################

defimpl Validate, for: Anoma.Proto.Node do
  @not_nil [:id]

  def valid?(request) do
    Validate.Helpers.validate(request, non_nil: @not_nil)
  end
end

############################################################
#                       Intent                             #
############################################################

defimpl Validate, for: Anoma.Proto.Intents.Intent do
  @not_nil [:intent]

  def valid?(request) do
    Validate.Helpers.validate(request, non_nil: @not_nil)
  end
end

############################################################
#                       Transaction                        #
############################################################

defimpl Validate, for: Anoma.Proto.Mempool.Transaction do
  @not_nil [:transaction]

  def valid?(request) do
    Validate.Helpers.validate(request, non_nil: @not_nil)
  end
end

# catch all for other types
defimpl Validate, for: Any do
  def valid?(_value) do
    {:ok, :valid}
  end
end
