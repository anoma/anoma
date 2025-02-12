defmodule Anoma.Protobuf.ErrorHandler do
  @moduledoc """
  I define functions that deal with errors during the handling of GRPC requests.

  I have functionality to validate an incoming request, and to raise errors that
  return useful error messages to the sender of the request.
  """
  require Logger

  @doc """
  Given a GRPC request, I check if this request is valid.

  If it is not, I raise a GRPC error. If it is, I do nothing.
  """
  @spec validate_request!(any()) :: :noop
  def validate_request!(req) do
    case Validate.valid?(req) do
      {:ok, :valid} ->
        :noop

      {:error, :invalid, errors} ->
        raise_grpc_error!({:invalid_request, errors})
    end
  end

  # @doc """
  # Function to handle any error values coming from the domain.
  #
  # If no clause matches, a generic undefined error is raised.
  # """
  @spec raise_grpc_error!(any()) :: any()
  # node id was not found on this machine
  def raise_grpc_error!(:invalid_node_id) do
    Logger.error("node id does not exist")

    raise GRPC.RPCError,
      status: GRPC.Status.invalid_argument(),
      message: "node id does not exist"
  end

  # error cueing a jammed noun
  def raise_grpc_error!(%Noun.Jam.CueError{}) do
    raise GRPC.RPCError,
      status: GRPC.Status.invalid_argument(),
      message: "invalid nock code"
  end

  # generic invalid request
  def raise_grpc_error!(:invalid_request) do
    raise GRPC.RPCError,
      status: GRPC.Status.unknown(),
      message: "invalid request"
  end

  # invalid request with list of values that are invalid
  # E.g., [node_id: "can not be nil"]
  def raise_grpc_error!({:invalid_request, errors}) when is_map(errors) do
    raise GRPC.RPCError,
      status: GRPC.Status.invalid_argument(),
      message: error_message(errors)
  end

  # an exception was caught that has a message
  # e.g., KeyError
  def raise_grpc_error!(%{message: m}) do
    raise GRPC.RPCError,
      status: GRPC.Status.unknown(),
      message: m
  end

  # generic catch all
  def raise_grpc_error!(e) do
    Logger.error("unknown error in GRPC: #{inspect(e)}")

    raise GRPC.RPCError,
      status: GRPC.Status.unknown(),
      message: "undefined error"
  end

  ############################################################
  #                       Helpers                            #
  ############################################################

  # @doc """
  # I take in an error map and return a string that explains the errors to the
  # user.
  # """
  @spec error_message(Validate.error_map()) :: String.t()
  defp error_message(%{nil: nils, invalid: invalids}) do
    nil_errors(nils) <> invalid_errors(invalids)
  end

  # @doc """
  # I return a string telling all the fields that were nil, and were not supposed to be.
  # """
  @spec nil_errors([Validate.nil_error()]) :: String.t()
  defp nil_errors(nils) do
    nils
    |> Enum.map(&"#{&1} can not be nil")
    |> Enum.intersperse(", ")
    |> Enum.join("")
  end

  # @doc """
  # I return an error message telling why specific fields were invalid, and the
  # reason they are invalid.
  # """
  @spec invalid_errors([{atom(), Validate.error_map()}]) :: String.t()
  defp invalid_errors([]), do: ""

  defp invalid_errors(invalids) do
    invalids
    |> Enum.map(fn {field, errors} ->
      "#{inspect(field)} #{error_message(errors)}"
    end)
  end
end
