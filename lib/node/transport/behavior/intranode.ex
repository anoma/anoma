defmodule Anoma.Node.Transport.IntraNode do
  alias Anoma.Node.Transport.NetworkRegister.Advert.GRPCAddress
  alias Anoma.Node.Transport.NetworkRegister.Advert.TCPAddress

  @typedoc """
  An established, reusable connection to a remote node (for the gRPC
  transport this is a `GRPC.Channel`). Held by the TransportProtocol
  engine and reused across messages, rather than reconnecting on
  every send.
  """
  @type connection :: term()

  @doc """
  I establish a reusable connection to a remote node's transport
  address. The returned connection is held and reused for subsequent
  sends.
  """
  @callback connect(GRPCAddress.t() | TCPAddress.t()) ::
              {:ok, connection()} | {:error, term()}

  @doc """
  I send a call message over an established connection and expect a
  response back.
  """
  @callback call(connection(), map()) :: {:ok, term()} | {:error, term()}

  @doc """
  I cast a message over an established connection and do not expect a
  result back.
  """
  @callback cast(connection(), map()) :: :ok | {:error, term()}

  @doc """
  I publish an event over an established connection and do not expect
  a result back.
  """
  @callback publish(connection(), String.t(), EventBroker.Event.t()) ::
              :ok | {:error, term()}
end
