defmodule Anoma.Node.Transport.IntraNode do
  alias Anoma.Node.Transport.NetworkRegister.Advert.GRPCAddress
  alias Anoma.Node.Transport.NetworkRegister.Advert.TCPAddress

  @doc """
  I send a call message to a remote node over the wire, and expect a response back.
  """
  @callback call(GRPCAddress.t() | TCPAddress.t(), map()) :: {:ok, term()}

  @doc """
  I cast a message to a remote node over the wire, and do not expect a result back.
  """
  @callback cast(GRPCAddress.t() | TCPAddress.t(), map()) :: :ok

  @doc """
  I send an event across the wire to another node, and do not expect a result back.
  """
  @callback publish(
              GRPCAddress.t() | TCPAddress.t(),
              String.t(),
              EventBroker.Event.t()
            ) :: :ok
end
