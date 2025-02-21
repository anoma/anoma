defmodule Anoma.Node.Transport.NetworkRegister.Advert do
  @moduledoc """
  I am a node advertisement.

  https://specs.anoma.net/main/arch/node/engines/net_registry_messages.html#nodeadvert
  """
  use TypedStruct

  # -----------------------------------------------------------
  # Addresses

  typedstruct module: GRPCAddress, enforce: true do
    @typedoc """
    I am an address.
    https://specs.anoma.net/main/arch/node/engines/net_registry_messages.html#nodeadvert
    """
    field(:host, String.t())
    field(:port, integer())
  end

  typedstruct module: TCPAddress, enforce: true do
    @typedoc """
    I am an address.
    https://specs.anoma.net/main/arch/node/engines/net_registry_messages.html#nodeadvert
    """
    field(:host, String.t())
    field(:port, integer())
  end

  # -----------------------------------------------------------
  # Advertisement

  typedstruct enforce: true do
    @typedoc """
    I am the state of Anoma.Node.NetworkRegistry.
    """
    field(:node_id, String.t())
    field(:grpc_address, GRPCAddress.t(), default: nil)
    field(:tcp_address, TCPAddress.t(), default: nil)
    field(:version, String.t())
  end
end
