defmodule Anoma.Node.Transport.IntraNode do
  alias Anoma.Node.Transport.NetworkRegister.Advert.GRPCAddress
  alias Anoma.Node.Transport.NetworkRegister.Advert.TCPAddress

  @callback call(GRPCAddress.t() | TCPAddress.t(), map()) :: map()
end
