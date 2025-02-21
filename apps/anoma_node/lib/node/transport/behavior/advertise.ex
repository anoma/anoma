defmodule Anoma.Node.Transport.Advertise do
  alias Anoma.Node.Transport.NetworkRegister.Advert

  @callback advertise(
              node_config :: map(),
              remote_node_id :: String.t(),
              remote_node_advert :: Advert.t()
            ) :: :ok
end
