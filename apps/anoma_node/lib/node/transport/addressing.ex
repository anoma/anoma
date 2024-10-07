defmodule Anoma.Node.Transport.Addressing do
  @moduledoc """
  I contain logic to deal with addresses to local and remote engines.
  """

  alias Anoma.Crypto.Id

  @doc """
  Given an engine id and an engine type I can return the pid to send messages to.
  """
  @spec get_address_for(Id.t(), atom()) :: {:via, atom(), {term(), term()}}
  def get_address_for(engine_id, engine_type) do
    key = %{type: engine_type, remote_id: engine_id}
    {:via, Registry, {ProxyRegister, key}}
  end
end
