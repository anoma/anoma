defmodule Anoma.Node.Identity.Decryption do
  use GenServer

  alias Anoma.Identity.Encapsulated
  alias Anoma.Crypto.Encrypt

  def start_link(args) do
    GenServer.start_link(__MODULE__, args)
  end

  def init({private, public, type}) do
    {:ok, %Encapsulated{private: private, public: public, type: type}}
  end

  ############################################################
  #                      Public RPC API                      #
  ############################################################

  @spec decrypt(GenServer.server(), binary()) ::
          {:ok, term()} | {:error, any()}
  def decrypt(server, data) when is_binary(data) do
    GenServer.call(server, {:decrypt, data})
  end

  ############################################################
  #                    Genserver Behavior                    #
  ############################################################

  def handle_call({:decrypt, data}, _from, state) do
    data =
      case state.type do
        :box -> Encrypt.unseal(data, state.public, state.private)
        _ -> {:error, "not implemented"}
      end

    {:reply, data, state}
  end
end
