defmodule Anoma.Node.Identity.Commitment do
  use GenServer

  alias Anoma.Crypto.Sign
  alias Anoma.Identity.Encapsulated

  def start_link(args) do
    GenServer.start_link(__MODULE__, args)
  end

  def init({private, type}) do
    {:ok, %Encapsulated{private: private, type: type}}
  end

  ############################################################
  #                      Public RPC API                      #
  ############################################################

  @spec commit(GenServer.server(), term()) ::
          {:ok, binary()} | {:error, String.t()}
  def commit(server, data) do
    GenServer.call(server, {:commit, data})
  end

  ############################################################
  #                    Genserver Behavior                    #
  ############################################################

  def handle_call({:commit, data}, _from, state) do
    data =
      case state.type do
        :ed25519 ->
          {:ok, Sign.sign(:erlang.term_to_binary(data), state.private)}

        _ ->
          {:error, "not implemented"}
      end

    {:reply, data, state}
  end
end
