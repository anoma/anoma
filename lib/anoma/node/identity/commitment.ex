defmodule Anoma.Node.Identity.Commitment do
  use GenServer

  alias Anoma.Identity.Parameters
  alias Anoma.Crypto.Sign
  alias Anoma.Identity.Encapsulated

  @typep mode() :: :combined | :detatched

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
    GenServer.call(server, {:commit, data, :detatched})
  end

  @spec commit_combined(GenServer.server(), term()) ::
          {:ok, binary()} | {:error, String.t()}
  def commit_combined(server, data) do
    GenServer.call(server, {:commit, data, :combined})
  end

  ############################################################
  #                    Genserver Behavior                    #
  ############################################################

  def handle_call({:commit, data, mode}, _from, state) do
    response = commit_to_data(state.type, mode, binary(data), state.private)
    {:reply, response, state}
  end

  ############################################################
  #                     GenServer Helpers                    #
  ############################################################

  @spec commit_to_data(Parameters.t(), mode, binary(), Sign.ed25519_secret()) ::
          {:ok, binary()} | {:error, any()}

  defp commit_to_data(:ed25519, :combined, data, sec) do
    {:ok, Sign.sign(data, sec)}
  end

  defp commit_to_data(:ed25519, :detatched, data, sec) do
    {:ok, Sign.sign_detatched(data, sec)}
  end

  defp commit_to_data(_, _, _, _) do
    {:error, "not implemented"}
  end

  @spec binary(term()) :: binary()
  defp binary(term) when is_binary(term), do: term
  defp binary(term), do: :erlang.term_to_binary(term)
end
