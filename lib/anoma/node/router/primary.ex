defmodule Anoma.Node.Router.Primary do
  @moduledoc """
  I am part of the Networking Machine. I
  """

  alias Anoma.Networking.{Advert, Identity.External}
  alias Anoma.Networking.Routing.{Scope, Table}
  alias Anoma.Node.Utility
  use GenServer

  @type t() :: %{External.t() => Table.t()}

  def init(_args) do
    {:ok, %{}}
  end

  def start_link(arg) do
    GenServer.start_link(__MODULE__, arg, Utility.name(arg))
  end

  ############################################################
  #                      Public RPC API                      #
  ############################################################

  @spec advertize(GenServer.server(), Advert.peer() | Advert.domain()) :: :ok
  def advertize(server, advert) do
    GenServer.cast(server, {:advertize, advert})
  end

  @spec create_topic(GenServer.server(), External.t()) ::
          {:ok | :error, External.t()}
  def create_topic(server, id) do
    GenServer.call(server, {:create_topic, id})
  end

  @spec delete_topic(GenServer.server(), External.t()) ::
          {:ok | :error, External.t()}
  def delete_topic(server, id) do
    GenServer.call(server, {:delete_topic, id})
  end

  @spec unsubscribe_from_topic(GenServer.server(), External.t(), Scope.t()) ::
          {:ok | :error, External.t()}
  def unsubscribe_from_topic(server, id, scope) do
    GenServer.call(server, {:unsubscribe_from_topic, id, scope})
  end

  @spec subscribe_to_topic(GenServer.server(), External.t(), Scope.t()) ::
          {:ok | :error, External.t()}
  def subscribe_to_topic(server, id, scope) do
    GenServer.call(server, {:subscribe_to_topic, id, scope})
  end

  ############################################################
  #                    Genserver Behavior                    #
  ############################################################

  def handle_cast({:advertize, _advert}, state) do
    {:noreply, state}
  end

  def handle_call({:delete_topic, id}, _from, state) do
    {:reply, {:ok, id}, state}
  end

  def handle_call({:create_topic, id}, _from, state) do
    {:reply, {:ok, id}, state}
  end

  def handle_call({:subscribe_to_topic, id, scope}, _from, state) do
    {resp, new_state} = handle_subscribe(id, scope, state)
    {:reply, {resp, id}, new_state}
  end

  def handle_call({:unsubscribe_from_topic, id, scope}, _from, state) do
    {resp, new_state} = handle_unsubscribe(id, scope, state)
    {:reply, {resp, id}, new_state}
  end

  ############################################################
  #                  Genserver Implementation                #
  ############################################################

  @spec handle_subscribe(External.t(), Scope.t(), t()) :: {:ok | :error, t()}
  defp handle_subscribe(id, :forward, state) do
    # try to do it, it's OK, if it fails, we only care about success
    {_, new_state} = handle_subscribe(id, :local, state)
    {:ok, new_state}
  end

  defp handle_subscribe(id, :local, state) do
    Map.get_and_update(state, id, fn v ->
      case v do
        nil -> {:error, nil}
        {pubsub, sub_list} -> {:ok, {pubsub, [id | sub_list]}}
        id -> {:error, id}
      end
    end)
  end

  @spec handle_unsubscribe(External.t(), Scope.t(), t()) ::
          {:ok | :error, t()}
  defp handle_unsubscribe(_id, _scope, state) do
    {:ok, state}
  end
end
