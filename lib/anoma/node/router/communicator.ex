defmodule Anoma.Node.Router.Communicator do
  use Anoma.Communicator, sub_field: :subscribers
  alias Anoma.Communicator, as: ACom

  alias __MODULE__
  use TypedStruct
  alias Anoma.Node.Utility
  alias Anoma.Node.Router.Primary
  alias Anoma.Networking.{Advert, Identity.External}
  alias Anoma.Networking.Routing.{Scope, Table}

  typedstruct do
    field(:subscribers, ACom.t(), default: ACom.new())
    field(:primary, atom(), require: true)
  end

  def init(name: name, init: subscribers) do
    {:ok, %Communicator{primary: name, subscribers: subscribers}}
  end

  def start_link(arg) do
    GenServer.start_link(
      __MODULE__,
      arg,
      Utility.name(arg, &Utility.com_name/1)
    )
  end

  ############################################################
  #                      Public RPC API                      #
  ############################################################

  @spec advertize(GenServer.server(), Advert.peer() | Advert.domain()) :: :ok
  @spec create_topic(GenServer.server(), External.t()) ::
          {:ok | :error, External.t()}
  @spec delete_topic(GenServer.server(), External.t()) ::
          {:ok | :error, External.t()}
  @spec unsubscribe_from_topic(GenServer.server(), External.t(), Scope.t()) ::
          {:ok | :error, External.t()}
  @spec subscribe_to_topic(GenServer.server(), External.t(), Scope.t()) ::
          {:ok | :error, External.t()}

  defdelegate create_topic(communicator, id), to: Primary
  defdelegate delete_topic(communicator, id), to: Primary
  defdelegate advertize(communicator, advert), to: Primary
  defdelegate subscribe_to_topic(communicator, id, scope), to: Primary
  defdelegate unsubscribe_from_topic(communicator, id, scope), to: Primary

  ############################################################
  #                    Genserver Behavior                    #
  ############################################################

  def handle_cast({:advertize, advert}, state) do
    Primary.advertize(state.primary, advert)
    {:noreply, state}
  end

  def handle_call({:create_topic, id}, _from, state) do
    {:reply, Primary.create_topic(state.primary, id), state}
  end

  def handle_call({:delete_topic, id}, _from, state) do
    {:reply, Primary.delete_topic(state.primary, id), state}
  end

  def handle_call({:subscribe_to_topic, id, scope}, _from, state) do
    {:reply, Primary.subscribe_to_topic(state.primary, id, scope), state}
  end

  def handle_call({:unsubscribe_from_topic, id, scope}, _from, state) do
    {:reply, Primary.unsubscribe_from_topic(state.primary, id, scope), state}
  end
end
