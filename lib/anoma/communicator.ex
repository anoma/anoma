defmodule Anoma.Communicator do
  @moduledoc """

  I am generic communicator stub. The `using` module should be fine
  with being a `Genserver` My main purpose is to be `use/2`d and to
  automate boilerplate


  ### `use Anoma.Communicator, :sub_field :subscribers, handle_sub: :message ` {: .info}

  - required fields
    + `:sub_field` - the field where the subscribers are stored

  - optional fields
    + `:handle_sub`
    + `:handle_unsub`

  When you `use Anoma.Communicator`, the module will inject a
  `subscribe/2` and `unsubscribe/2` messages to automate the behavior.

  Further it will inject a `use Genserver`, since the module must be a
  `GenServer`

  #### :sub_field

  the subscribers type will be had at the `:sub_field`.  These
  behaviors will also define the `handle_cast/`'s for :subscribe and
  :unsubscribe.

  #### :handle_sub

  This gives the option to have a response call for when a
  subscription happens.

  I.E.

      defmodule Foo do
        use Anoma.Communicator, sub_field: sub, handle_sub: :on_sub

        ...
        def handle_cast({:on_sub, subscriber}, agent) do
         ...anything you want on a new sub...
        end
        ...
      end

  #### :handle_unsub

  The same as :handle_sub expect when unsubscription happens

  """

  @type t() :: MapSet.t(GenServer.server())

  def new() do
    MapSet.new()
  end

  defp subfield(args) do
    quote do
      Access.key!(unquote(Keyword.get(args, :sub_field)))
    end
  end

  defp call_back(field, sub) do
    if field do
      quote do
        GenServer.cast(self(), {unquote(field), unquote(sub)})
      end
    end
  end

  defmacro __using__(args) do
    quote do
      use GenServer

      @doc """
      Subscribes to the communicator
      """
      @spec subscribe(GenServer.server(), GenServer.server()) :: :ok
      def subscribe(communicator, subscriber) do
        GenServer.cast(communicator, {:subscribe, subscriber})
      end

      @doc """
      Unsubscribes to the communicator
      """
      @spec unsubscribe(GenServer.server(), GenServer.server()) :: :ok
      def unsubscribe(communicator, subscriber) do
        GenServer.cast(communicator, {:unsubscribe, subscriber})
      end

      def handle_cast({:subscribe, new_sub}, agent) do
        field = unquote(subfield(args))

        unquote(call_back(Keyword.get(args, :handle_sub), quote(do: new_sub)))

        {nil, new_agent} =
          get_and_update_in(agent, [field], fn map ->
            {nil, MapSet.put(map, new_sub)}
          end)

        {:noreply, new_agent}
      end

      def handle_cast({:unsubscribe, old_sub}, agent) do
        field = unquote(subfield(args))

        unquote(
          call_back(Keyword.get(args, :handle_unsub), quote(do: old_sub))
        )

        {nil, new_agent} =
          get_and_update_in(agent, [field], fn map ->
            {nil, MapSet.delete(map, old_sub)}
          end)

        {:noreply, new_agent}
      end
    end
  end
end
