# GenServer wrapper to let us interpose some communication before the child
# process starts, and some wrapping of message receipt
defmodule Anoma.Node.Router.Engine do
  use GenServer
  use TypedStruct

  alias Anoma.Crypto.Id
  alias Anoma.Node.Router

  defmacro __using__(_) do
    quote do
    end
  end

  typedstruct do
    field(:router_addr, Router.addr())
    field(:module, module())
    field(:module_state, term())
  end

  @type from() :: Router.addr()

  @callback handle_call(request :: term, from, state :: term) ::
              {:reply, reply, new_state}
              | {:reply, reply, new_state,
                 timeout | :hibernate | {:continue, continue_arg :: term}}
              | {:noreply, new_state}
              | {:noreply, new_state,
                 timeout | :hibernate | {:continue, continue_arg :: term}}
              | {:stop, reason, reply, new_state}
              | {:stop, reason, new_state}
            when reply: term, new_state: term, reason: term

  @callback handle_cast(request :: term, from, state :: term) ::
              {:noreply, new_state}
              | {:noreply, new_state,
                 timeout | :hibernate | {:continue, continue_arg :: term}}
              | {:stop, reason :: term, new_state}
            when new_state: term

  @spec start_link({Router.addr(), atom(), Id.t(), term()}) ::
          :ignore | {:error, any()} | {:ok, pid()}
  def start_link({router, mod, id, arg}) do
    GenServer.start_link(__MODULE__, {router, mod, id, arg},
      name: Router.process_name(mod, id.external)
    )
  end

  @spec init({Router.addr(), atom(), Id.t(), term()}) :: {:ok, t()} | any()
  def init({router, mod, id, arg}) do
    GenServer.cast(router.router, {:init_local_engine, id, self()})
    Process.put(:engine_id, id.external)

    Process.put(:engine_addr, Router.process_name(mod, id.external))

    Process.flag(:trap_exit, true)

    case mod.init(arg) do
      {:ok, state} ->
        {:ok,
         %__MODULE__{router_addr: router, module: mod, module_state: state}}

      err ->
        err
    end
  end

  @spec get_state(Router.addr()) :: struct() | any()
  def get_state(addr) do
    GenServer.call(addr.server, :get_state)
  end

  @spec handle_cast({any(), Router.addr(), term()}, t()) :: any()
  def handle_cast({:router_cast, src, msg}, state = %__MODULE__{}) do
    {:noreply, ns} = state.module.handle_cast(msg, src, state.module_state)
    {:noreply, %__MODULE__{state | module_state: ns}}
  end

  @spec handle_call({any(), Router.addr(), term()}, GenServer.from(), t()) ::
          any()
  def handle_call({:router_call, src, msg}, _, state = %__MODULE__{}) do
    case state.module.handle_call(msg, src, state.module_state) do
      {:reply, res, new_state} ->
        {:reply, res, %__MODULE__{state | module_state: new_state}}

      {:reply, res, new_state, cont = {:continue, _}} ->
        {:reply, res, %__MODULE__{state | module_state: new_state}, cont}
    end
  end

  @spec handle_call(:get_state, GenServer.from(), %__MODULE__{}) ::
          struct() | any()
  def handle_call(:get_state, _, state) do
    {:reply, state.module_state, state}
  end

  @spec handle_continue(term(), t()) :: {:noreply, t()} | {:stop, term(), t()}
  def handle_continue(arg, state = %__MODULE__{}) do
    {:noreply, new_state} = state.module.handle_continue(arg, state)
    {:noreply, %__MODULE__{state | module_state: new_state}}
  end

  @spec terminate(reason, t()) :: {:stop, reason, t()} when reason: term()
  def terminate(reason, state = %__MODULE__{}) do
    GenServer.cast(
      state.router_addr.router,
      {:cleanup_local_engine, Router.self_addr(state.router_addr)}
    )

    {:stop, reason, state}
  end

  def handle_info(info, state = %__MODULE__{}) do
    {:noreply, new_state} = state.module.handle_info(info, state.module_state)
    {:noreply, %__MODULE__{state | module_state: new_state}}
  end
end
