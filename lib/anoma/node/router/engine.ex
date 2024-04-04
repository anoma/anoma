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
    server = Router.process_name(mod, id.external)

    GenServer.start_link(__MODULE__, {router, mod, id, arg, server},
      name: server
    )
  end

  @spec init({Router.addr(), atom(), Id.t(), term(), term()}) ::
          :ignore
          | {:ok, t()}
          | {:ok, any(),
             :hibernate | :infinity | non_neg_integer() | {:continue, any()}}
          | {:stop, any()}
  def init({router, mod, id, arg, server}) do
    GenServer.cast(router.router, {:init_local_engine, id, server})
    Process.put(:engine_id, id.external)
    Process.put(:engine_server, server)

    Process.flag(:trap_exit, true)

    postprocess(mod.init(arg), %__MODULE__{router_addr: router, module: mod})
  end

  @spec get_state(Router.addr()) :: struct() | any()
  def get_state(addr) do
    GenServer.call(addr.server, :get_state)
  end

  @spec handle_cast({any(), Router.addr(), term()}, t()) ::
          {:noreply, any()}
          | {:noreply, any(),
             :hibernate | :infinity | non_neg_integer() | {:continue, any()}}
          | {:stop, any(), any()}
  def handle_cast({:router_cast, src, msg}, state = %__MODULE__{}) do
    postprocess(state.module.handle_cast(msg, src, state.module_state), state)
  end

  @spec handle_call({any(), Router.addr(), term()}, GenServer.from(), t()) ::
          {:noreply, any()}
          | {:noreply, any(),
             :hibernate | :infinity | non_neg_integer() | {:continue, any()}}
          | {:reply, any(), any()}
          | {:reply, any(), any(),
             :hibernate | :infinity | non_neg_integer() | {:continue, any()}}
          | {:stop, any(), any()}
          | {:stop, any(), any(), any()}
  def handle_call({:router_call, src, msg}, _, state = %__MODULE__{}) do
    postprocess(state.module.handle_call(msg, src, state.module_state), state)
  end

  @spec handle_call(:get_state, GenServer.from(), %__MODULE__{}) ::
          struct() | any()
  def handle_call(:get_state, _, state) do
    {:reply, state.module_state, state}
  end

  @spec handle_continue(term(), t()) :: {:noreply, t()} | {:stop, term(), t()}
  def handle_continue(arg, state = %__MODULE__{}) do
    postprocess(state.module.handle_continue(arg, state.module_state), state)
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
    postprocess(state.module.handle_info(info, state.module_state), state)
  end

  defp postprocess(result, state) do
    case result do
      {:ok, new_state} ->
        {:ok, %__MODULE__{state | module_state: new_state}}

      {:ok, new_state, cont} ->
        {:ok, %__MODULE__{state | module_state: new_state}, cont}

      {:stop, reason, new_state} ->
        {:stop, reason, %__MODULE__{state | module_state: new_state}}

      {:noreply, new_state} ->
        {:noreply, %__MODULE__{state | module_state: new_state}}

      {:noreply, new_state, cont} ->
        {:noreply, %__MODULE__{state | module_state: new_state}, cont}

      {:reply, res, new_state} ->
        {:reply, res, %__MODULE__{state | module_state: new_state}}

      {:reply, res, new_state, cont} ->
        {:reply, res, %__MODULE__{state | module_state: new_state}, cont}

      err ->
        err
    end
  end
end
