# GenServer wrapper to let us interpose some communication before the child
# process starts, and some wrapping of message receipt
defmodule Anoma.Node.Router.Engine do
  use GenServer

  def start_link({router, mod, id, arg}) do
    GenServer.start_link(__MODULE__, {router, mod, id, arg},
      name: {:via, Registry, {router.registry, id.external}}
    )
  end

  def init({router, mod, id, arg}) do
    GenServer.cast(router.router, {:init_local_engine, id, {:via, Registry, {router.registry, id.external}}})
    Registry.register(router.registry, self(), id.external)
    Process.flag(:trap_exit, true)

    case mod.init(arg) do
      {:ok, state} -> {:ok, {router, mod, state}}
      err -> err
    end
  end

  def handle_cast({src, msg}, {router, mod, state}) do
    case mod.handle_cast(msg, src, state) do
      {:noreply, state} -> {:noreply, {router, mod, state}}
      {:stop, reason, state} ->
        broadcast_terminate(router)
        {:stop, reason, {router, mod, state}}
    end
  end

  def handle_call({src, msg}, _, {router, mod, state}) do
    case mod.handle_call(msg, src, state) do
      {:reply, res, state} ->
        {:reply, res, {router, mod, state}}

      {:reply, res, state, cont = {:continue, _}} ->
        {:reply, res, {router, mod, state}, cont}

      {:stop, reason, reply, state} ->
        broadcast_terminate(router)
        {:stop, reason, reply, {router, mod, state}}
    end
  end

  def handle_continue(arg, {router, mod, state}) do
    {:noreply, state} = mod.handle_continue(arg, state)
    {:noreply, {router, mod, state}}
  end

  def broadcast_terminate(router) do
    GenServer.cast(
      router.router,
      {:cleanup_local_engine, Anoma.Node.Router.self_addr(router)}
    )
  end

  def terminate(reason, state = {router, _, _}) do
    broadcast_terminate(router)
    {:stop, reason, state}
  end

  def handle_info(info, {router, mod, state}) do
    {:noreply, state} = mod.handle_info(info, state)
    {:noreply, {router, mod, state}}
  end
end
