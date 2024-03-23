# GenServer wrapper to let us interpose some communication before the child
# process starts, and some wrapping of message receipt
defmodule Anoma.Node.Router.Engine do
  use GenServer

  alias Anoma.Crypto.Id
  alias Anoma.Node.Router

  # Replace with a struct
  @type t() :: {Router.addr(), atom(), term()}

  @spec start_link({Router.addr(), atom(), Id.t(), term()}) ::
          :ignore | {:error, any()} | {:ok, pid()}
  def start_link({router, mod, id, arg}) do
    GenServer.start_link(__MODULE__, {router, mod, id, arg},
      name: Anoma.Node.Router.process_name(mod, id.external)
    )
  end

  @spec init({Router.addr(), atom(), Id.t(), term()}) :: {:ok, t()} | any()
  def init({router, mod, id, arg}) do
    GenServer.cast(router.router, {:init_local_engine, id, self()})
    Process.put(:engine_id, id.external)

    Process.put(
      :engine_addr,
      Anoma.Node.Router.process_name(mod, id.external)
    )

    Process.flag(:trap_exit, true)

    case mod.init(arg) do
      {:ok, state} -> {:ok, {router, mod, state}}
      err -> err
    end
  end

  @spec handle_cast({Router.addr(), term()}, t()) :: any()
  def handle_cast({src, msg}, {router, mod, state}) do
    {:noreply, state} = mod.handle_cast(msg, src, state)
    {:noreply, {router, mod, state}}
  end

  @spec handle_call({Router.addr(), term()}, GenServer.from(), t()) :: any()
  def handle_call({src, msg}, _, {router, mod, state}) do
    case mod.handle_call(msg, src, state) do
      {:reply, res, state} ->
        {:reply, res, {router, mod, state}}

      {:reply, res, state, cont = {:continue, _}} ->
        {:reply, res, {router, mod, state}, cont}
    end
  end

  @spec handle_continue(term(), t()) :: {:noreply, t()} | {:stop, term(), t()}
  def handle_continue(arg, {router, mod, state}) do
    {:noreply, state} = mod.handle_continue(arg, state)
    {:noreply, {router, mod, state}}
  end

  @spec terminate(reason, t()) :: {:stop, reason, t()} when reason: term()
  def terminate(reason, state = {router, _, _}) do
    GenServer.cast(
      router.router,
      {:cleanup_local_engine, Anoma.Node.Router.self_addr(router)}
    )

    {:stop, reason, state}
  end

  def handle_info(info, {router, mod, state}) do
    {:noreply, state} = mod.handle_info(info, state)
    {:noreply, {router, mod, state}}
  end
end
