defmodule Anoma.Node.Router.Engine do
  @moduledoc """
  I am the general Engine module.

  I possess info on how all Engines are set up and process messages through
  the Router. Moreover, I provide access to Engine-wide API.

  Note other that the coordination info, such as a Router address and
  module name of an engine instance, I provide an actual storage of an
  Engine state and all state transformaations correspond to the changes one
  can observe in my code.

  ### Public API

  I provide the following public functionality:

  - `get_state/1`
  - `get_router/1`
  - `child_spec/1`
  - `terminate/2`
  """

  use GenServer
  use TypedStruct

  alias Anoma.Crypto.Id
  alias Anoma.Node.Router
  alias Anoma.Node.Router.Addr

  defmacro __using__(opts) do
    quote do
      use GenServer, unquote(opts)
    end
  end

  typedstruct do
    @typedoc """
    I am the type of an Engine as seen from the Router on the Anoma
    networking layer.

    ### Fields

    - `:router_addr` - The address of the Router that the Engine is
                       connected to.
    - `:module` - The module name of the Engine.
    - `:module_state` - The current state of an Engine instance.
    """

    field(:router_addr, Router.addr())
    field(:module, module())
    field(:module_state, term())
  end

  @typedoc """
  I am the type designating the Address of where the message got sent from.
  """
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

  @doc """
  I am the starting function for an Engine instance.

  I create an appropriate server name using `Router.process_name` with the
  supplied module name and Anoma ID, launching a GenServer with said name
  afterwards.
  """

  @spec start_link({Router.addr(), atom(), Id.t() | Id.Extern.t(), term()}) ::
          :ignore | {:error, any()} | {:ok, pid()}
  def start_link({router, mod, id, arg}) do
    server = Router.process_name(mod, Id.external_id(id))

    GenServer.start_link(__MODULE__, {router, mod, id, arg, server},
      name: server
    )
  end

  @doc """
  I am an Engine instance initialization function. I setup the initial
  state of an Engine structure based on the state of an actual Engine
  instance.

  I first cast a message to initialize a local engine to the connected
  eoutwe, then store the engine ID, server, and router information in the
  Process dictionary.

  Afterwards, I call the actual (module) Engine instance initialization
  function, and put it as the initial state alongside the router and module
  information directly provided on startup.
  """

  @spec init({Router.addr(), atom(), Id.t() | Id.Extern.t(), term(), atom()}) ::
          :ignore
          | {:ok, t()}
          | {:ok, any(),
             :hibernate | :infinity | non_neg_integer() | {:continue, any()}}
          | {:stop, any()}
  def init({router, mod, id, arg, server}) do
    GenServer.cast(Addr.server(router), {:init_local_engine, id, server})
    Process.put(:engine_id, Id.external_id(id))
    Process.put(:engine_server, server)
    Process.put(:engine_router, router)

    Process.flag(:trap_exit, true)

    postprocess(mod.init(arg), %__MODULE__{router_addr: router, module: mod})
  end

  ############################################################
  #                      Public RPC API                      #
  ############################################################

  @doc """
  I am an Engine-wide function for getting the current Engine state.

  Given an address of an Engine, I simply dump the state associated with it
  stored in the Engine structure associated with the given server name.
  """

  @spec get_state(Router.addr()) :: struct() | any()
  def get_state(addr) do
    GenServer.call(addr.server, :get_state)
  end

  @doc """
  I am an Engine-wide function for getting the router address associated
  with the given Engine.

  I provide it by simply dumping the `:router` field of the asssociated
  Engine server.
  """

  @spec get_router(Router.addr()) :: Router.addr() | any()
  def get_router(addr) do
    GenServer.call(addr.server, :get_router)
  end

  ############################################################
  #                    Genserver Behavior                    #
  ############################################################

  @spec handle_cast({any(), Router.addr(), term()}, t()) ::
          {:noreply, any()}
          | {:noreply, any(),
             :hibernate | :infinity | non_neg_integer() | {:continue, any()}}
          | {:stop, any(), any()}
  def handle_cast({:router_cast, src, msg}, state = %__MODULE__{}) do
    postprocess(state.module.handle_cast(msg, src, state.module_state), state)
  end

  def handle_cast({:router_external_cast, src, msg}, state = %__MODULE__{}) do
    # we should have a set of expected message shapes for the engine, and
    # discard messages which do not fit
    case Anoma.Serialise.unpack(msg) do
      {:ok, msg} ->
        postprocess(
          state.module.handle_cast(msg, src, state.module_state),
          state
        )

      _err ->
        # drop; should perhaps log and distrust and whatnot (is this actually
        # good reason for distrust?)
        {:noreply, state}
    end
  end

  def handle_cast(
        {:router_external_call, src, cookie, msg},
        state = %__MODULE__{router_addr: router}
      ) do
    # ditto, and also the result should be postprocessed more appropriately
    # before being sent off
    case Anoma.Serialise.unpack(msg) do
      {:ok, msg} ->
        result =
          case state.module.handle_call(msg, src, state.module_state) do
            {:reply, res, mod_state} ->
              Router.send_response(
                router,
                src,
                cookie,
                Anoma.Serialise.pack(res)
              )

              {:noreply, mod_state}

            {:reply, res, mod_state, cont} ->
              Router.send_response(
                router,
                src,
                cookie,
                Anoma.Serialise.pack(res)
              )

              {:noreply, mod_state, cont}

            {:stop, reason, res, mod_state} ->
              Router.send_response(
                router,
                src,
                cookie,
                Anoma.Serialise.pack(res)
              )

              {:stop, reason, mod_state}

            result ->
              result
          end

        postprocess(result, state)

      _ ->
        # drop
        {:noreply, state}
    end
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

  @spec handle_call(:get_router, GenServer.from(), %__MODULE__{}) ::
          Router.addr() | any()
  def handle_call(:get_router, _, state) do
    {:reply, state.router_addr, state}
  end

  @spec handle_continue(term(), t()) :: {:noreply, t()} | {:stop, term(), t()}
  def handle_continue(arg, state = %__MODULE__{}) do
    postprocess(state.module.handle_continue(arg, state.module_state), state)
  end

  def handle_info(info, state = %__MODULE__{}) do
    postprocess(state.module.handle_info(info, state.module_state), state)
  end

  ############################################################
  #                  Genserver Implementation                #
  ############################################################

  @spec postprocess(
          any()
          | {:ok | :noreply, struct()}
          | {:ok | :stop | :noreply | :reply, struct() | atom() | any(),
             any() | struct()}
          | {:reply, any(), struct(), any()},
          struct()
        ) ::
          {:ok | :noreply, %__MODULE__{}}
          | {:ok | :noreply, %__MODULE__{}, any()}
          | {:stop | :reply, any(), %__MODULE__{}}
          | {:stop, :reply, any(), %__MODULE__{}, any()}
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

  ############################################################
  #                          Helpers                         #
  ############################################################

  @doc """
  I am an Engine-wide child specification function.

  Given a tuple with router, module, ID, argument information, I call the
  `child_spec/1` function of the specified module and then merge with it
  base information where the start info gets provided from the supplied
  arguments.
  """

  @spec child_spec({Router.Addr.t(), module(), any(), any()}) :: %{
          id: __MODULE__,
          start: {__MODULE__, :start_link, [any()]}
        }
  def child_spec(argument = {_router, mod, _id, arg}) do
    mod.child_spec(arg)
    |> Map.merge(%{
      id: __MODULE__,
      start: {__MODULE__, :start_link, [argument]}
    })
  end

  @doc """
  I am an engine termination function.

  I provide the main functionality to shut down an engine.

  I first use the GenSercer functionality to route a message to clean up
  the local engine info stored inside the specified Router and then stop
  the Engine process with a given reason.
  """

  @spec terminate(reason, t()) :: {:stop, reason, t()} when reason: term()
  def terminate(reason, state = %__MODULE__{}) do
    GenServer.cast(
      Addr.server(state.router_addr),
      {:cleanup_local_engine, Router.self_addr()}
    )

    {:stop, reason, state}
  end
end
