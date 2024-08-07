defmodule Anoma.Node.Clock do
  @moduledoc """
  I am the implmentation of the Local Wall Clock Engine.

  I provide info on the time elapsed in milliseconds after the node launched
  and the epoch from which it has been calculated using monotonic time.

  The current implementation launches the epoch by asking for the system
  monotonic time at the point of an Anoma node launch. This is recommended
  as all my public API uses system monotonic time to give measurements.

  ### Public API

  I have the following public functionality:

  - `get_time/1`
  """

  alias __MODULE__
  alias Anoma.Node.Router

  use TypedStruct
  use Router.Engine

  typedstruct do
    @typedoc """
    I am the type of the Clock Engine.

    I currently just store the start-up info, recording when the system was
    launched.

    ### Fields

    - `:start` - Integer value corresponding to the system start timing
                 provided by the internal erlang functionality.
    """

    field(:start, integer())
  end

  @doc """
  I am the initialization function for a Clock Engine instance.

   ### Pattern-Matching Variations

  - `init(%Clock{})` - I initialize the Engine with the given state.
  - `init(args)` - I expect a keylist with a `:start` key and return the
                   appropriate state.
  """
  def init(%Clock{} = state) do
    {:ok, state}
  end

  @spec init(list({:start, integer()})) :: {:ok, Clock.t()}
  def init(args) do
    {:ok, %Clock{start: args[:start]}}
  end

  ############################################################
  #                      Public RPC API                      #
  ############################################################

  @doc """
  I get the local time by checking the clock epoch attached to
  the clock address, taking the current system monotonic time,
  and then subtracting the former from the latter.
  """

  @spec get_time(Router.Addr.t()) :: integer()
  def get_time(clock) do
    Router.call(clock, :get_time)
  end

  ############################################################
  #                    Genserver Behavior                    #
  ############################################################

  def handle_call(:get_time, _from, clock) do
    {:reply, do_get_time(clock), clock}
  end

  ############################################################
  #                  Genserver Implementation                #
  ############################################################

  @spec do_get_time(Clock.t()) :: integer()
  defp do_get_time(clock) do
    System.monotonic_time(:millisecond) - clock.start
  end
end
