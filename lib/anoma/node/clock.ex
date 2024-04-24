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
  - `get_epoch/1`
  """

  alias __MODULE__
  alias Anoma.Node.Router

  use TypedStruct
  use Router.Engine

  typedstruct do
    field(:start, integer())
  end

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

  @doc """
  I get the epoch attached to a clock address. This is usually
  the system monotonic time as recorder at the start of the
  node to which the apporproaite clock engine is related.
  """

  @spec get_epoch(Router.Addr.t()) :: integer()
  def get_epoch(clock) do
    Router.call(clock, :get_epoch)
  end

  ############################################################
  #                    Genserver Behavior                    #
  ############################################################

  def handle_call(:get_time, _from, clock) do
    {:reply, System.monotonic_time(:millisecond) - clock.start, clock}
  end

  def handle_call(:get_epoch, _from, clock) do
    {:reply, clock.start, clock}
  end
end
