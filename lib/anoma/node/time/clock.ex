defmodule Anoma.Node.Time.Clock do
  @moduledoc """
  I am the implmentation of the Local Wall Clock Engine.

  I provide info on the time elapsed in milliseconds after the node launched
  and the epoch from which it has been calculated using monotonic time.
  """

  alias __MODULE__
  use TypedStruct
  use GenServer

  alias Anoma.Node.Utility

  typedstruct do
    field(:start, integer())
  end

  def init(args) do
    {:ok, %Clock{start: args[:start]}}
  end

  def start_link(args) do
    GenServer.start_link(__MODULE__, args, Utility.name(args))
  end

  ############################################################
  #                      Public RPC API                      #
  ############################################################

  @spec get_time(GenServer.server()) :: integer()
  def get_time(clock) do
    GenServer.call(clock, :get_time)
  end

  @spec get_epoch(GenServer.server()) :: integer()
  def get_epoch(clock) do
    GenServer.call(clock, :get_epoch)
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
