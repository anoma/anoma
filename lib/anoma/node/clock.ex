defmodule Anoma.Node.Clock do
  @moduledoc """
  I am the implmentation of the Local Wall Clock Engine.

  I provide info on the time elapsed in milliseconds after the node launched
  and the epoch from which it has been calculated using monotonic time.
  """

  alias __MODULE__
  use TypedStruct
  use GenServer

  alias Anoma.Node.Router

  typedstruct do
    field(:start, integer())
  end

  def init(args) do
    {:ok, %Clock{start: args[:start]}}
  end

  ############################################################
  #                      Public RPC API                      #
  ############################################################

  @spec get_time(Router.Addr.t()) :: integer()
  def get_time(clock) do
    Router.call(clock, :get_time)
  end

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
