defmodule Anoma.Node.Time.Communicator do
  @moduledoc """
  I am the communicator for the local wall-clock.
  """

  use TypedStruct
  use GenServer

  alias __MODULE__
  alias Anoma.Node.Utility
  alias Anoma.Node.Time.Clock

  typedstruct do
    field(:clock, atom())
  end

  def init(arg) do
    {:ok, %Communicator{clock: arg[:name]}}
  end

  def start_link(arg) do
    GenServer.start_link(
      __MODULE__,
      arg,
      Utility.name(arg, &Utility.com_name/1)
    )
  end

  ############################################################
  #                      Public RPC API                      #
  ############################################################

  @spec get_time(GenServer.server()) :: integer()
  @spec get_epoch(GenServer.server()) :: integer()

  defdelegate get_time(clock), to: Clock
  defdelegate get_epoch(clock), to: Clock

  ############################################################
  #                    Genserver Behavior                    #
  ############################################################

  def handle_call(:get_time, _from, state) do
    {:reply, Clock.get_time(state.clock), state}
  end

  def handle_call(:get_epoch, _from, state) do
    {:reply, Clock.get_epoch(state.clock), state}
  end
end
