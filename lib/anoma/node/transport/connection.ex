defmodule Anoma.Node.Transport.Connection do
  @moduledoc """
  I am the Transport Connection module.

  I am an abstract interface that connection implementations obey.

  ### Public API

  I provide the following public functionality:

  - `send/2`
  - `shutdown/1`
  """

  alias Anoma.Node.Router

  defmacro __using__(_) do
    quote do
      # a connection is a type of engine
      # more to come maybe?
      use Anoma.Node.Router.Engine, restart: :transient
    end
  end

  ############################################################
  #                      Public RPC API                      #
  ############################################################

  @doc """
  I send messages to the given connection.
  """
  @spec send(Router.addr(), binary()) :: :ok
  def send(conn, msg) do
    Router.cast(conn, {:send, msg})
  end

  @doc """
  I asynchronously initiate the connection shutdown.
  """
  @spec shutdown(Router.addr()) :: :ok
  def shutdown(conn) do
    Router.cast(conn, :shutdown)
  end
end
