defmodule Anoma.Node.Transport.Connection do
  @moduledoc """
  I am an abstract interface that connections obey.
  """

  alias Anoma.Node.Router

  defmacro __using__(_) do
    quote do
      # a connection is a type of engine
      # more to come maybe?
      use Anoma.Node.Router.Engine, restart: :transient
    end
  end

  @spec send(Router.addr(), binary()) :: :ok
  def send(conn, msg) do
    Router.cast(conn, {:send, msg})
  end

  @doc """
  Asynchronously initiate a shutdown.  Idempotent.
  """
  @spec shutdown(Router.addr()) :: :ok
  def shutdown(conn) do
    Router.cast(conn, :shutdown)
  end
end
