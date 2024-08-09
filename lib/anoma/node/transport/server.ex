defmodule Anoma.Node.Transport.Server do
  @moduledoc """
  I am an abstract interface that servers obey.
  """

  defmacro __using__(_) do
    quote do
      # a server is a type of engine
      # more to come maybe?
      use Anoma.Node.Router.Engine
    end
  end
end
