defmodule Anoma.LocalDomain.Application do
  @moduledoc """
  I am the behaviour implemented by local domain applications.

  To make your module a local domain application, `use` this module,
  providing a name (e.g., `use Anoma.LocalDomain.Application, name: "sample"`,
  and, optionally, implement the following callbacks:

  - `init/0` contains any initialization logic your application needs to do.
  The default implementation takes care of important things like registering
  your application's storage key space; if you implement your own `init/0`
  callback, you should probably call `super()` at the top to take care of
  this. `init/0` is expected to return `:ok`; any other value means the
  application has failed to start.

  - `scry/2` lets you define handlers for reads from your keyspace. The
  default implementation simply reads from it; you would want to override
  this if some keys in your keyspace are computed rather than merely stored.
  """

  defmacro __using__(opts) do
    name = Keyword.fetch!(opts, :name)

    quote do
      use Anoma.LocalDomain
      @behaviour Anoma.LocalDomain.Application

      @anoma_application_name unquote(name)

      def init() do
        :ok =
          Anoma.LocalDomain.HandlerRegistry.register(
            {[~k"/anoma/local"], [@anoma_application_name]},
            &scry/2
          )
      end

      defoverridable init: 0

      def scry(_, _) do
        {:error, :no_handler}
      end

      defoverridable scry: 2
    end
  end

  @callback init() :: :ok | :error
  @callback scry(list(list(String.t())), list(String.t())) ::
              {:ok, term()}
              | :absent
              | {:error, term()}
end
