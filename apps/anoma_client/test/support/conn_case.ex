defmodule Anoma.Client.Web.ConnCase do
  @moduledoc """
  This module defines the test case to be used by
  tests that require setting up a connection.

  Such tests rely on `Phoenix.ConnTest` and also
  import other functionality to make it easier
  to build common data structures and query the data layer.

  Finally, if the test case interacts with the database,
  we enable the SQL sandbox, so changes done to the database
  are reverted at the end of every test. If you are using
  PostgreSQL, you can even run database tests asynchronously
  by setting `use Anoma.Client.Web.ConnCase, async: true`, although
  this option is not recommended for other databases.
  """

  use ExUnit.CaseTemplate

  using do
    quote do
      # Import conveniences for testing with connections
      import Anoma.Client.Web.ConnCase
      import Phoenix.ConnTest
      import Plug.Conn

      alias Anoma.Client.Web.Router.Helpers, as: Routes

      use Phoenix.VerifiedRoutes,
        endpoint: Anoma.Client.Web.Endpoint,
        router: Anoma.Client.Web.Router,
        statics: Anoma.Client.Web.static_paths()

      # The default endpoint for testing
      @endpoint Anoma.Client.Web.Endpoint
    end
  end
end
