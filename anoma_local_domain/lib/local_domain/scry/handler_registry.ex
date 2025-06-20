defmodule Anoma.LocalDomain.HandlerRegistry do
  @moduledoc """
  Registers scry handling functions to key prefixes. e.g., /anoma/local ->
  Scry.scry_local

  Uses the least specific prefix first, then more specific ones?

  e.g. /anoma/local/[node id]/[time]/app-1/abc

  {prev_prefixes_list, prefix} e.g. {[/anoma/local], /app-1}
                                    {[/anoma/local, /app-1], /section-1}

  default registry:
  {[], /anoma/local} -> Scry.scry_local
  {[], /anoma/controller]} -> Scry.scry_controller

  Scry.scry_local({[], /anoma/local})
  App1.scry_app1({[/app-1], key})

  This should go to the /anoma/local handler first. However, this wants to
  later find a handler for app-1.

  If we match the shortest prefix, we always end up with the /anoma/local
  handler.

  If we match the longest prefix, we end up deep in app-1 right away.

  If we chop off the front of the key, app-1 can't be used by other keyspaces.
  """

  def initialize_tables(_) do
    # always populate the default /anoma/local and /anoma/controller handlers
    # then read the preferences of the user for any others
  end

  def register(prefix, fun) do
    # register a new prefix handler
  end

  def deregister(prefix) do
    # deregister the handler for some prefix
  end
end
