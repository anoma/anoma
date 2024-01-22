defmodule Anoma.Networking.Routing.Table do
  use TypedStruct
  alias Anoma.Networking.Identity.External

  # todo replace with something proper?
  @type pubsub :: any()

  @type t() :: External.t() | {pubsub(), list(External.t())}
end
