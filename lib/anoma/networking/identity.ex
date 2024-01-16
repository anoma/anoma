defmodule Anoma.Networking.Identity do
  use TypedStruct
  alias Anoma.Networking.Advert
  alias Anoma.Networking.Identity.{Peer, Engine, Domain, Topic, External}

  @type t() :: Engine.t() | Peer.t() | Domain.t() | Topic.t()

  typedstruct module: Peer, enforce: true do
    field(:id, External.t())
    field(:advert, Advert.peer())
    field(:trust, Anoma.Networking.Trust.t())
    field(:tprefs, any(), default: nil)
    field(:cprefs, any(), default: nil)
  end

  typedstruct module: Engine, enforce: true do
    field(:id, External.t())
    field(:addr, binary())
  end

  typedstruct module: Domain, enforce: true do
    field(:id, External.t())
    field(:advert, Advert.domain())
  end

  typedstruct module: Topic, enforce: true do
    field(:id, External.t())
    field(:advert, any())
  end

  # %Turn this into a file later

  defmodule External do
    @type t() :: binary
  end
end
