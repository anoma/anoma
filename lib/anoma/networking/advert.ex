defmodule Anoma.Networking.Advert do
  @enforce_keys [:id, :peers, :version, :created, :sig]
  defstruct [:id, :peers, :version, :created, :sig]

  @type t(value) ::
          %__MODULE__{
            id: binary(),
            peers: value,
            version: integer(),
            created: DateTime.t(),
            sig: binary()
          }

  @type peer() :: t({any(), Anoma.Networking.Trust.t()})
  @type domain() :: t(binary())
end
