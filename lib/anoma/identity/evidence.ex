defmodule Anoma.Identity.Evidence do
  use TypedStruct
  alias Anoma.Crypto.Id

  @enforce_keys [:signature, :signature_key, :signed_data]
  defstruct signed_data: nil, signature_key: nil, signature: nil

  @type t(data) :: %__MODULE__{
          signed_data: data,
          signature_key: Id.Extern.t(),
          signature: binary()
        }

  @type name() :: t(Id.Extern.t())
end
