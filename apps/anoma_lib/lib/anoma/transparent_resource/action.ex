defmodule Anoma.TransparentResource.Action do
  use TypedStruct

  import alias Anoma.TransparentResource.Proof

  typedstruct enforce: true do
    field(:commitments, list(binary()), default: [])
    field(:nullifiers, list(binary()), default: [])
    field(:proofs, list(Proof.t()), default: [])
    field(:app_data, binary(), default: <<>>)
  end
end
