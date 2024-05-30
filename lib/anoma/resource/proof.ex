defmodule Anoma.Resource.Proof do
  use TypedStruct

  @type proof_value() ::
          {:transparent, Anoma.Resource.t()}
          | {:binary, binary()}

  # a transparent resource logic proof is just the resource
  typedstruct enforce: true do
    field(:proof_value, proof_value())
  end
end
