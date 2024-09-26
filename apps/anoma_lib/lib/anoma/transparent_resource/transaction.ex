defmodule Anoma.TransparentResource.Transaction do
  use TypedStruct

  import alias Anoma.TransparentResource.Action
  import alias Anoma.TransparentResource.Delta

  typedstruct enforce: true do
    field(:roots, list(binary()), default: [])
    field(:actions, list(Action.t()), default: [])
    field(:delta, Delta.t(), default: %{})
    # useless field for shielded only.
    field(:delta_proof, <<>>, default: <<>>)
  end
end
