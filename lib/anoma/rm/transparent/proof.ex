defmodule Anoma.RM.Transparent.Proof do
  use TypedStruct

  # a transparent resource logic proof is just the resource
  typedstruct enforce: true do
    field(:resource, Anoma.RM.Resource.t())
  end
end
