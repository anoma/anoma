defmodule Anoma.Resource.Proof do
  @moduledoc false

  use TypedStruct

  # a transparent resource logic proof is just the resource
  typedstruct enforce: true do
    field(:resource, Anoma.Resource.t())
  end
end
