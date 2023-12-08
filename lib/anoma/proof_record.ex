defmodule Anoma.ProofRecord do
  @moduledoc false

  alias __MODULE__
  use TypedStruct

  alias Anoma.Proof

  typedstruct enforce: true do
    field(:proof, Anoma.Proof.t(), default: nil)
  end

  def prove(resource) do
    %ProofRecord{
      proof: %Proof{
        resource: resource
      }
    }
  end
end
