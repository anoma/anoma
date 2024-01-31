defmodule Anoma.Identity.Encapsulated do
  @moduledoc """

  I contain the information necessary for engines like:
    - `Anoma.Node.Identity.Decryption`
    - `Anoma.Node.Identity.Commitment`

  to operate.

  To speak plainly, I contain the secret information necessary to
  decrypt and encrypt messages

  """
  use TypedStruct

  alias Anoma.Identity.Parameters

  typedstruct do
    field(:private, binary())
    field(:public, binary(), enforce: false)
    field(:type, Parameters.t())
  end
end
