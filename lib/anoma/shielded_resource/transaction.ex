defmodule Anoma.ShieldedResource.Transaction do
  @moduledoc """
  I am a shielded resource machine transaction.
  """

  use TypedStruct
  alias Anoma.ShieldedResource.PartialTransaction

  typedstruct enforce: true do
    # TODO: The roots, commitments, and nullifiers can be eliminated. We can
    # obtain them from public inputs. Then we can make the same improvement for
    # transparent transactions. However, they are used in the executor atm.
    field(:roots, list(binary()), default: [])
    field(:commitments, list(binary()), default: [])
    field(:nullifiers, list(binary()), default: [])
    field(:partial_tx, list(PartialTransaction.t()), default: [])
    field(:delta, binary(), default: %{})
    field(:preference, term(), default: nil)
  end
end
