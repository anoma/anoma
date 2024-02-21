defmodule Anoma.Identity.Encryption do
  @moduledoc """
  I am responsible for encrypting messages to external identities. It
  automatically uses "reads for" relationship information from the
  Reads For Engine along with caller preference information in order
  to choose which identity to encrypt to.
  """

  alias Anoma.Crypto.Id

  @spec seal(term(), Id.Extern.t(), any()) :: binary()
  def seal(data, id, readsForTable) do
    id_to_use = (readsForTable && read_for_lookup(id, readsForTable)) || id
    Id.seal(data, id_to_use)
  end

  defp read_for_lookup(id, _table) do
    id
  end
end
