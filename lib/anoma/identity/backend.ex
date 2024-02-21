defmodule Anoma.Identity.Backend do
  @moduledoc """
  I determine which backend to use in order to generate or connect an identity.
  """
  alias Anoma.Identity.Backend.{Memory, Remote, Local}
  alias Anoma.Crypto.Symmetric
  use TypedStruct

  @type t() :: Memory.t() | Local.t() | Remote.t()

  defmodule Memory do
    @moduledoc """
    I keep the identity in memory. I may or may not persisting across
    reboots. This depends if the table is set to disc_only copies, or
    memory_copies
    """
    @typedoc """

    We determine the information needed to properly store a memory copy.

    ### Fields

      - `symmetric` - this is a symmetric encryption/decryption key
        known by the user and the system. This key will be used to
        encrypt the public and private key, and along with the
        `External.t/1` can index into table

     - `table` - this is the local table in which where in memory this
       will be stored

    """
    typedstruct enforce: true do
      field(:symmetric, Symmetric.t())
      field(:nonce, binary(), default: nil)
      field(:table, atom())
    end
  end

  defmodule Local do
    @moduledoc """
    I represent generating the keys on some sort of local storage that
    is connected to Anoma
    """
    @type t() :: :ledger | :wallet | atom()
  end

  defmodule Remote do
    alias Anoma.Crypto.Id

    @moduledoc """
    I denote that the identity creation should be routed to some
    `t:Anoma.Crypto.Id.Extern.t/0`
    """
    typedstruct enforce: true do
      field(:id, Id.Extern.t())
    end
  end
end
