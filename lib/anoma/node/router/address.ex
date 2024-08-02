defmodule Anoma.Node.Router.Addr do
  @moduledoc """
  I am a module providing address information for the Engine and Node
  networking used by the Router.

  The server, if known, is a local actor which can receive it directly;
  otherwise, the mssage will be sent via the central router.

  If the server is known, but the id is not, then this is a local-only
  engine, which can only talk to other local engines.

  ### Public API

  I provide the follwing public functionality:

  - `id/1`
  - `id!/1`
  - `server/1`
  - `server!/1`
  - `pid/1`
  - `pid!/1`
  - `noncanonical_server/1`
  """

  alias Anoma.Crypto.Id

  @typedoc """
  I am the type for general process identifications.

  I posess the minimal information needed to send some agent a message
  through the Anoma networking structure.

  ### Options

  - `pid()` - The raw PID
  - `atom()` - The registered name
  - `Id.Extern.t()` - The Anoma identification
  - ` %Addr
      {
       id: Id.Extern.t(),
       server: Process.dest()
       }`  - The full address structure with its Anoma ID and server.
  """
  @type t() ::
          pid()
          | atom()
          | Id.Extern.t()
          | %__MODULE__{
              id: Id.Extern.t(),
              server: Process.dest()
            }
  # uncomment the below enforce_keys once we use the accessors correctly
  # throughout the codebase
  # @enforce_keys [:id, :server]
  defstruct [:id, :server]

  @doc """
  I am a function printing PIDs of an address with a guarded side-effect.

  I use `pid/1` on an address and print an error message if appropriate PID
  does not exist.
  """

  @spec pid!(t()) :: Process.dest()
  def pid!(addr) do
    pid(addr) || exit("no pid for #{inspect(addr)}")
  end

  @doc """
  I am a function printing IDs of an address with a guarded side-effect.

  I use `id/1` on an address and print an error message if appropriate ID
  does not exist.
  """

  @spec id!(t()) :: Id.Extern.t()
  def id!(addr) do
    id(addr) || exit("no id for #{inspect(addr)}")
  end

  @doc """
  I am a function printing servers of an address with a guarded side-effect.

  I use `server/1` on an address and print an error message if appropriate
  ID does not exist.
  """

  @spec server!(t()) :: Process.dest()
  def server!(addr) do
    server(addr) || exit("no server for #{inspect(addr)}")
  end

  @doc """
  I am an ID printing function.

  ### Pattern-Matching Variations

  - `id(%Id.Extern{})` - If I match on an external identity, I print it.
  - `id(%Addr{id: id})` - If I match on an Address struct with an `id`
                          field, I print said field
  - `id(x)` when is_pid(x) or is_atom(x) - If I match on a PID or atom, I
                                           print nothing.
  """

  @spec id(t()) :: Id.Extern.t() | nil
  def id(id = %Id.Extern{}) do
    id
  end

  def id(%__MODULE__{id: id}) do
    id
  end

  def id(x) when is_pid(x) or is_atom(x) do
    nil
  end

  @doc """
  I am a function printing the server of the given Address attempting to
  find its canonical registered name.

  Given an address, I use `noncanonical_server/1` to see its associated pid
  or name. If I get a pidm then I print the name of the process associated
  with said pid by checking Erlang's `process_info/2` in case there is any,
  otherwise returning the original PID.

  Else, I print the returned value (`nil`).
  """

  @spec server(t()) :: Process.dest() | nil
  def server(addr) do
    case noncanonical_server(addr) do
      pid when is_pid(pid) ->
        with {:registered_name, name} <-
               :erlang.process_info(pid, :registered_name) do
          name
        else
          _ ->
            pid
        end

      other ->
        other
    end
  end

  @doc """
  I am a function finding an associated PID to an address.

  Given an address I feed it to `noncanonical_server`. In case the returned
  value is a PID, I print it. If I get the server name, I use
  `Process.whereis/1` to search for its PID.

  If `nil` is returned, I print `nil`.
  """

  @spec pid(t()) :: pid() | nil
  def pid(addr) do
    case noncanonical_server(addr) do
      pid when is_pid(pid) -> pid
      nil -> nil
      name when is_atom(name) -> Process.whereis(name)
    end
  end

  @doc """
  I return some Process destination without attempting to canonalize the
  result, that is, I may return a PID.

  ### Pattern-Matching Variations

  - `noncanonical_server(%Id.Extern{})` - No server, so I print nil.
  - `noncanonical_server(%Addr{server: server})` - If an address has a
                                                   server I print it.
  - `noncanonical_server(server)` when is_atom(server) - I print the server
                                                        name as given.
  - `noncanonical_server(pid)` when is_pid(pid) - I print the pid.
  """

  @spec noncanonical_server(t()) :: Process.dest() | nil
  def noncanonical_server(%Id.Extern{}) do
    nil
  end

  def noncanonical_server(%__MODULE__{server: server}) do
    server
  end

  def noncanonical_server(server) when is_atom(server) do
    server
  end

  def noncanonical_server(pid) when is_pid(pid) do
    pid
  end
end
