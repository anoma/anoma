defmodule Anoma.Node.Router.Addr do
  @moduledoc """
  An address to which we can send a message.
  The server, if known, is a local actor which can receive it directly;
  otherwise, the mssage will be sent via the central router.
  If the server is known, but the id is not, then this is a local-only
  engine, which can only talk to other local engines.
  (Hence, at least one of id and server must be known; potentially both are.)
  """

  alias Anoma.Crypto.Id
  alias Anoma.Crypto.Id

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

  # server, but try to canonicalise to a name if possible
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

  @spec pid(t()) :: pid() | nil
  def pid(addr) do
    case noncanonical_server(addr) do
      pid when is_pid(pid) -> pid
      nil -> nil
      name when is_atom(name) -> Process.whereis(name)
    end
  end

  # these always return or else error
  @spec id!(t()) :: Id.Extern.t()
  def id!(addr) do
    id(addr) || exit("no id for #{inspect(addr)}")
  end

  @spec server!(t()) :: Process.dest()
  def server!(addr) do
    server(addr) || exit("no server for #{inspect(addr)}")
  end

  @spec pid!(t()) :: Process.dest()
  def pid!(addr) do
    pid(addr) || exit("no pid for #{inspect(addr)}")
  end

  # returns some server--either a pid or a name, or nil if there is none--but
  # does not attempt to canonicalise it
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
