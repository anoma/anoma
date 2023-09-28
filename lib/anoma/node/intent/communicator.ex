# TODO Find a way to make this generic across all communicators
defmodule Anoma.Node.Intent.Communicator do
  @moduledoc """


  """

  use GenServer
  use TypedStruct
  alias __MODULE__

  typedstruct do
    field(:subscribers, MapSet.t(GenServer.server()), default: MapSet.new())
    field(:pool, atom(), require: true)
  end

  def init(name: name, init: subscribers) do
    {:ok, %Communicator{pool: name, subscribers: subscribers}}
  end

  def start_link(arg) do
    # hack do better
    name = arg[:name]

    options =
      if name do
        [name: com_name(name)]
      else
        []
      end

    GenServer.start_link(__MODULE__, arg, options)
  end

  # TODO Move such logic to a generic utility module
  @spec com_name(atom()) :: atom()
  def com_name(name), do: (Atom.to_string(name) <> "_com") |> String.to_atom()
end
