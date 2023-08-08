defmodule Taiga.Nullifier.Container do
  @moduledoc """
  Ι am a container for either a nullifier key or the container
  """

  alias Taiga.Base
  alias __MODULE__

  require Record

  Record.defrecord(:commitment, :commitment, base: Base.default())
  Record.defrecord(:key, :key, base: Base.default())

  @type t() :: {:key, Base.t()} | {:commitment, Base.t()}

  def default() do
    {:key, Base.default()}
  end
end
