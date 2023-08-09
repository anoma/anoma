defmodule Taiga.Note.Commitment do
  alias Taiga.Base
  @type t() :: {{:Ep, Base.t(), Base.t(), Base.t()}}

  @spec default() :: t()
  def default() do
    {{:Ep, Base.default(), Base.default(), Base.default()}}
  end

  @spec x(t()) :: Base.t()
  defdelegate x(commitment), to: Taiga.Native, as: :commitment_x
end
