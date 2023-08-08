defmodule Taiga.Note do
  use TypedStruct

  alias Taiga.{NoteType, Base, Nullifier, Nullifier.Container}
  alias __MODULE__

  typedstruct do
    field(:note_type, Taiga.NoteType.t(), default: NoteType.default())
    field(:nk_container, Container.t(), default: Container.default())
    field(:app_data_dynamic, Base.t(), default: Base.default())
    field(:rho, Nullifier.t(), default: Nullifier.default())
    field(:psi, Base.t(), default: Base.default())
    field(:rcm, Base.t(), default: Base.default())
    field(:is_merkle_checked, boolean(), default: false)
    field(:value, non_neg_integer(), default: 0)
  end

  @spec default() :: t()
  def default() do
    %Note{}
  end

  defdelegate random_input(), to: Taiga.Native, as: :note_random_input
  defdelegate random_output(), to: Taiga.Native, as: :note_random_output
end
