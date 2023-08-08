defmodule Taiga.NoteType do
  alias __MODULE__
  use TypedStruct
  alias Taiga.Base

  typedstruct do
    field(:app_vk, Base.t(), default: Base.default())
    field(:app_data_static, Base.t(), default: Base.default())
  end

  @spec default() :: t()
  def default() do
    %NoteType{}
  end
end
