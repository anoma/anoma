defmodule Taiga.Base do
  # If we tag it then make it a defrecord
  @type t() ::
          {non_neg_integer(), non_neg_integer(), non_neg_integer(), non_neg_integer()}

  @spec default() :: t()
  def default() do
    {0, 0, 0, 0}
  end
end
