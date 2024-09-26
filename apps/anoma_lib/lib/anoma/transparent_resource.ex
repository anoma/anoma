defmodule Anoma.TransparentResource do
  import alias Anoma.TransparentResource.Transaction

  def verify(%Transaction{}) do
    true
  end
end
