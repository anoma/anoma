defmodule AnomaIntentLab.Intent do
  @moduledoc """
  Minimal intent modeli:
    * `owner`: atom veya string
    * `give`: %{asset => miktar}
    * `want`: %{asset => miktar}
    * `constraints`: %{domain: "local:..."} gibi basit kısıtlar
  """
  @enforce_keys [:owner, :want, :give]
  defstruct [:owner, :want, :give, constraints: %{}, metadata: %{}]

  @type t :: %__MODULE__{
          owner: term(),
          want: map(),
          give: map(),
          constraints: map(),
          metadata: map()
        }

  def domain(%__MODULE__{constraints: c}), do: Map.get(c, :domain)
end
