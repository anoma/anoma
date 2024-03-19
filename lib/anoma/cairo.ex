defmodule Anoma.Cairo do
  use Rustler,
    otp_app: :anoma,
    crate: :cairo

  @moduledoc """
  Documentation for `Cairo`.
  """

  # When loading a NIF module, dummy clauses for all NIF function are required.
  # NIF dummies usually just error out when called when the NIF is not loaded, as that should never normally happen.
  def cairo_run_and_prove(_arg1), do: :erlang.nif_error(:nif_not_loaded)
  def cairo_verify(_arg1, _arg2), do: :erlang.nif_error(:nif_not_loaded)
end
