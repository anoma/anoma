defmodule Taiga.Native do
  use Rustler, otp_app: :anoma, crate: "nif_taiga"
  def add(_a, _b), do: error()
  def proof_new(_proof), do: error()
  def note_to_rust(_note), do: error()
  def note_random_input(), do: error()
  def note_random_output(), do: error()
  defp error, do: :erlang.nif_error(:nif_not_included)
end
