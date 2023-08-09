defmodule Taiga.Native do
  use Rustler, otp_app: :anoma, crate: "nif_taiga"
  def add(_a, _b), do: error()
  def proof_new(_proof), do: error()
  def note_to_rust(_note), do: error()
  def note_random_input(), do: error()
  def note_random_output(), do: error()
  def build_transaction(_inputs, _outputs), do: error()
  def note_commitment(_note), do: error()
  def note_nullifier(_note), do: error()
  def commitment_x(_commitment), do: error()
  defp error, do: :erlang.nif_error(:nif_not_included)
end
