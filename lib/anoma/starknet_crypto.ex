defmodule Anoma.StarknetCrypto do
  use Rustler,
    otp_app: :anoma,
    crate: :starknetcrypto

  def poseidon_single(_arg1), do: :erlang.nif_error(:nif_not_loaded)
  def poseidon(_arg1, _arg2), do: :erlang.nif_error(:nif_not_loaded)
  def poseidon_many(_arg1), do: :erlang.nif_error(:nif_not_loaded)
end