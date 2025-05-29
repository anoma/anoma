defmodule Anoma.CairoResource.Delta.ProvingSystem do
  @moduledoc """
  I represent the delta proving system.
  """

  alias Anoma.CairoResource.Delta.Instance

  @spec prove(<<>>, Instance.t(), binary()) :: binary()
  def prove(_proving_key, instance, witness) do
    witness
    |> :binary.bin_to_list()
    |> Cairo.sign(instance.tx_digest |> Enum.map(&:binary.bin_to_list/1))
    |> :binary.list_to_bin()
  end

  @spec verify(<<>>, Instance.t(), binary()) :: true | {:error, String.t()}
  def verify(_verifying_key, instance, proof) do
    case Cairo.sig_verify(
           instance.delta,
           instance.tx_digest |> Enum.map(&:binary.bin_to_list/1),
           proof |> :binary.bin_to_list()
         ) do
      true -> true
      _ -> {:error, "Delta proof verification failure"}
    end
  end

  @spec key() :: binary()
  def key() do
    <<>>
  end
end
