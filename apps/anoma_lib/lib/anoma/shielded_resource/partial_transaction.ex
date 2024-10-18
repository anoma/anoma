defmodule Anoma.ShieldedResource.PartialTransaction do
  @moduledoc """
  I am a shielded resource machine partial transaction.
  """

  @behaviour Noun.Nounable.Kind

  require Logger

  alias __MODULE__
  use TypedStruct
  alias Anoma.ShieldedResource.ProofRecord

  typedstruct enforce: true do
    field(:logic_proofs, list(ProofRecord.t()), default: [])
    field(:compliance_proofs, list(ProofRecord.t()), default: [])
  end

  @spec from_noun(Noun.t()) :: {:ok, t()} | :error
  def from_noun([
        logic_proofs
        | compliance_proofs
      ]) do
    to_noun_list = fn xs ->
      xs
      |> Noun.list_nock_to_erlang()
      |> Enum.map(&ProofRecord.from_noun/1)
    end

    logic = to_noun_list.(logic_proofs)
    compilance = to_noun_list.(compliance_proofs)
    checked = Enum.all?(logic ++ compilance, &(elem(&1, 0) == :ok))

    with true <- checked do
      {:ok,
       %PartialTransaction{
         logic_proofs: Enum.map(logic, &elem(&1, 1)),
         compliance_proofs: Enum.map(compilance, &elem(&1, 1))
       }}
    else
      false -> :error
    end
  end

  @spec verify(t()) :: boolean()
  def verify(partial_transaction) do
    with true <- verify_proofs(partial_transaction.logic_proofs),
         true <-
           verify_proofs(
             partial_transaction.compliance_proofs,
             "compliance result"
           ) do
      true
    else
      _ -> false
    end
  end

  @spec verify_proofs(list(ProofRecord.t()), binary() | nil) :: boolean()
  defp verify_proofs(proofs, debug_msg \\ nil) do
    Enum.reduce_while(proofs, true, fn proof_record, _acc ->
      public_inputs =
        proof_record.public_inputs
        |> :binary.bin_to_list()

      res =
        proof_record.proof
        |> :binary.bin_to_list()
        |> Cairo.verify(public_inputs)

      if debug_msg do
        Logger.debug("#{debug_msg}: #{inspect(res)}")
      end

      case res do
        true -> {:cont, true}
        false -> {:halt, false}
        {:error, _} -> {:halt, false}
      end
    end)
  end

  defimpl Noun.Nounable, for: __MODULE__ do
    def to_noun(ptx = %PartialTransaction{}) do
      {ptx.logic_proofs, ptx.compliance_proofs}
      |> Noun.Nounable.to_noun()
    end
  end
end
