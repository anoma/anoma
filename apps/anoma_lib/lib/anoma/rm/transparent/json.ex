# ----------------------------------------------------------------------------
# Resource

defimpl Jason.Encoder, for: Anoma.RM.Transparent.Resource do
  def encode(resource, opts) do
    resource =
      resource
      |> Map.update!(:nonce, &Base.encode64/1)
      |> Map.update!(:nullifierkeycommitment, &Base.encode64/1)

    Jason.Encode.map(resource, opts)
  end
end

# ----------------------------------------------------------------------------
# CPS Instance
defimpl Jason.Encoder, for: Anoma.RM.Transparent.ProvingSystem.CPS.Instance do
  def encode(instance, opts) do
    # encode the consumed resources
    consumed =
      instance.consumed
      |> Enum.map(fn {nullifier, root, logic} ->
        %{nullifier: nullifier, root: root, logic: logic}
      end)

    # encode the created resources
    created =
      instance.created
      |> Enum.map(fn {commitment, logic} ->
        %{commitment: commitment, logic: logic}
      end)

    Jason.Encode.map(%{instance | consumed: consumed, created: created}, opts)
  end
end

# ----------------------------------------------------------------------------
# Compliance Unit

defimpl Jason.Encoder, for: Anoma.RM.Transparent.ComplianceUnit do
  def encode(compliance_unit, opts) do
    # encode the consumed resources
    compliance_unit
    |> Map.update!(:proof, &Base.encode64/1)
    |> Map.update!(:vk, &Base.encode64/1)
    |> Jason.Encode.map(opts)
  end
end

# ----------------------------------------------------------------------------
# Action

defimpl Jason.Encoder, for: Anoma.RM.Transparent.Action do
  alias Anoma.RM.Transparent.Resource
  alias Anoma.RM.Transparent.Action

  @spec decode_nullifier(integer()) :: Resource.t()
  defp decode_nullifier(nullifier) do
    <<"NF_", bin::binary>> = :binary.encode_unsigned(nullifier, :little)

    {:ok, resource} =
      bin
      |> Noun.Jam.cue!()
      |> Resource.from_noun()

    resource
  end

  @spec decode_commitment(integer()) :: Resource.t()
  defp decode_commitment(nullifier) do
    <<"CM_", bin::binary>> = :binary.encode_unsigned(nullifier, :little)

    {:ok, resource} =
      bin
      |> Noun.Jam.cue!()
      |> Resource.from_noun()

    resource
  end

  # @doc """
  # This extra info is tempory for the solver. It will be removed once
  # created/consumed is no longer available. If you are reading this because you
  # just changed the encoding of created and consumed resources, you can safely
  # comment this out, but this will break the solver. Talk to m1dnight or artem
  # about this.
  # """
  @spec extra_info(Action.t()) :: map()
  defp extra_info(action) do
    consumed_resources = Enum.map(action.consumed, &decode_nullifier/1)
    created_resources = Enum.map(action.created, &decode_commitment/1)

    %{
      created_resources: created_resources,
      consumed_resources: consumed_resources
    }
  end

  # https://specs.anoma.net/latest/arch/system/state/resource_machine/data_structures/action/index.html
  def encode(action, opts) do
    extra_info = extra_info(action)

    # encode the resource logic proofs.
    resource_logic_proofs =
      action.resource_logic_proofs
      |> Enum.map(fn {tag, {logic_ref, proof}} ->
        {tag, %{logic_ref: logic_ref, proof: Base.encode64(proof)}}
      end)
      |> Enum.into(%{})

    # encode the delta proof
    app_data =
      action.app_data
      |> Enum.map(fn {tag, data} ->
        data =
          Enum.map(data, fn {data, deletion_criteria} ->
            %{
              data: Base.encode64(data),
              deletation_criteria: deletion_criteria
            }
          end)

        {tag, data}
      end)
      |> Enum.into(%{})

    action
    |> Map.put(:resource_logic_proofs, resource_logic_proofs)
    |> Map.put(:app_data, app_data)
    |> Map.merge(extra_info)
    |> Jason.Encode.map(opts)
  end

  # ----------------------------------------------------------------------------
  # Action

  defimpl Jason.Encoder, for: Anoma.RM.Transparent.Transaction do
    def encode(transaction, opts) do
      transaction
      |> Map.update!(:delta_proof, &Base.encode64/1)
      |> Jason.Encode.map(opts)
    end
  end
end
