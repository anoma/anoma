defmodule Examples.ECairo.EResourceLogic do
  alias Anoma.CairoResource.LogicInstance
  alias Anoma.CairoResource.ProofRecord
  alias Examples.ECairo.EProofRecord
  alias Examples.ECairo.EResource

  use Memoize
  use TestHelper.TestMacro

  @spec a_input_resource_logic() :: ProofRecord.t()
  defmemo a_input_resource_logic() do
    res =
      EProofRecord.a_resource_logic(
        "params/trivial_input_resource_logic_witness.json"
      )

    instance = res.instance |> LogicInstance.from_public_input()

    assert {:ok, plaintext} =
             LogicInstance.decrypt(instance.cipher, <<1::256>>)

    a_resource = EResource.a_fixed_resource()

    expected_text = [
      a_resource.logic_ref,
      a_resource.label_ref,
      a_resource.quantity,
      a_resource.value_ref,
      <<0::256>>,
      a_resource.nonce,
      a_resource.nk_commitment,
      a_resource.rand_seed,
      <<0::256>>,
      <<0::256>>
    ]

    assert expected_text == plaintext

    res
  end

  @spec a_output_resource_logic() :: ProofRecord.t()
  defmemo a_output_resource_logic() do
    EProofRecord.a_resource_logic(
      "params/trivial_output_resource_logic_witness.json"
    )
  end

  @spec an_input_intent_resource_logic() :: ProofRecord.t()
  defmemo an_input_intent_resource_logic() do
    EProofRecord.a_resource_logic(
      "params/trivial_input_intent_resource_logic_witness.json"
    )
  end

  @spec an_output_intent_resource_logic() :: ProofRecord.t()
  defmemo an_output_intent_resource_logic() do
    EProofRecord.a_resource_logic(
      "params/trivial_output_intent_resource_logic_witness.json"
    )
  end

  @spec a_resource_logic_invalid_proving_key() :: {:error, term()}
  def a_resource_logic_invalid_proving_key() do
    ret =
      ProofRecord.prove(
        "",
        ""
      )

    assert {:error, "Invalid program content"} = ret

    ret
  end

  @spec a_resource_logic_invalid_input() :: {:error, term()}
  def a_resource_logic_invalid_input() do
    proving_key_dir =
      Path.join(
        :code.priv_dir(:anoma_lib),
        "params/trivial_resource_logic.json"
      )

    assert {:ok, proving_key} = File.read(proving_key_dir)

    assert {:error, "Invalid input JSON"} =
             ProofRecord.prove(
               proving_key,
               "xxx"
             )

    assert {:error, "Runtime error: The cairo program execution failed"} =
             ProofRecord.prove(
               proving_key,
               ""
             )

    invalid_input = """
    {"resource_nf_key": "0x1"}
    """

    ret =
      ProofRecord.prove(
        proving_key,
        invalid_input
      )

    assert {:error, "Runtime error: The cairo program execution failed"} ==
             ret

    ret
  end
end
