defmodule Anoma.CairoResource.Workflow do
  @moduledoc """
  I am a bunch of workflow functions for creating a shielded transaction.
  """

  alias Anoma.CairoResource.Action
  alias Anoma.CairoResource.ProofRecord
  alias Anoma.CairoResource.Resource
  alias Anoma.CairoResource.Tree
  alias Anoma.CairoResource.Utils

  require Logger

  @spec hash_resource_logic(binary()) :: {:ok, binary()} | {:error, term()}
  def hash_resource_logic(resource_logic_bin) do
    with {:ok, resource_logic} <-
           Jason.decode(resource_logic_bin, objects: :ordered_objects),
         {:ok, program} <-
           resource_logic["data"]
           |> Enum.map(&Utils.hex_to_n_byte_binary(&1, 32))
           |> Utils.check_list(),
         hash =
           Enum.drop(program, -2)
           |> Enum.map(&:binary.bin_to_list/1)
           |> Cairo.poseidon_many()
           |> :binary.list_to_bin() do
      {:ok, hash}
    else
      {:error, msg} -> {:error, "Error hashing resource logic: #{msg}"}
    end
  end

  @spec update_resource_jsons(list(Jason.OrderedObject.t()), list(binary())) ::
          list(Jason.OrderedObject.t())
  defp update_resource_jsons(jsons, logic_hashes) do
    hex_logic_hashes =
      Enum.map(logic_hashes, &Utils.binary_to_hex/1)

    Enum.zip_with(
      jsons,
      hex_logic_hashes,
      &put_in(&1["logic"], &2)
    )
  end

  @spec update_nk_commitment(Jason.OrderedObject.t(), binary()) ::
          Jason.OrderedObject.t()
  defp update_nk_commitment(json, nf_key) do
    if Utils.json_object_has_nonempty_key(json, "nk_commitment") do
      json
    else
      nk_commitment =
        nf_key
        |> Resource.get_nk_commitment()
        |> Utils.binary_to_hex()

      put_in(json["nk_commitment"], nk_commitment)
    end
  end

  @spec update_nonce(Jason.OrderedObject.t(), binary()) ::
          Jason.OrderedObject.t()
  defp update_nonce(json, input_nullifier) do
    if Utils.json_object_has_nonempty_key(json, "nonce") do
      json
    else
      put_in(json["nonce"], Utils.binary_to_hex(input_nullifier))
    end
  end

  @spec update_input_resource_jsons(
          list(Jason.OrderedObject.t()),
          list(binary()),
          list(binary())
        ) ::
          list(Jason.OrderedObject.t())
  defp update_input_resource_jsons(jsons, logic_hashes, input_nf_keys) do
    jsons1 = update_resource_jsons(jsons, logic_hashes)

    Enum.zip_with(
      jsons1,
      input_nf_keys,
      &update_nk_commitment(&1, &2)
    )
  end

  @spec update_output_resource_jsons(
          list(Jason.OrderedObject.t()),
          list(binary()),
          list(binary())
        ) ::
          list(Jason.OrderedObject.t())
  defp update_output_resource_jsons(jsons, logic_hashes, input_nullifiers) do
    jsons1 = update_resource_jsons(jsons, logic_hashes)

    Enum.zip_with(
      jsons1,
      input_nullifiers,
      &update_nonce(&1, &2)
    )
  end

  @spec get_resources(
          (list(Jason.OrderedObject.t()), list(binary()) ->
             list(Jason.OrderedObject.t())),
          list(Jason.OrderedObject.t()),
          list(binary())
        ) ::
          {:ok, list(Resource.t())}
          | {:error, term()}
  defp get_resources(update, jsons, resource_logics) do
    with {:ok, logic_hashes} <-
           Enum.map(resource_logics, &hash_resource_logic/1)
           |> Utils.check_list(),
         resource_jsons = update.(jsons, logic_hashes),
         resources = Enum.map(resource_jsons, &Resource.from_json_object/1) do
      {:ok, resources}
    else
      {:error, msg} -> {:error, "Error reading resources: #{msg}"}
    end
  end

  @spec get_input_resources(
          list(Jason.OrderedObject.t()),
          list(binary()),
          list(binary())
        ) ::
          {:ok, list(Resource.t())}
          | {:error, term()}
  def get_input_resources(jsons, resource_logics, input_nf_keys) do
    get_resources(
      &update_input_resource_jsons(&1, &2, input_nf_keys),
      jsons,
      resource_logics
    )
  end

  @spec get_output_resources(
          list(Jason.OrderedObject.t()),
          list(binary()),
          list(binary())
        ) ::
          {:ok, list(Resource.t())}
          | {:error, term()}
  def get_output_resources(jsons, resource_logics, input_nullifiers) do
    get_resources(
      &update_output_resource_jsons(&1, &2, input_nullifiers),
      jsons,
      resource_logics
    )
  end

  @spec create_path(Tree.t(), binary()) ::
          {:ok, list(Jason.OrderedObject.t())} | {:error, term()}
  defp create_path(rt, nf) do
    lst = Tree.prove(rt, nf)

    with false <- is_nil(lst) do
      {:ok,
       Enum.map(lst, fn {f, s} ->
         Jason.OrderedObject.new([{"fst", f}, {"snd", s}])
       end)}
    else
      _ -> {:error, "Error creating path"}
    end
  end

  @spec create_merkle_tree_paths(
          list(binary()),
          list(binary())
        ) ::
          {:ok, list(list(Jason.OrderedObject.t())),
           list(list(Jason.OrderedObject.t()))}
          | {:error, term()}
  def create_merkle_tree_paths(input_nullifiers, output_commitments) do
    rt =
      Tree.construct(
        CommitmentTree.Spec.cairo_poseidon_resource_tree_spec(),
        Enum.zip(input_nullifiers, output_commitments)
        |> Enum.flat_map(fn {i, o} -> [i, o] end)
      )

    with {:ok, input_paths} <-
           Enum.map(input_nullifiers, &create_path(rt, &1))
           |> Utils.check_list(),
         {:ok, output_paths} <-
           Enum.map(output_commitments, &create_path(rt, &1))
           |> Utils.check_list() do
      {:ok, input_paths, output_paths}
    else
      {:error, msg} -> {:error, "Error creating merkle tree paths: #{msg}"}
    end
  end

  @spec update_witness_json(
          Jason.OrderedObject.t(),
          Resource.t(),
          binary(),
          list(Jason.OrderedObject.t())
        ) ::
          Jason.OrderedObject.t()
  defp update_witness_json(witness_json, resource, nf_key, merkle_path) do
    witness1 =
      if Utils.json_object_has_empty_key(witness_json, "self_resource") do
        put_in(
          witness_json["self_resource"],
          Resource.to_json_object(resource)
        )
      else
        witness_json
      end

    witness2 =
      if Utils.json_object_has_empty_key(witness1, "resource_nf_key") do
        put_in(witness1["resource_nf_key"], Utils.binary_to_hex(nf_key))
      else
        witness1
      end

    witness3 =
      if Utils.json_object_has_empty_key(witness2, "merkle_path") do
        put_in(witness2["merkle_path"], merkle_path)
      else
        witness2
      end

    witness3
  end

  @spec update_witnesses(
          list(binary()),
          list(Resource.t()),
          list(binary()),
          list(list(Jason.OrderedObject.t()))
        ) ::
          {:ok, list(binary())} | {:error, term()}
  def update_witnesses(witnesses, resources, nf_keys, merkle_paths) do
    with {:ok, witness_jsons} <-
           Enum.map(
             witnesses,
             &Jason.decode(&1, objects: :ordered_objects)
           )
           |> Utils.check_list(),
         updated_witness_jsons =
           Enum.zip_with(
             witness_jsons,
             Enum.zip(resources, Enum.zip(nf_keys, merkle_paths)),
             fn json, {r, {nk, p}} -> update_witness_json(json, r, nk, p) end
           ),
         {:ok, updated_witnesses} <-
           Enum.map(updated_witness_jsons, &Jason.encode/1)
           |> Utils.check_list() do
      {:ok, updated_witnesses}
    else
      {:error, msg} -> {:error, "Error updating witnesses: #{msg}"}
    end
  end

  @spec generate_resource_logic_proofs(list(binary()), list(binary())) ::
          {:ok, list(ProofRecord.t())} | {:error, term()}
  def generate_resource_logic_proofs(
        resource_logics,
        resource_logic_witnesses
      ) do
    with {:ok, proofs} <-
           Enum.zip_with(
             resource_logics,
             resource_logic_witnesses,
             &ProofRecord.generate_cairo_proof/2
           )
           |> Utils.check_list() do
      {:ok, proofs}
    else
      {:error, msg} ->
        {:error, "Error generating resource logic proofs: #{msg}"}
    end
  end

  @spec create_compliance_inputs(
          list(Jason.OrderedObject.t()),
          list(Resource.t()),
          list(Resource.t())
        ) :: {:ok, list(String.t())} | {:error, term()}
  def create_compliance_inputs(
        compliance_pre_inputs,
        input_resources,
        output_resources
      ) do
    with {:ok, compliance_inputs} <-
           Enum.zip_with(
             compliance_pre_inputs,
             output_resources,
             &put_in(&1["output"], Resource.to_json_object(&2))
           )
           |> Enum.zip_with(
             input_resources,
             &put_in(&1["input"], Resource.to_json_object(&2))
           )
           |> Enum.map(&Jason.encode/1)
           |> Utils.check_list() do
      {:ok, compliance_inputs}
    else
      {:error, msg} -> {:error, "Error creating compliance inputs: #{msg}"}
    end
  end

  @spec generate_compliance_proofs(list(String.t())) ::
          {:ok, list(ProofRecord.t())} | {:error, term()}
  def generate_compliance_proofs(compliance_inputs) do
    with {:ok, compliance_proofs} <-
           Enum.map(
             compliance_inputs,
             &ProofRecord.generate_compliance_proof/1
           )
           |> Utils.check_list() do
      {:ok, compliance_proofs}
    else
      {:error, msg} -> {:error, "Error creating compliance proofs: #{msg}"}
    end
  end

  @spec create_action(
          list(ProofRecord.t()),
          list(ProofRecord.t()),
          list(ProofRecord.t())
        ) :: Action.t()
  def create_action(
        input_logic_proofs,
        output_logic_proofs,
        compliance_proofs
      ) do
    Action.new(
      Enum.concat(
        input_logic_proofs,
        output_logic_proofs
      ),
      compliance_proofs
    )
  end

  @spec create_private_keys(list(Jason.OrderedObject.t())) ::
          {:ok, binary()} | {:error, term()}
  def create_private_keys(compliance_input_jsons) do
    with {:ok, priv_keys} <-
           Enum.map(compliance_input_jsons, & &1["rcv"])
           |> Enum.map(&Utils.hex_to_n_byte_binary(&1, 32))
           |> Utils.check_list() do
      {:ok, priv_keys |> Enum.reduce(&<>/2)}
    else
      {:error, msg} -> {:error, "Error creating private keys: #{msg}"}
    end
  end
end
