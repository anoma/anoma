defmodule Anoma.Node.Transaction.Narwhal.Config do
  @moduledoc """
  I am the Narwhal network configuration.

  I hold the validator set, this node's keypair, and derived BFT
  thresholds. Each node receives one Config at boot.

  ### Public API

  - `n/1`                  - Total number of validators.
  - `f/1`                  - Max faulty validators tolerated.
  - `quorum/1`             - Quorum size (2f+1).
  - `commit_threshold/1`   - Bullshark commit threshold (f+1).
  - `sorted_validators/1`  - Sorted public key list.
  - `generate_configs/1`   - Generate fresh validator configs.

  ### Fields

  - `:node_id`       - The Anoma Node ID this config belongs to.
  - `:public_key`    - This validator's Ed25519 public key.
  - `:secret_key`    - This validator's Ed25519 secret key.
  - `:validator_set` - MapSet of all validator public keys.
  - `:node_id_set`   - MapSet of all validator node IDs.
  """

  alias Anoma.Crypto.Sign

  use TypedStruct

  typedstruct enforce: true do
    field(:node_id, String.t())
    field(:public_key, binary())
    field(:secret_key, binary())
    field(:validator_set, MapSet.t(binary()))
    field(:node_id_set, MapSet.t(String.t()))
    field(:pk_to_node_id, %{binary() => String.t()})

    field(
      :pk_to_address,
      %{binary() => %{host: String.t(), port: pos_integer()}},
      default: %{}
    )
  end

  @doc """
  I return the total number of validators.
  """
  @spec n(t()) :: pos_integer()
  def n(%__MODULE__{validator_set: vs}), do: MapSet.size(vs)

  @doc """
  I return the maximum number of faulty validators tolerated.
  """
  @spec f(t()) :: non_neg_integer()
  def f(config), do: div(n(config) - 1, 3)

  @doc """
  I return the quorum size (2f+1) needed for certificate formation.
  """
  @spec quorum(t()) :: pos_integer()
  def quorum(config), do: 2 * f(config) + 1

  @doc """
  I return the commit threshold (f+1) needed for Bullshark anchor commits.
  """
  @spec commit_threshold(t()) :: pos_integer()
  def commit_threshold(config), do: f(config) + 1

  @doc """
  I create a single-validator config for the given node_id.

  The node becomes its own solo validator: n=1, f=0, quorum=1,
  commit_threshold=1. Used when starting a node with Narwhal
  consensus but no external validator set.
  """
  @spec single_validator(String.t()) :: t()
  def single_validator(node_id) do
    kp = Sign.new_keypair()

    %__MODULE__{
      node_id: node_id,
      public_key: kp.public,
      secret_key: kp.secret,
      validator_set: MapSet.new([kp.public]),
      node_id_set: MapSet.new([node_id]),
      pk_to_node_id: %{kp.public => node_id}
    }
  end

  @doc """
  I generate `count` Narwhal configs with fresh Ed25519 keypairs.

  Returns a list of Config structs, one per validator. Each has its own
  keypair and the full validator set. Node IDs are generated as
  "narwhal_0", "narwhal_1", etc.
  """
  @spec generate_configs(pos_integer()) :: [t()]
  def generate_configs(count) when count > 0 do
    keypairs =
      for _ <- 1..count do
        Sign.new_keypair()
      end

    validator_set = MapSet.new(keypairs, & &1.public)
    prefix = Base.encode16(:crypto.strong_rand_bytes(4))

    node_id_list = for i <- 0..(count - 1), do: "narwhal_#{prefix}_#{i}"
    node_ids = MapSet.new(node_id_list)

    pairs = Enum.zip(keypairs, node_id_list)
    pk_to_node_id = Map.new(pairs, fn {kp, nid} -> {kp.public, nid} end)

    base_port = Application.get_env(:anoma_node, :grpc_port, 50_051)

    pk_to_address =
      pairs
      |> Enum.with_index()
      |> Map.new(fn {{kp, _nid}, i} ->
        {kp.public, %{host: "localhost", port: base_port + i * 1500}}
      end)

    Enum.map(pairs, fn {kp, nid} ->
      %__MODULE__{
        node_id: nid,
        public_key: kp.public,
        secret_key: kp.secret,
        validator_set: validator_set,
        node_id_set: node_ids,
        pk_to_node_id: pk_to_node_id,
        pk_to_address: pk_to_address
      }
    end)
  end

  @doc """
  I return a sorted list of validator public keys.

  Used for deterministic leader election in Bullshark.
  """
  @spec sorted_validators(t()) :: [binary()]
  def sorted_validators(%__MODULE__{validator_set: vs}) do
    vs |> MapSet.to_list() |> Enum.sort()
  end
end
