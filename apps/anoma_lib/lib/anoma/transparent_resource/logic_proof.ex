defmodule Anoma.TransparentResource.LogicProof do
  use TypedStruct

  @behaviour Noun.Nounable.Kind

  alias Anoma.TransparentResource.Resource
  alias __MODULE__

  typedstruct enforce: true do
    # the resource being proven
    field(:resource, Resource.t())
    # public inputs
    field(:commitments, MapSet.t(Resource.commitment()),
      default: MapSet.new()
    )

    field(:nullifiers, MapSet.t(Resource.nullifier()), default: MapSet.new())

    field(
      :self_tag,
      {:committed, Resource.commitment()}
      | {:nullified, Resource.commitment()}
    )

    field(:other_public, Noun.t(), default: <<>>)
    # private inputs
    field(:committed_plaintexts, MapSet.t(Resource.t()),
      default: MapSet.new()
    )

    field(:nullified_plaintexts, MapSet.t(Resource.t()),
      default: MapSet.new()
    )

    field(:other_private, Noun.t(), default: <<>>)
  end

  @spec verify(t()) :: boolean()
  def verify(proof = %LogicProof{}) do
    {public_inputs, private_inputs} = internal_logic_inputs(proof)

    args = [public_inputs | private_inputs]

    result = Nock.nock(proof.resource.logic, [9, 2, 10, [6, 1 | args], 0 | 1])

    case result do
      {:ok, zero} when zero in [0, <<>>, <<0>>, []] -> true
      _ -> false
    end
  end

  @empty [0, <<>>, <<0>>, []]

  @spec verify_resource_corresponds_to_tag(t()) :: boolean()
  def verify_resource_corresponds_to_tag(%LogicProof{
        resource: resource,
        self_tag: {:committed, commitment}
      }) do
    Resource.commitment(resource) == commitment
  end

  def verify_resource_corresponds_to_tag(%LogicProof{
        resource: resource,
        self_tag: {:nullified, nullifier}
      }) do
    Resource.nullifier(resource) == nullifier
  end

  @spec from_noun(Noun.t()) :: {:ok, t()} | :error
  def from_noun([
        resource,
        [
          [commits, nulls, self_tag, other_public | terminator],
          [commits_plain, nullified_plain, other_private | terminator2]
          | terminator3
        ]
        | terminator4
      ])
      when terminator in @empty and
             terminator2 in @empty and
             terminator3 in @empty and
             terminator4 in @empty do
    with {:ok, self_resource} <- Resource.from_noun(resource),
         {:ok, tag} <- determine_self_tag(self_tag),
         {:ok, nullified_plaintexts} <- from_noun_plaintext(nullified_plain),
         {:ok, committed_plaintexts} <- from_noun_plaintext(commits_plain) do
      {:ok,
       %LogicProof{
         resource: self_resource,
         # THEY MUST BE BINARY
         commitments:
           MapSet.new(
             Noun.list_nock_to_erlang(commits),
             &Noun.atom_integer_to_binary/1
           ),
         nullifiers:
           MapSet.new(
             Noun.list_nock_to_erlang(nulls),
             &Noun.atom_integer_to_binary/1
           ),
         self_tag: tag,
         other_public: other_public,
         committed_plaintexts: committed_plaintexts,
         nullified_plaintexts: nullified_plaintexts,
         other_private: other_private
       }}
    end
  end

  def from_noun(_) do
    :error
  end

  defimpl Noun.Nounable, for: __MODULE__ do
    # We chose an interesting Nock encoding
    @impl true
    def to_noun(proof = %Anoma.TransparentResource.LogicProof{}) do
      {public_inputs, private_inputs} =
        LogicProof.internal_logic_inputs(proof)

      [Resource.to_noun(proof.resource), [public_inputs, private_inputs]]
    end
  end

  # TODO I want to make this private but use it in to_noun
  @spec internal_logic_inputs(t()) :: {Noun.t(), Noun.t()}
  def internal_logic_inputs(proof = %LogicProof{}) do
    # We shouldn't have to do any work, since we aren't changing the
    # format in from_noun
    self_tag =
      case proof.self_tag do
        {:committed, comm} -> comm
        {:nullified, null} -> null
      end

    public_inputs = [
      MapSet.to_list(proof.commitments),
      MapSet.to_list(proof.nullifiers),
      self_tag,
      proof.other_public
    ]

    private_inputs = [
      Enum.map(proof.committed_plaintexts, &Resource.to_noun/1),
      Enum.map(proof.nullified_plaintexts, &Resource.to_noun/1),
      proof.other_private
    ]

    {public_inputs, private_inputs}
  end

  @spec determine_self_tag(Noun.t()) :: {:ok, any()} | :error
  # Does this case ever happen, why would the NF or CM be an integer!?
  # We should keep it in for safety however, maybe even report it if
  # we do find this to be an error
  defp determine_self_tag(self) when is_integer(self) do
    determine_self_tag(Noun.atom_integer_to_binary(self))
  end

  # We depend on internal details that may change
  defp determine_self_tag(nf = <<"NF_", _::binary>>) do
    {:ok, {:nullified, nf}}
  end

  defp determine_self_tag(nf = <<"CM_", _::binary>>) do
    {:ok, {:committed, nf}}
  end

  defp determine_self_tag(_tag) do
    :error
  end

  @spec from_noun_plaintext(Noun.t()) :: {:ok, MapSet.t(Resource.t())}
  defp from_noun_plaintext(noun) when noun in @empty do
    {:ok, MapSet.new([])}
  end

  defp from_noun_plaintext(noun) when is_list(noun) do
    maybe_resources =
      Enum.map(Noun.list_nock_to_erlang(noun), &Resource.from_noun/1)

    if Enum.any?(maybe_resources, &(:error == &1)) do
      :error
    else
      {:ok, MapSet.new(Enum.map(maybe_resources, fn {:ok, x} -> x end))}
    end
  end

  defp from_noun_plaintext(_) do
    :error
  end
end
