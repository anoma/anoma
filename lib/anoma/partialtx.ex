defmodule Anoma.PartialTx do
  @moduledoc """
  I represent partial transactions.
  """

  alias __MODULE__
  use TypedStruct
  alias Anoma.Resource

  @type resource_set(value) :: %{optional(binary()) => value}

  @type resources :: resource_set(Resource.t())

  typedstruct do
    field(:inputs, resource_set(Resource.t()), default: %{})
    field(:outputs, resource_set(Resource.t()), default: %{})
    field(:extra_data, :binary, default: <<>>)
  end

  @spec add_input(t(), Resource.t()) :: t()
  def add_input(partial, input) do
    %PartialTx{partial | inputs: update_resource_set(partial.inputs, input)}
  end

  @spec add_output(t(), Resource.t()) :: t()
  def add_output(partial, output) do
    %PartialTx{partial | outputs: update_resource_set(partial.outputs, output)}
  end

  @spec balanced(t()) :: boolean()
  def balanced(partial) do
    Map.merge(partial.inputs, partial.outputs, fn _k, i, o -> sub_quantities(i, o) end)
    |> Map.to_list()
    |> Enum.all?(fn {_k, v} -> v.quantity == 0 end)
  end

  def empty(), do: %PartialTx{}

  # Helpers

  @doc """
  I update the resource set with the new resource


  ### Parameters

  - resource_set (resource_set()) - the resource set we are updating

  - new_resource (Resource.t()) - the resource we are adding

  ### Returns

  the updated resource set
  """
  @spec update_resource_set(resources, Resource.t()) :: resources
  defp update_resource_set(resource_set, new_resource) do
    denom = Resource.denomination(new_resource)

    Map.update(resource_set, denom, new_resource, &add_quantities(&1, new_resource))
  end

  # the resources must be the same
  @spec add_quantities(Resource.t(), Resource.t()) :: Resource.t()
  defp add_quantities(resource_1, resource_2) do
    %Resource{resource_1 | quantity: resource_1.quantity + resource_2.quantity}
  end

  @spec sub_quantities(Resource.t(), Resource.t()) :: Resource.t()
  defp sub_quantities(resource_1, resource_2) do
    %Resource{resource_1 | quantity: resource_1.quantity - resource_2.quantity}
  end
end

defimpl Anoma.Intent, for: Anoma.PartialTx do
  def is_intent(_data) do
    true
  end
end
