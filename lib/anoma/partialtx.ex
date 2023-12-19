defmodule Anoma.PartialTx do
  @moduledoc """
  I represent partial transactions.
  """

  alias __MODULE__
  use TypedStruct
  alias Anoma.Resource

  @type resource_set(value) :: %{optional(binary()) => list(value)}

  @type resources :: resource_set(Resource.t())

  typedstruct do
    field(:inputs, resource_set(Resource.t()), default: %{})
    field(:outputs, resource_set(Resource.t()), default: %{})
  end

  @spec add_input(t(), Resource.t()) :: t()
  def add_input(partial, input) do
    %PartialTx{partial | inputs: update_resource_set(partial.inputs, input)}
  end

  @spec add_output(t(), Resource.t()) :: t()
  def add_output(partial, output) do
    %PartialTx{
      partial
      | outputs: update_resource_set(partial.outputs, output)
    }
  end

  @spec balanced(t()) :: boolean()
  def balanced(%PartialTx{inputs: inputs, outputs: outputs}) do
    inputs
    |> Map.merge(outputs, fn _k, i, o ->
      add_quantities(i) - add_quantities(o)
    end)
    |> Enum.all?(fn {_k, v} -> v == 0 end)
  end

  @doc """

  I check if a partial transaction is valid

  A transaction `is_valid` iff each logic inside the output and input
  resource set all agree that the transaction is valid.

  ### Parameters

  - partial - the partial transaction

  """
  @spec is_valid(t()) :: boolean()
  def is_valid(partial) do
    # Drop all nesting
    to_list = fn map ->
      map |> Map.values() |> List.flatten()
    end

    inputs = to_list.(partial.inputs)
    outputs = to_list.(partial.outputs)

    valid? = fn io? ->
      fn resource ->
        # we will use the interpreter for now
        Anoma.Eval.apply(resource.logic, {io?, inputs, outputs}) == 0
      end
    end

    Enum.all?(inputs, valid?.(:in)) && Enum.all?(outputs, valid?.(:out))
  end

  def empty(), do: %PartialTx{}

  @doc """

  I help create an "unique" partial transaction, by making a partial
  transaction including an empty resource with the binary of the given term.

  ### Parameters

     - `term` - any term one wishes to put in the resource

  ### Output

    - the semi-unique term

  """
  def unique_empty(term) do
    empty()
    |> add_input(Resource.make_empty(term))
  end

  ######################################################################
  # Helpers
  ######################################################################

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

    Map.update(resource_set, denom, [new_resource], &[new_resource | &1])
  end

  # the resources must be the same
  @spec add_quantities(list(Resource.t())) :: integer()
  defp add_quantities(resources) do
    Enum.reduce(resources, 0, fn x, acc -> x.quantity + acc end)
  end

  @spec sub_quantities(Resource.t(), Resource.t()) :: Resource.t()
  defp sub_quantities(resource_1, resource_2) do
    %Resource{
      resource_1
      | quantity: resource_1.quantity - resource_2.quantity
    }
  end
end

defimpl Anoma.Intent, for: Anoma.PartialTx do
  def is_intent(_data) do
    true
  end
end
