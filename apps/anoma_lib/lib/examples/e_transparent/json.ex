defmodule Examples.ETransparent.EJSON do
  alias Examples.ETransparent.EResource
  alias Examples.ETransparent.EAction
  alias Examples.ETransparent.ETransaction

  use TestHelper.TestMacro

  @doc """
  I test the json encoding of a cps unit.
  """
  def cps_instance do
    instance = arbitrary_cps_instance()
    Jason.encode!(instance)
  end

  @doc """
  I test the json encoding of a compliance unit.
  """
  def compliance_unit do
    compliance_unit = arbitrary_compliance_unit()
    Jason.encode!(compliance_unit)
  end

  @doc """
  I test the json encoding of a resource
  """
  def resource do
    resource = arbitrary_resource()
    Jason.encode!(resource)
  end

  @doc """
  I test the json encoding of an action.
  """
  def action do
    action = arbitrary_action()
    Jason.encode!(action)
  end

  @doc """
  I test the json encoding of a transaction.
  """
  def transaction do
    transaction = arbitrary_transaction()
    Jason.encode!(transaction)
  end

  ############################################################
  #                           Helpers                        #
  ############################################################

  def arbitrary_transaction do
    ETransaction.single_swap()
  end

  def arbitrary_action do
    EAction.trivial_swap_action()
  end

  def arbitrary_resource do
    EResource.trivial_false_resource()
  end

  def arbitrary_compliance_unit do
    arbitrary_action()
    |> Map.get(:compliance_units)
    |> Enum.into([])
    |> hd()
  end

  def arbitrary_cps_instance do
    arbitrary_action()
    |> Map.get(:instance)
  end
end
