defmodule EventBroker.FilterSpec do
  @moduledoc """
  a filter specification
  """

  use TypedStruct

  typedstruct enforce: true do
    field(:filter_module, module())
    field(:filter_params, struct())
  end
end
