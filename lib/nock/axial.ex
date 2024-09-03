defmodule Nock.Axial do
  import Nock
  import Noun

  @stdlib_layer_count 6

  def layer_gate_context_mug(layer) do
    context_axis = Integer.pow(2, @stdlib_layer_count - layer + 1) - 1
    {:ok, context} = axis(context_axis, stdlib_core())
    mug(context)
  end
end