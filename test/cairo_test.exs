defmodule NifTest do
  use ExUnit.Case
  doctest Anoma.Cairo

  test "cairo_api_test" do
    {:ok, program} = File.read("./native/cairo/fibonacci_5.json")
    {proof, public_input} = Anoma.Cairo.cairo_run_and_prove(program)
    assert true = Anoma.Cairo.cairo_verify(proof, public_input)
  end
end
