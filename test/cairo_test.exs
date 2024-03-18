defmodule NifTest do
  use ExUnit.Case
  doctest Anoma.Cairo

  test "cairo0_api_test" do
    {:ok, program} = File.read("./native/cairo/fibonacci_5.json")
    {proof, public_input} = Anoma.Cairo.cairo0_run_and_prove(program)
    assert true = Anoma.Cairo.cairo_verify(proof, public_input)
  end

  test "cairo1_api_test" do
    {trace, memory} =
      Anoma.Cairo1.cairo1_vm_runner(
        "./native/cairo1/sierra_program",
        "2 [1 2 3 4] 0 [9 8]"
      )

    # Prove and verify
    {proof, public_input} = Anoma.Cairo.cairo_prove(trace, memory)
    assert true = Anoma.Cairo.cairo_verify(proof, public_input)
  end
end
