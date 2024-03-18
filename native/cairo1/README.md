# NIF for Elixir.Anoma.Cairo1

## To build the NIF module:

- Your NIF will now build along with your project.

## To load the NIF:

```elixir
defmodule Anoma.Cairo1 do
  use Rustler, otp_app: :anoma, crate: "cairo1"

  # When your NIF is loaded, it will override this function.
  def cairo1_vm_runner(_arg1, _arg2), do: :erlang.nif_error(:nif_not_loaded)
end
```

## TO compile Cairo1 code

### Compile cairo1 code to sierra

Use [cairo-compile](https://github.com/starkware-libs/cairo) to compile cairo code to sierra(pls use v2.5.4)

```bash
cargo run --bin cairo-compile -- --single-file /path/to/input.cairo /path/to/output.sierra --replace-ids
```

Note: It will be replaced by Juvix code and Juvix-lang compiler

### Run cairo1-vm and generate the proof
An example can be found in "cairo1_api_test"

```
# Run cairo1-vm
{trace, memory} =
      Anoma.Cairo1.cairo1_vm_runner(
        sierra_program,
        pub_inputs
      )

# Prove and verify
{proof, public_input} = Anoma.Cairo.cairo_prove(trace, memory)
Anoma.Cairo.cairo_verify(proof, public_input)
```