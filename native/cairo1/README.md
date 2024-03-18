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
