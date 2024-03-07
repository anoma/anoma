# NIF for Elixir.Cairo

## To build the NIF module:

- Your NIF will now build along with your project.

## To load the NIF:

```elixir
defmodule Cairo do
  use Rustler, otp_app: :anoma, crate: "cairo"

  # When your NIF is loaded, it will override this function.
  def cairo_run_and_prove(_arg1), do: :erlang.nif_error(:nif_not_loaded)
  def cairo_verify(_arg1, _arg2), do: :erlang.nif_error(:nif_not_loaded)
end
```
