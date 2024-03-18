# NIF for Elixir.Cairo

## To build the NIF module:

- Your NIF will now build along with your project.

## To load the NIF:

```elixir
defmodule Cairo do
  use Rustler, otp_app: :anoma, crate: "cairo"

  # When your NIF is loaded, it will override this function.
  def cairo0_run_and_prove(_arg1), do: :erlang.nif_error(:nif_not_loaded)
  def cairo_prove(_arg1, _arg2), do: :erlang.nif_error(:nif_not_loaded)
  def cairo_verify(_arg1, _arg2), do: :erlang.nif_error(:nif_not_loaded)
end
```

## TO compile Cairo code

### Install [cairo-long](https://github.com/starkware-libs/cairo-lang):

[Setting up the environment](https://docs.cairo-lang.org/quickstart.html):

```shell
# set up environment
python3.9 -m venv ~/cairo_venv
source ~/cairo_venv/bin/activate

# install dependencies
# On Ubuntu
sudo apt install -y libgmp3-dev

# On Mac
brew install gmp

# install pip packages
pip3 install ecdsa fastecdsa sympy

# install the `cairo-lang`
pip3 install cairo-lang
```

### Compile cairo code
Note: make sure all commands are executed in the virtual environment.

```shell
cairo-compile fibonacci_5.cairo --output fibonacci_5.json --proof_mode
```
