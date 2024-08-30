defmodule Anoma.Serialise do
  # perform basic conversion of general erlang terms <-> something that can be turned into msgpack
  # eventually, this well be type-aware and have its own code generation, but currently it is primitive and general
  # N.B. Code.Typespec.fetch_types

  def pack(o) do
    :msgpack.pack(to_msgpack(o))
  end

  def unpack(s) do
    with {:ok, val} <- :msgpack.unpack(s) do
      from_msgpack(val)
    end
  end

  # can't use ** in guards--that is -(2**63) <= o and o < 2**64; i.e., signed or unsigned 64-bit integer
  def to_msgpack(o)
      when (is_integer(o) and -9_223_372_036_854_775_808 <= o and
              o < 18_446_744_073_709_551_616) or is_binary(o) or is_boolean(o) do
    o
  end

  # big integer
  def to_msgpack(o) when is_integer(o) do
    %{
      "__tag__" => "bigint",
      "sign" =>
        if o < 0 do
          1
        else
          0
        end,
      "magnitude" => Enum.reverse(Integer.digits(abs(o), 2 ** 64))
    }
  end

  def to_msgpack(o) when is_list(o) do
    if List.improper?(o) do
      # convert to proper list; is there a better way to do this?
      %{
        "__tag__" => "improper-list",
        "contents" => Enum.map(improper_to_proper_list(o), &to_msgpack/1)
      }
    else
      Enum.map(o, &to_msgpack/1)
    end
  end

  def to_msgpack(o) when is_tuple(o) do
    %{
      "__tag__" => "tuple",
      "contents" => Enum.map(Tuple.to_list(o), &to_msgpack/1)
    }
  end

  def to_msgpack(o) when is_atom(o) do
    %{"__tag__" => "atom", "contents" => Atom.to_string(o)}
  end

  def to_msgpack(addr = %Anoma.Node.Router.Addr{}) do
    exit("can't serialise addr #{inspect(addr)}")
  end

  def to_msgpack(o) when is_map(o) do
    if Enum.all?(Map.keys(o), &is_atom/1) do
      # no 'map over values' builtin?  grumble
      # maps with __struct__ set are not 'enumerable', so we need to_list.  grumble
      Map.new(Map.to_list(o), fn {k, v} -> {k, to_msgpack(v)} end)
    else
      %{
        "__tag__" => "complex-map",
        "contents" =>
          Enum.map(o, fn {k, v} -> [to_msgpack(k), to_msgpack(v)] end)
      }
    end
  end

  @spec from_msgpack(term()) :: {:ok, term()} | {:error, term()}
  def from_msgpack(o) when is_integer(o) or is_binary(o) or is_boolean(o) do
    {:ok, o}
  end

  def from_msgpack([]) do
    {:ok, []}
  end

  def from_msgpack([x | xs]) do
    with {:ok, x} <- from_msgpack(x) do
      with {:ok, xs} <- from_msgpack(xs) do
        {:ok, [x | xs]}
      end
    end
  end

  def from_msgpack(%{
        "__tag__" => "bigint",
        "sign" => sign,
        "magnitude" => magnitude = [_ | _]
      })
      when sign in [0, 1] do
    if Enum.all?(magnitude, &is_integer/1) do
      magnitude = Integer.undigits(Enum.reverse(magnitude), 2 ** 64)

      {:ok,
       if sign == 1 do
         -magnitude
       else
         magnitude
       end}
    else
      {:error, :malformed_integer}
    end
  end

  def from_msgpack(%{
        "__tag__" => "improper-list",
        "contents" => contents = [_ | _]
      }) do
    with {:ok, list} <- from_msgpack(contents) do
      {:ok, proper_to_improper_list(list)}
    end
  end

  def from_msgpack(%{"__tag__" => "tuple", "contents" => contents})
      when is_list(contents) do
    with {:ok, list} <- from_msgpack(contents) do
      {:ok, List.to_tuple(list)}
    end
  end

  def from_msgpack(%{"__tag__" => "atom", "contents" => contents})
      when is_binary(contents) do
    atom_from_binary(contents)
  end

  def from_msgpack(%{
        "__tag__" => "complex-map",
        "contents" => contents = [_ | _]
      }) do
    from_complex_map(contents, %{})
  end

  # would be convenient if it would turn binaries to atoms iff they are map keys...
  # should send a patch for this later
  def from_msgpack(map = %{}) do
    from_map(Enum.to_list(map), %{})
  end

  def from_msgpack(o) do
    {:error, {:unrecognised, o}}
  end

  defp from_map([], acc) do
    {:ok, acc}
  end

  defp from_map([{k, v} | xs], acc) do
    with {:ok, k} <- atom_from_binary(k) do
      with {:ok, v} <- from_msgpack(v) do
        from_map(xs, Map.put(acc, k, v))
      end
    end
  end

  defp from_complex_map([], acc) do
    {:ok, acc}
  end

  defp from_complex_map([[k, v] | xs], acc) do
    with {:ok, k} <- from_msgpack(k) do
      with {:ok, v} <- from_msgpack(v) do
        from_complex_map(xs, Map.put(acc, k, v))
      end
    end
  end

  defp atom_from_binary(x) when is_atom(x) do
    {:ok, x}
  end

  defp atom_from_binary(x) when is_binary(x) do
    # not sure if there is another way to do this?
    try do
      {:ok, String.to_existing_atom(x)}
    rescue
      _ -> {:error, {:no_such_atom, x}}
    end
  end

  defp atom_from_binary(x) do
    {:error, {:not_binary_or_atom, x}}
  end

  defp improper_to_proper_list([x | xs]) do
    [x | improper_to_proper_list(xs)]
  end

  defp improper_to_proper_list(x) do
    [x]
  end

  defp proper_to_improper_list([x]) do
    x
  end

  defp proper_to_improper_list([x | xs]) do
    [x | proper_to_improper_list(xs)]
  end
end
