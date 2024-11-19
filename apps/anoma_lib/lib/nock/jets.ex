defmodule Nock.Jets do
  @moduledoc """
  Jets for the Nock interpreter, taking a gate core. Not fully general.
  """

  import Noun
  import Bitwise
  alias Anoma.Crypto.Sign

  @spec calculate_mug_of_core(non_neg_integer(), non_neg_integer()) ::
          non_neg_integer()
  @doc """
  We calculate the mug of a given core at a given gate.

  ### Parameters

  - `index_in_core` - the index of the gate itself

  - `parent_layer` - the layer of the standard library. This should be
    the same as the layer numbers found in `anoma.hoon`

  ### Example

  In the Hoon repl one should write

      dojo> |commit %anoma
      >=
      dojo> =anoma -build-file /=anoma=/lib/anoma/hoon
      dojo> =>  anoma  !=(sub)
      [9 47 0 31]

  Now in IEX

      > Nock.Jets.calculate_mug_of_core(47, 1)
      14801825384048474882

  """
  def calculate_mug_of_core(index_in_core, parent_layer) do
    {:ok, core} = calculate_core(index_in_core, parent_layer)
    Noun.mug(hd(core))
  end

  @doc """
  We calculate the mug of a given layer at any given gate.

  ### Parameters

  - `parent_layer` - the layer of the standard library. This should be
    the same as the layer numbers found in `anoma.hoon`

  ### Example

  In the Hoon repl one should write

      dojo> |commit %anoma
      >=
      dojo> =anoma -build-file /=anoma=/lib/anoma/hoon
      dojo> =>  anoma  !=(sub)
      [9 47 0 31]

  Now in IEX

      > Nock.Jets.calculate_mug_of_layer(1)
      17654928022549292273

  This value should match Nock.@layer_1_contex_mug
  """
  @spec calculate_mug_of_layer(non_neg_integer()) :: non_neg_integer()
  def calculate_mug_of_layer(layer) do
    context_axis = Integer.pow(2, Nock.stdlib_layers() - layer + 1) - 1

    with {:ok, context} <- Noun.axis(context_axis, Nock.stdlib_core()) do
      mug(context)
    end
  end

  @spec calculate_mug_of_param_core(
          non_neg_integer(),
          non_neg_integer(),
          non_neg_integer()
        ) ::
          non_neg_integer()
  @doc """
  Like `calculate_mug_of_core/2` except we work over a parameterized core.

  ### Example

      > Nock.Jets.calculate_mug_of_param_core(767, 10, 4)
      12605872635346981159

  For our standard library, so far only layer 4 is parameterized
  """
  def calculate_mug_of_param_core(index_in_core, core_index, parent_layer) do
    with {:ok, val} <-
           calculate_core_param(core_index, index_in_core, parent_layer) do
      Noun.mug(hd(val))
    end
  end

  @spec calculate_mug_of_param_layer(non_neg_integer(), non_neg_integer()) ::
          non_neg_integer()
  @doc """
  Like `calculate_mug_of_layer/1` except we work over a parameterized core.

  ### Example

      > Nock.Jets.calculate_mug_of_param_layer(10, 4)
      11284470320276584209

  For our standard library, so far only layer 4 is parameterized
  """
  def calculate_mug_of_param_layer(core_index, parent_layer) do
    with {:ok, core} <- calculate_core_param(core_index, 4, parent_layer),
         {:ok, parent} <- Noun.axis(14, core) do
      Noun.mug(parent)
    end
  end

  def calculate_core_param(core_index, gate_index, parent_layer) do
    Nock.nock(Nock.logics_core(), [
      7,
      [
        9,
        core_index,
        0 | Noun.index_to_offset(Nock.stdlib_layers() - parent_layer + 3)
      ],
      9,
      gate_index,
      0 | 1
    ])
  end

  @spec calculate_core(non_neg_integer(), non_neg_integer()) ::
          :error | {:ok, Noun.t()}
  defp calculate_core(index_in_core, parent_layer) do
    Nock.nock(Nock.logics_core(), [
      8,
      # We drive `layers - parent + 3`, from how layers get pushed.
      # Each layer pushes the previous one down by one. the + 3 is for:
      # 0. layer 0 (I believe, Ι may be incorrect on this)
      # 1. the rm_core
      # 2. the logics_core
      [
        9,
        index_in_core,
        0 | Noun.index_to_offset(Nock.stdlib_layers() - parent_layer + 3)
      ],
      0 | 2
    ])
  end

  # when this is called, we've already jet-matched axis 7.
  # so axis 6 exists. nevertheless, we have ok and error cases in case
  # of implementation bugs.
  def sample([_, s | _]) do
    {:ok, s}
  end

  def sample(_) do
    :error
  end

  defp an_integer(x), do: Noun.atom_binary_to_integer(x)

  def dec(core) do
    with {:ok, noun} when is_noun_atom(noun) <- sample(core),
         sample when sample > 0 <- an_integer(noun) do
      {:ok, sample - 1}
    else
      _ -> :error
    end
  end

  def add(core) do
    with {:ok, [a | b]} when is_noun_atom(a) and is_noun_atom(b) <-
           sample(core) do
      {:ok, an_integer(a) + an_integer(b)}
    else
      _ -> :error
    end
  end

  def sub(core) do
    with {:ok, [a | b]} when is_noun_atom(a) and is_noun_atom(b) <-
           sample(core),
         {c, d} when c >= d <- {an_integer(a), an_integer(b)} do
      {:ok, c - d}
    else
      _ -> :error
    end
  end

  def lth(core) do
    with {:ok, [a | b]} when is_noun_atom(a) and is_noun_atom(b) <-
           sample(core),
         {c, d} = {an_integer(a), an_integer(b)} do
      if c < d do
        {:ok, 0}
      else
        {:ok, 1}
      end
    else
      _ -> :error
    end
  end

  def lte(core) do
    with {:ok, [a | b]} when is_noun_atom(a) and is_noun_atom(b) <-
           sample(core),
         {c, d} = {an_integer(a), an_integer(b)} do
      if c <= d do
        {:ok, 0}
      else
        {:ok, 1}
      end
    else
      _ -> :error
    end
  end

  def gth(core) do
    with {:ok, [a | b]} when is_noun_atom(a) and is_noun_atom(b) <-
           sample(core),
         {c, d} = {an_integer(a), an_integer(b)} do
      if c > d do
        {:ok, 0}
      else
        {:ok, 1}
      end
    else
      _ -> :error
    end
  end

  def gte(core) do
    with {:ok, [a | b]} when is_noun_atom(a) and is_noun_atom(b) <-
           sample(core),
         {c, d} = {an_integer(a), an_integer(b)} do
      if c >= d do
        {:ok, 0}
      else
        {:ok, 1}
      end
    else
      _ -> :error
    end
  end

  def mul(core) do
    maybe_sample = sample(core)

    case maybe_sample do
      {:ok, [a | b]} when is_noun_atom(a) and is_noun_atom(b) ->
        {:ok, an_integer(a) * an_integer(b)}

      _ ->
        :error
    end
  end

  def div(core) do
    with {:ok, [a | b]} when is_noun_atom(a) and is_noun_atom(b) <-
           sample(core),
         {c, d} when d != 0 <- {an_integer(a), an_integer(b)} do
      {:ok, Kernel.div(c, d)}
    else
      _ -> :error
    end
  end

  def mod(core) do
    with {:ok, [a | b]} when is_noun_atom(a) and is_noun_atom(b) <-
           sample(core),
         {c, d} when d != 0 <- {an_integer(a), an_integer(b)} do
      {:ok, Kernel.rem(c, d)}
    else
      _ -> :error
    end
  end

  @spec verify_detatched(Nock.t()) :: :error | {:ok, 0 | 1}
  def verify_detatched(core) do
    maybe_sample = sample(core)

    case maybe_sample do
      {:ok, [a, b | c]}
      when is_noun_atom(a) and is_noun_atom(b) and is_noun_atom(c) ->
        try do
          if Sign.verify_detached(
               Noun.atom_integer_to_binary(a),
               Noun.atom_integer_to_binary(b),
               Noun.atom_integer_to_binary(c)
             ) do
            {:ok, 0}
          else
            {:ok, 1}
          end
        rescue
          _ in ArgumentError -> {:ok, 1}
        end

      _ ->
        :error
    end
  end

  @spec verify(Nock.t()) :: :error | {:ok, binary()}
  def verify(core) do
    maybe_sample = sample(core)

    case maybe_sample do
      {:ok, [a | b]} when is_noun_atom(a) and is_noun_atom(b) ->
        try do
          case Sign.verify(
                 Noun.atom_integer_to_binary(a),
                 Noun.atom_integer_to_binary(b)
               ) do
            {:ok, val} -> {:ok, [0 | val]}
            {:error, _} -> {:ok, 0}
          end
        rescue
          _ in ArgumentError -> {:ok, 0}
        end

      _ ->
        :error
    end
  end

  @spec sign(Nock.t()) :: :error | {:ok, binary()}
  def sign(core) do
    on_binary(&Sign.sign/2, core)
  end

  @spec sign_detatched(Nock.t()) :: :error | {:ok, binary()}
  def sign_detatched(core) do
    on_binary(&Sign.sign_detached/2, core)
  end

  @spec on_binary(
          (Noun.noun_atom(), Noun.noun_atom() -> Noun.noun_atom()),
          Nock.t()
        ) ::
          :error | {:ok, binary()}
  defp on_binary(sign_fn, core) do
    maybe_sample = sample(core)

    case maybe_sample do
      {:ok, [a | b]} when is_noun_atom(a) and is_noun_atom(b) ->
        try do
          {:ok,
           sign_fn.(
             Noun.atom_integer_to_binary(a),
             Noun.atom_integer_to_binary(b)
           )}
        rescue
          _ in ArgumentError -> :error
        end

      _ ->
        :error
    end
  end

  @spec bex(Nock.t()) :: :error | {:ok, Noun.t()}
  def bex(core) do
    with {:ok, a} when is_noun_atom(a) <- sample(core) do
      {:ok, 2 ** a}
    else
      _ -> :error
    end
  end

  @spec mix(Nock.t()) :: :error | {:ok, Noun.t()}
  def mix(core) do
    with {:ok, [a | b]} when is_noun_atom(a) and is_noun_atom(b) <-
           sample(core) do
      {:ok, bxor(a, b)}
    else
      _ -> :error
    end
  end

  @spec lsh(Nock.t()) :: :error | {:ok, Noun.t()}
  def lsh(core) do
    with {:ok, [count | val]} when is_noun_atom(count) and is_noun_atom(val) <-
           sample(core),
         {:ok, block_size} when is_noun_atom(block_size) <-
           Noun.axis(30, core) do
      {:ok, val <<< (count <<< block_size)}
    else
      _ -> :error
    end
  end

  @spec rsh(Nock.t()) :: :error | {:ok, Noun.t()}
  def rsh(core) do
    with {:ok, [count | val]} when is_noun_atom(count) and is_noun_atom(val) <-
           sample(core),
         {:ok, block_size} when is_noun_atom(block_size) <-
           Noun.axis(30, core) do
      {:ok, val >>> (count <<< block_size)}
    else
      _ -> :error
    end
  end

  @spec nend(Nock.t()) :: :error | {:ok, Noun.t()}
  def nend(core) do
    with {:ok, [count | val]} when is_noun_atom(count) and is_noun_atom(val) <-
           sample(core),
         {:ok, block_size} when is_noun_atom(block_size) <-
           Noun.axis(30, core) do
      # we get #b1111, if count is 4. Since 1 <<< 4 = #b10000 - 1 = #b1111
      # block_size just a left shift on the count
      mask = (1 <<< (count <<< block_size)) - 1
      {:ok, val &&& mask}
    else
      _ -> :error
    end
  end

  @spec met(Nock.t()) :: :error | {:ok, Noun.t()}
  def met(core) do
    with {:ok, sample} when is_noun_atom(sample) <- sample(core),
         {:ok, block_size} when is_noun_atom(block_size) <-
           Noun.axis(30, core) do
      {:ok, Nock.Bits.num_bits(sample, block_size)}
    else
      _ -> :error
    end
  end

  def jam(core) do
    maybe_sample = sample(core)

    case maybe_sample do
      {:ok, sample} ->
        {:ok, Noun.Jam.jam(sample)}

      _ ->
        :error
    end
  end

  def cue(core) do
    maybe_sample = sample(core)

    case maybe_sample do
      {:ok, sample} when is_noun_atom(sample) ->
        Noun.Jam.cue(sample)

      _ ->
        :error
    end
  end

  def shax(core) do
    with {:ok, noun} when is_noun_atom(noun) <- sample(core),
         sample <- Noun.atom_integer_to_binary(noun) do
      {:ok, :crypto.hash(:sha256, sample) |> Noun.atom_binary_to_integer()}
    else
      _ -> :error
    end
  end
end
