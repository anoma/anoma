defmodule Nock.Jets do
  @moduledoc """
  Jets for the Nock interpreter, taking a gate core. Not fully general.
  """

  alias Anoma.Crypto.Sign
  alias Anoma.RM.Transparent.Action
  alias Anoma.RM.Transparent.ComplianceUnit
  alias Anoma.RM.Transparent.Resource
  alias Anoma.RM.Transparent.Primitive.DeltaHash
  alias Anoma.RM.Transparent.ProvingSystem.DPS
  alias Anoma.RM.Transparent.ProvingSystem.CPS
  alias Anoma.CairoResource

  import Bitwise
  import Noun

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
    calculate_core(index_in_core, parent_layer) |> hd() |> Noun.mug()
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
    layer |> calculate_layer() |> Noun.mug()
  end

  @doc """
  I get the layer based on its number.
  """
  @spec calculate_layer(non_neg_integer()) :: Noun.t()
  def calculate_layer(layer) do
    context_axis = layer_offset(layer)

    with {:ok, context} <- Noun.axis(context_axis, Nock.Lib.rm_core()) do
      context
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
    calculate_core_param(index_in_core, core_index, parent_layer)
    |> hd()
    |> Noun.mug()
  end

  @spec calculate_mug_of_param_layer(non_neg_integer(), non_neg_integer()) ::
          non_neg_integer()
  @doc """
  Like `calculate_mug_of_layer/1` except we work over a parameterized core.

  ### Example

      > Nock.Jets.calculate_mug_of_param_layer(10, 4)
      11284470320276584209
  """
  def calculate_mug_of_param_layer(core_index, parent_layer) do
    core_index |> calculate_param_layer(parent_layer) |> Noun.mug()
  end

  @doc """
  I find the door cores, i.e. parametrized layers.
  """
  @spec calculate_param_layer(non_neg_integer(), non_neg_integer) :: Noun.t()
  def calculate_param_layer(core_index, parent_layer) do
    with core <- calculate_core_param(core_index, 4, parent_layer),
         {:ok, parent} <- Noun.axis(14, core) do
      parent
    end
  end

  @spec calculate_core_param(
          non_neg_integer(),
          non_neg_integer(),
          non_neg_integer()
        ) ::
          Noun.t()
  def calculate_core_param(gate_index, core_index, parent_layer) do
    with {:ok, res} <-
           nock4k(Nock.Lib.rm_core(), [
             7,
             [
               9,
               core_index,
               0 | layer_offset(parent_layer)
             ],
             9,
             gate_index,
             0 | 1
           ]) do
      res
    end
  end

  @spec calculate_core(non_neg_integer(), non_neg_integer()) ::
          Noun.t()
  def calculate_core(index_in_core, layer) do
    with {:ok, res} <-
           nock4k(Nock.Lib.rm_core(), [
             8,
             [
               9,
               index_in_core,
               0 | layer_offset(layer)
             ],
             0 | 2
           ]) do
      res
    end
  end

  @spec layer_offset(non_neg_integer) :: non_neg_integer
  defp layer_offset(layers) do
    Noun.index_to_offset(Nock.Lib.stdlib_layers() - layers + 1)
  end

  # when this is called, we've already jet-matched axis 7.
  # so axis 6 exists. nevertheless, we have ok and error cases in case
  # of implementation bugs.
  @spec sample(Noun.t()) :: :error | {:ok, Noun.t()}
  def sample([_, s | _]) do
    {:ok, s}
  end

  def sample(_) do
    :error
  end

  @spec an_integer(Noun.t()) :: Noun.t()
  defp an_integer(x), do: Noun.atom_binary_to_integer(x)

  @spec dec(Noun.t()) :: :error | {:ok, Noun.t()}
  def dec(core) do
    with {:ok, noun} when is_noun_atom(noun) <- sample(core),
         sample when sample > 0 <- an_integer(noun) do
      {:ok, sample - 1}
    else
      _ -> :error
    end
  end

  @spec add(Noun.t()) :: :error | {:ok, Noun.t()}
  def add(core) do
    with {:ok, [a | b]} when is_noun_atom(a) and is_noun_atom(b) <-
           sample(core) do
      {:ok, an_integer(a) + an_integer(b)}
    else
      _ -> :error
    end
  end

  @spec sub(Noun.t()) :: :error | {:ok, Noun.t()}
  def sub(core) do
    with {:ok, [a | b]} when is_noun_atom(a) and is_noun_atom(b) <-
           sample(core) do
      partial_sub(an_integer(a), an_integer(b))
    else
      _ -> :error
    end
  end

  @spec lth(Noun.t()) :: :error | {:ok, Noun.t()}
  def lth(core) do
    with {:ok, [a | b]} when is_noun_atom(a) and is_noun_atom(b) <-
           sample(core),
         {c, d} <- {an_integer(a), an_integer(b)} do
      if c < d do
        {:ok, 0}
      else
        {:ok, 1}
      end
    else
      _ -> :error
    end
  end

  @spec lte(Noun.t()) :: :error | {:ok, Noun.t()}
  def lte(core) do
    with {:ok, [a | b]} when is_noun_atom(a) and is_noun_atom(b) <-
           sample(core),
         {c, d} <- {an_integer(a), an_integer(b)} do
      if c <= d do
        {:ok, 0}
      else
        {:ok, 1}
      end
    else
      _ -> :error
    end
  end

  @spec gth(Noun.t()) :: :error | {:ok, Noun.t()}
  def gth(core) do
    with {:ok, [a | b]} when is_noun_atom(a) and is_noun_atom(b) <-
           sample(core),
         {c, d} <- {an_integer(a), an_integer(b)} do
      if c > d do
        {:ok, 0}
      else
        {:ok, 1}
      end
    else
      _ -> :error
    end
  end

  @spec gte(Noun.t()) :: :error | {:ok, Noun.t()}
  def gte(core) do
    with {:ok, [a | b]} when is_noun_atom(a) and is_noun_atom(b) <-
           sample(core),
         {c, d} <- {an_integer(a), an_integer(b)} do
      if c >= d do
        {:ok, 0}
      else
        {:ok, 1}
      end
    else
      _ -> :error
    end
  end

  @spec mul(Noun.t()) :: :error | {:ok, Noun.t()}
  def mul(core) do
    maybe_sample = sample(core)

    case maybe_sample do
      {:ok, [a | b]} when is_noun_atom(a) and is_noun_atom(b) ->
        {:ok, an_integer(a) * an_integer(b)}

      _ ->
        :error
    end
  end

  @spec div(Noun.t()) :: :error | {:ok, Noun.t()}
  def div(core) do
    with {:ok, [a | b]} when is_noun_atom(a) and is_noun_atom(b) <-
           sample(core),
         {c, d} when d != 0 <- {an_integer(a), an_integer(b)} do
      {:ok, Kernel.div(c, d)}
    else
      _ -> :error
    end
  end

  @spec mod(Noun.t()) :: :error | {:ok, Noun.t()}
  def mod(core) do
    with {:ok, [a | b]} when is_noun_atom(a) and is_noun_atom(b) <-
           sample(core),
         {c, d} when d != 0 <- {an_integer(a), an_integer(b)} do
      {:ok, Kernel.rem(c, d)}
    else
      _ -> :error
    end
  end

  @spec verify_detatched(Noun.t()) :: :error | {:ok, 0 | 1}
  def verify_detatched(core) do
    maybe_sample = sample(core)

    case maybe_sample do
      {:ok, [a, b | c]}
      when is_noun_atom(a) and is_noun_atom(b) and is_noun_atom(c) ->
        try do
          if Sign.verify_detached(
               Noun.atom_integer_to_binary(a, 64),
               Noun.atom_integer_to_binary(b),
               Noun.atom_integer_to_binary(c, 32)
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

  @spec verify(Noun.t()) :: :error | {:ok, binary()}
  def verify(core) do
    maybe_sample = sample(core)

    case maybe_sample do
      {:ok, [a | b]} when is_noun_atom(a) and is_noun_atom(b) ->
        try do
          case Sign.verify(
                 Noun.atom_integer_to_binary(a),
                 Noun.atom_integer_to_binary(b, 32)
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

  @spec sign(Noun.t()) :: :error | {:ok, binary()}
  def sign(core) do
    on_binary(&Sign.sign/2, core)
  end

  @spec sign_detatched(Noun.t()) :: :error | {:ok, binary()}
  def sign_detatched(core) do
    on_binary(&Sign.sign_detached/2, core)
  end

  @spec on_binary(
          (Noun.noun_atom(), Noun.noun_atom() -> Noun.noun_atom()),
          Noun.t()
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
             Noun.atom_integer_to_binary(b, 64)
           )}
        rescue
          _ in ArgumentError -> :error
        end

      _ ->
        :error
    end
  end

  @spec bex(Noun.t()) :: :error | {:ok, Noun.t()}
  def bex(core) do
    with {:ok, a} when is_noun_atom(a) <- sample(core) do
      a = an_integer(a)
      {:ok, 2 ** a}
    else
      _ -> :error
    end
  end

  @spec mix(Noun.t()) :: :error | {:ok, Noun.t()}
  def mix(core) do
    with {:ok, [a | b]} when is_noun_atom(a) and is_noun_atom(b) <-
           sample(core) do
      a = an_integer(a)
      b = an_integer(b)
      {:ok, bxor(a, b)}
    else
      _ -> :error
    end
  end

  @spec lsh(Noun.t()) :: :error | {:ok, Noun.t()}
  def lsh(core) do
    with {:ok, [count | val]} when is_noun_atom(count) and is_noun_atom(val) <-
           sample(core),
         {:ok, block_size} when is_noun_atom(block_size) <-
           Noun.axis(30, core) do
      {:ok,
       an_integer(val) <<< (an_integer(count) <<< an_integer(block_size))}
    else
      _ -> :error
    end
  end

  @spec rsh(Noun.t()) :: :error | {:ok, Noun.t()}
  def rsh(core) do
    with {:ok, [count | val]} when is_noun_atom(count) and is_noun_atom(val) <-
           sample(core),
         {:ok, block_size} when is_noun_atom(block_size) <-
           Noun.axis(30, core) do
      {:ok,
       an_integer(val) >>> (an_integer(count) <<< an_integer(block_size))}
    else
      _ -> :error
    end
  end

  @spec nend(Noun.t()) :: :error | {:ok, Noun.t()}
  def nend(core) do
    with {:ok, [count | val]} when is_noun_atom(count) and is_noun_atom(val) <-
           sample(core),
         {:ok, block_size} when is_noun_atom(block_size) <-
           Noun.axis(30, core) do
      # we get #b1111, if count is 4. Since 1 <<< 4 = #b10000 - 1 = #b1111
      # block_size just a left shift on the count
      mask = (1 <<< (an_integer(count) <<< an_integer(block_size))) - 1
      {:ok, an_integer(val) &&& mask}
    else
      _ -> :error
    end
  end

  @spec met(Noun.t()) :: :error | {:ok, Noun.t()}
  def met(core) do
    with {:ok, sample} when is_noun_atom(sample) <- sample(core),
         {:ok, block_size} when is_noun_atom(block_size) <-
           Noun.axis(30, core) do
      {:ok, Noun.Bits.num_bits(sample, block_size)}
    else
      _ -> :error
    end
  end

  @spec jam(Noun.t()) :: :error | {:ok, Noun.t()}
  def jam(core) do
    maybe_sample = sample(core)

    case maybe_sample do
      {:ok, sample} ->
        {:ok, Noun.Jam.jam(sample)}

      _ ->
        :error
    end
  end

  @spec cue(Noun.t()) :: :error | {:ok, Noun.t()}
  def cue(core) do
    maybe_sample = sample(core)

    case maybe_sample do
      {:ok, sample} when is_noun_atom(sample) ->
        Noun.Jam.cue(sample)

      _ ->
        :error
    end
  end

  @spec shax(Noun.t()) :: :error | {:ok, Noun.t()}
  def shax(core) do
    with {:ok, noun} when is_noun_atom(noun) <- sample(core),
         sample <- Noun.atom_integer_to_binary(noun) do
      {:ok, :crypto.hash(:sha256, sample)}
    else
      _ -> :error
    end
  end

  ### Signed integer arithmetic

  @spec abs(Noun.t()) :: :error | {:ok, Noun.t()}
  def abs(core) do
    with {:ok, noun} when is_noun_atom(noun) <- sample(core) do
      res = a_signed_integer(noun) |> Kernel.abs()
      {:ok, res}
    else
      _ -> :error
    end
  end

  @spec dif(Noun.t()) :: :error | {:ok, Noun.t()}
  def dif(core) do
    with {:ok, [a | b]} when is_noun_atom(a) and is_noun_atom(b) <-
           sample(core) do
      {c, d} = {a_signed_integer(a), a_signed_integer(b)}
      {:ok, (c - d) |> encode_signed}
    else
      _ -> :error
    end
  end

  @spec dul(Noun.t()) :: :error | {:ok, Noun.t()}
  def dul(core) do
    with {:ok, [a | b]} when is_noun_atom(a) and is_noun_atom(b) <-
           sample(core) do
      {c, d} = {a_signed_integer(a), an_integer(b)}

      cond do
        c >= 0 -> {:ok, Kernel.rem(c, d)}
        c < 0 -> partial_sub(d, Kernel.abs(c))
      end
    else
      _ -> :error
    end
  end

  @spec fra(Noun.t()) :: :error | {:ok, Noun.t()}
  def fra(core) do
    with {:ok, [a | b]} when is_noun_atom(a) and is_noun_atom(b) <-
           sample(core) do
      {c, d} = {a_signed_integer(a), a_signed_integer(b)}
      {:ok, div(c, d) |> encode_signed}
    else
      _ -> :error
    end
  end

  @spec pro(Noun.t()) :: :error | {:ok, Noun.t()}
  def pro(core) do
    with {:ok, [a | b]} when is_noun_atom(a) and is_noun_atom(b) <-
           sample(core) do
      {c, d} = {a_signed_integer(a), a_signed_integer(b)}
      {:ok, (c * d) |> encode_signed}
    else
      _ -> :error
    end
  end

  @spec rem(Noun.t()) :: :error | {:ok, Noun.t()}
  def rem(core) do
    with {:ok, [a | b]} when is_noun_atom(a) and is_noun_atom(b) <-
           sample(core) do
      {c, d} = {a_signed_integer(a), a_signed_integer(b)}
      {:ok, rem(c, d) |> encode_signed}
    else
      _ -> :error
    end
  end

  @spec sum(Noun.t()) :: :error | {:ok, Noun.t()}
  def sum(core) do
    with {:ok, [a | b]} when is_noun_atom(a) and is_noun_atom(b) <-
           sample(core) do
      {c, d} = {a_signed_integer(a), a_signed_integer(b)}
      {:ok, (c + d) |> encode_signed}
    else
      _ -> :error
    end
  end

  @spec sun(Noun.t()) :: :error | {:ok, Noun.t()}
  def sun(core) do
    with {:ok, noun} when is_noun_atom(noun) <- sample(core) do
      {:ok, 2 * an_integer(noun)}
    else
      _ -> :error
    end
  end

  @spec syn(Noun.t()) :: :error | {:ok, Noun.t()}
  def syn(core) do
    with {:ok, noun} when is_noun_atom(noun) <- sample(core) do
      res = if a_signed_integer(noun) < 0, do: 1, else: 0
      {:ok, res}
    else
      _ -> :error
    end
  end

  @spec cmp(Noun.t()) :: :error | {:ok, Noun.t()}
  def cmp(core) do
    with {:ok, [a | b]} when is_noun_atom(a) and is_noun_atom(b) <-
           sample(core) do
      {c, d} = {a_signed_integer(a), a_signed_integer(b)}
      {:ok, compare(c, d) |> encode_signed}
    else
      _ -> :error
    end
  end

  @spec nmug(Noun.t()) :: :error | {:ok, Noun.t()}
  def nmug(core) do
    with {:ok, a} <- sample(core) do
      {:ok, Noun.mug(a)}
    else
      _ -> :error
    end
  end

  defp a_signed_integer(x), do: Noun.atom_binary_to_signed_integer(x)

  @spec dor(Noun.t()) :: :error | {:ok, Noun.t()}
  def dor(core) do
    with {:ok, [a | b]} <- sample(core) do
      res = Noun.Order.dor(a, b) |> Noun.bool_to_noun()
      {:ok, res}
    else
      _ -> :error
    end
  end

  @spec gor(Noun.t()) :: :error | {:ok, Noun.t()}
  def gor(core) do
    with {:ok, [a | b]} <- sample(core) do
      res = Noun.Order.gor(a, b) |> Noun.bool_to_noun()

      {:ok, res}
    else
      _ -> :error
    end
  end

  @spec mor(Noun.t()) :: :error | {:ok, Noun.t()}
  def mor(core) do
    with {:ok, [a | b]} <- sample(core) do
      res = Noun.Order.mor(a, b) |> Noun.bool_to_noun()

      {:ok, res}
    else
      _ -> :error
    end
  end

  @spec silt(Noun.t()) :: :error | {:ok, Noun.t()}
  def silt(core) do
    with {:ok, a} <- sample(core),
         {:ok, list} <- Noun.Nounable.List.from_noun(a) do
      {:ok, list |> MapSet.new() |> Noun.Nounable.to_noun()}
    else
      _ -> :error
    end
  end

  @spec put(Noun.t()) :: :error | {:ok, Noun.t()}
  def put(core) do
    with {:ok, elem} <- sample(core),
         {:ok, door_set} <- Noun.axis(30, core),
         {:ok, set} <- Noun.Nounable.MapSet.from_noun(door_set) do
      {:ok, set |> MapSet.put(elem) |> Noun.Nounable.to_noun()}
    else
      _ -> :error
    end
  end

  @spec uni(Noun.t()) :: :error | {:ok, Noun.t()}
  def uni(core) do
    with {:ok, set_arg} <- sample(core),
         {:ok, door_set} <- Noun.axis(30, core),
         {:ok, set1} <- Noun.Nounable.MapSet.from_noun(set_arg),
         {:ok, set2} <- Noun.Nounable.MapSet.from_noun(door_set) do
      {:ok, set1 |> MapSet.union(set2) |> Noun.Nounable.to_noun()}
    else
      _ -> :error
    end
  end

  @spec int(Noun.t()) :: :error | {:ok, Noun.t()}
  def int(core) do
    with {:ok, set_arg} <- sample(core),
         {:ok, door_set} <- Noun.axis(30, core),
         {:ok, set1} <- Noun.Nounable.MapSet.from_noun(set_arg),
         {:ok, set2} <- Noun.Nounable.MapSet.from_noun(door_set) do
      {:ok, set1 |> MapSet.intersection(set2) |> Noun.Nounable.to_noun()}
    else
      _ -> :error
    end
  end

  @spec sdif(Noun.t()) :: :error | {:ok, Noun.t()}
  def sdif(core) do
    with {:ok, set_arg} <- sample(core),
         {:ok, door_set} <- Noun.axis(30, core),
         {:ok, set1} <- Noun.Nounable.MapSet.from_noun(set_arg),
         {:ok, set2} <- Noun.Nounable.MapSet.from_noun(door_set) do
      {:ok, set1 |> MapSet.difference(set2) |> Noun.Nounable.to_noun()}
    else
      _ -> :error
    end
  end

  @spec duni(Noun.t()) :: :error | {:ok, Noun.t()}
  def duni(core) do
    with {:ok, set_arg} <- sample(core),
         {:ok, door_set} <- Noun.axis(30, core),
         {:ok, set1} <- Noun.Nounable.MapSet.from_noun(set_arg),
         {:ok, set2} <- Noun.Nounable.MapSet.from_noun(door_set),
         true <- MapSet.disjoint?(set1, set2) do
      {:ok, set1 |> MapSet.union(set2) |> Noun.Nounable.to_noun()}
    else
      _ -> :error
    end
  end

  @spec has(Noun.t()) :: :error | {:ok, Noun.t()}
  def has(core) do
    with {:ok, elem} <- sample(core),
         {:ok, door_set} <- Noun.axis(30, core),
         {:ok, set} <- Noun.Nounable.MapSet.from_noun(door_set) do
      {:ok, set |> MapSet.member?(elem) |> Noun.bool_to_noun()}
    else
      _ -> :error
    end
  end

  @spec mput(Noun.t()) :: :error | {:ok, Noun.t()}
  def mput(core) do
    with {:ok, [key | value]} <- sample(core),
         {:ok, door_map} <- Noun.axis(30, core),
         {:ok, map} <- Noun.Nounable.Map.from_noun(door_map) do
      {:ok, map |> Map.put(key, value) |> Noun.Nounable.Map.to_noun()}
    else
      _ -> :error
    end
  end

  @spec got(Noun.t()) :: :error | {:ok, Noun.t()}
  def got(core) do
    with {:ok, key} <- sample(core),
         {:ok, door_map} <- Noun.axis(30, core),
         {:ok, map} <- Noun.Nounable.Map.from_noun(door_map),
         res <- Map.get(map, key) do
      {:ok, Noun.Nounable.Map.to_noun(res)}
    else
      _ -> :error
    end
  end

  @spec kind(Noun.t()) :: :error | {:ok, Noun.t()}
  def kind(core) do
    with {:ok, a} when is_noun_cell(a) <- sample(core),
         {:ok, resource} <- Resource.from_noun(a) do
      res = Resource.kind(resource)
      {:ok, res}
    else
      _ ->
        :error
    end
  end

  @spec delta_add(Noun.t()) :: :error | {:ok, Noun.t()}
  def delta_add(core) do
    with {:ok, [a | b]} <- sample(core),
         {:ok, _map} <- Noun.Nounable.Map.from_noun(a),
         {:ok, _map} <- Noun.Nounable.Map.from_noun(b) do
      res =
        DeltaHash.delta_add(
          Noun.atom_binary_to_integer(a),
          Noun.atom_binary_to_integer(b)
        )

      {:ok, res}
    else
      _ ->
        :error
    end
  end

  @spec delta_sub(Noun.t()) :: :error | {:ok, Noun.t()}
  def delta_sub(core) do
    with {:ok, [a | b]} <- sample(core),
         {:ok, _map} <- Noun.Nounable.Map.from_noun(a),
         {:ok, _map} <- Noun.Nounable.Map.from_noun(b) do
      res =
        DeltaHash.delta_sub(
          Noun.atom_binary_to_integer(a),
          Noun.atom_binary_to_integer(b)
        )

      {:ok, res}
    else
      _ -> :error
    end
  end

  @spec compliance_delta(Noun.t()) :: :error | {:ok, Noun.t()}
  def compliance_delta(core) do
    with {:ok, a} <- sample(core),
         {:ok, action} <- ComplianceUnit.from_noun(a) do
      res = action |> ComplianceUnit.delta()
      {:ok, res}
    else
      _ -> :error
    end
  end

  @spec action_delta(Noun.t()) :: :error | {:ok, Noun.t()}
  def action_delta(core) do
    with {:ok, a} <- sample(core),
         {:ok, action} <- Action.from_noun(a) do
      res = action |> Action.delta()
      {:ok, res}
    else
      _ -> :error
    end
  end

  @spec make_delta(Noun.t()) :: :error | {:ok, Noun.t()}
  def make_delta(core) do
    with {:ok, a} <- sample(core),
         {:ok, set} <- Noun.Nounable.MapSet.from_noun(a),
         action_list <- set |> Enum.map(&Action.from_noun/1),
         false <- action_list |> Enum.any?(&(&1 == :error)) do
      res =
        action_list
        |> Enum.map(&Action.delta(elem(&1, 1)))
        |> Enum.reduce(2, &DeltaHash.delta_sub/2)

      {:ok, res}
    else
      _ -> :error
    end
  end

  @spec action_create(Noun.t()) :: :error | {:ok, Noun.t()}
  def action_create(core) do
    with {:ok, [con, cre | data]} <- sample(core),
         {:ok, con} <- Noun.Nounable.List.from_noun(con),
         {:ok, cre} <- Noun.Nounable.List.from_noun(cre),
         {:ok, data} <- Noun.Nounable.Map.from_noun(data),
         con_res <-
           con
           |> Enum.map(fn [key, res | root] ->
             {:ok, res} = Resource.from_noun(res)

             {Noun.atom_integer_to_binary(key, 32), res,
              Noun.atom_binary_to_integer(root)}
           end),
         cre_res <-
           cre
           |> Enum.map(fn res ->
             {:ok, res} = Resource.from_noun(res)
             res
           end),
         data_res <-
           data
           |> Enum.into(%{}, fn {key, [bin | bool]} ->
             {Noun.atom_binary_to_integer(key),
              {Noun.atom_integer_to_binary(bin), Noun.equal?(bool, 0)}}
           end) do
      {:ok, Action.create(con_res, cre_res, data_res)}
    else
      _ -> :error
    end
  end

  @spec trm_compliance_key(Noun.t()) :: :error | {:ok, Noun.t()}
  def trm_compliance_key(core) do
    with {:ok, sample} <- sample(core),
         {:ok, instance} <- CPS.Instance.from_noun(sample) do
      {:ok,
       CPS.verify_jet(
         instance.consumed,
         instance.created,
         instance.unit_delta
       )
       |> Noun.Nounable.to_noun()}
    else
      _ -> :error
    end
  end

  @spec trm_delta_key(Noun.t()) :: :error | {:ok, Noun.t()}
  def trm_delta_key(core) do
    with {:ok, sample} <- sample(core),
         {:ok, instance} <- DPS.Instance.from_noun(sample) do
      {:ok,
       DPS.verify_jet(instance.delta, instance.expected_balance)
       |> Noun.Nounable.to_noun()}
    else
      _ -> :error
    end
  end

  @spec cairo_compose(Noun.t()) :: :error | {:ok, Noun.t()}
  def cairo_compose(core) do
    with {:ok, [tx1 | tx2]} <- sample(core),
         {:ok, cairo_tx1} <- CairoResource.Transaction.from_noun(tx1),
         {:ok, cairo_tx2} <- CairoResource.Transaction.from_noun(tx2) do
      {:ok,
       CairoResource.Transaction.compose(cairo_tx1, cairo_tx2)
       |> Noun.Nounable.to_noun()}
    else
      _ -> :error
    end
  end

  ############################################################
  #                   Arithmetic Helpers                     #
  ############################################################

  defp partial_sub(x, y) do
    if x >= y, do: {:ok, x - y}, else: :error
  end

  defp compare(x, y) when x == y, do: 0
  defp compare(x, y) when x < y, do: -1
  defp compare(x, y) when x > y, do: 1

  ############################################################
  #                         Nock4K                           #
  ############################################################

  @spec nock4k(Noun.t(), Noun.t(), Nock.t()) :: {:ok, Noun.t()} | :error
  defp nock4k(subject, formula, environment \\ %Nock{}) do
    if environment.meter_pid != nil do
      send(environment.meter_pid, {:gas, 1})
    end

    try do
      case formula do
        [formula_1 = [_ | _] | formula_2] ->
          {:ok, result_1} = nock4k(subject, formula_1, environment)
          {:ok, result_2} = nock4k(subject, formula_2, environment)
          {:ok, [result_1 | result_2]}

        [zero | axis] when zero in [0, <<>>, []] and is_integer(axis) ->
          Noun.axis(axis, subject)

        [zero | axis] when zero in [0, <<>>, []] and is_binary(axis) ->
          Noun.axis(Noun.atom_binary_to_integer(axis), subject)

        [zero | axis] when zero in [0, <<>>, []] and axis == [] ->
          :error

        [one | constant] when one in [1, <<1>>] ->
          {:ok, constant}

        [two, subject_formula | formula_formula] when two in [2, <<2>>] ->
          {:ok, new_subject} = nock4k(subject, subject_formula, environment)
          {:ok, new_formula} = nock4k(subject, formula_formula, environment)
          nock4k(new_subject, new_formula, environment)

        [three | sub_formula] when three in [3, <<3>>] ->
          {:ok, sub_result} = nock4k(subject, sub_formula, environment)

          if Noun.is_noun_cell(sub_result) do
            {:ok, 0}
          else
            {:ok, 1}
          end

        [four | sub_formula] when four in [4, <<4>>] ->
          {:ok, sub_result} = nock4k(subject, sub_formula, environment)

          cond do
            sub_result == [] ->
              {:ok, 1}

            is_integer(sub_result) ->
              {:ok, sub_result + 1}

            is_binary(sub_result) ->
              {:ok, Noun.atom_binary_to_integer(sub_result) + 1}

            true ->
              :error
          end

        [five, formula_1 | formula_2] when five in [5, <<5>>] ->
          {:ok, result_1} = nock4k(subject, formula_1, environment)
          {:ok, result_2} = nock4k(subject, formula_2, environment)

          if Noun.equal?(result_1, result_2) do
            {:ok, 0}
          else
            {:ok, 1}
          end

        [six, cond | branches = [_true_branch | _false_branch]]
        when six in [6, <<6>>] ->
          {:ok, cond_plus_two} =
            nock4k(subject, [4 | [4 | cond]], environment)

          {:ok, crash_guard} =
            nock4k([2 | 3], [0 | cond_plus_two], environment)

          {:ok, branch_formula} =
            nock4k(branches, [0 | crash_guard], environment)

          nock4k(subject, branch_formula, environment)

        [seven, subject_formula | sub_formula] when seven in [7, <<7>>] ->
          {:ok, new_subject} = nock4k(subject, subject_formula, environment)
          nock4k(new_subject, sub_formula, environment)

        [eight, push_formula | sub_formula] when eight in [8, <<8>>] ->
          {:ok, pushed_noun} = nock4k(subject, push_formula, environment)
          new_subject = [pushed_noun | subject]
          nock4k(new_subject, sub_formula, environment)

        [nine, axis | sub_formula] when nine in [9, <<9>>] ->
          {:ok, sub_result} = nock4k(subject, sub_formula, environment)
          nock4k(sub_result, [2 | [[0 | 1] | [0 | axis]]], environment)

        [ten, [axis | replacement_formula] | sub_formula]
        when ten in [10, <<10>>] ->
          {:ok, replacement} =
            nock4k(subject, replacement_formula, environment)

          {:ok, sub_result} = nock4k(subject, sub_formula, environment)

          Noun.replace(
            Noun.atom_binary_to_integer(axis),
            replacement,
            sub_result
          )

        [eleven, [hint_noun | hint_formula] | sub_formula]
        when eleven in [11, <<11>>] ->
          {:ok, hint_result} = nock4k(subject, hint_formula, environment)
          Nock.process_hint(hint_noun, hint_result, environment)
          {:ok, real_result} = nock4k(subject, sub_formula, environment)
          nock4k([hint_result | real_result], [0 | 3], environment)

        [eleven, hint_noun | sub_formula] when eleven in [11, <<11>>] ->
          Nock.process_hint(hint_noun)
          nock4k(subject, sub_formula, environment)

        _ ->
          :error
      end
    rescue
      _ in MatchError -> :error
    end
  end
end
