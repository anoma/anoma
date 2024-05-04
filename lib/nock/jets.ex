defmodule Nock.Jets do
  @moduledoc """
  Jets for the Nock interpreter, taking a gate core. Not fully general.
  """

  import Noun

  @spec calculate_mug_of_core(non_neg_integer(), non_neg_integer()) ::
          non_neg_integer()
  @doc """
  We calculate the mug of a given core at a given gate.

  ### Parameters

  - `index_in_core` - the index of the gate itself

  - `parent_layer` - the layer of the standard library. This should be
    the same as the numbers found in `anoma.hoon` with `+ 3` added for
    `@rm_core_val`, `@stdlib_core_val`, and the current index being
    pushed

  ### Example

  In the Hoon repl one should write

      dojo> |commit %anoma
      >=
      dojo> =anoma -build-file /=anoma=/lib/anoma/hoon
      dojo> =>  anoma  !=(sub)
      [9 47 0 31]

  Now in IEX

      > Nock.Jets.calculate_mug_of_core(47, 7)
      14801825384048474882

  We derived the 7 because the current top layer is 5, and dec is at
  layer 1. So 5 - 1 + 3 = 7

  """
  def calculate_mug_of_core(index_in_core, parent_layer) do
    {:ok, core} = calculate_core(index_in_core, parent_layer)
    Noun.mug(hd(core))
  end

  @doc """
  We calculate the mug of a given layer at any given gate.

  ### Parameters

  - `index_in_core` - the index of the gate whose parents we wish to
    learn about

  - `parent_layer` - the layer of the standard library. This should be
    the same as the numbers found in `anoma.hoon` with `+ 3` added for
    `@rm_core_val`, `@stdlib_core_val`, and the current index being
    pushed

  ### Example

  In the Hoon repl one should write

      dojo> |commit %anoma
      >=
      dojo> =anoma -build-file /=anoma=/lib/anoma/hoon
      dojo> =>  anoma  !=(sub)
      [9 47 0 31]

  Now in IEX

      > Nock.Jets.calculate_mug_of_layer(47, 7)
      17654928022549292273

  We derived the 7 because the current top layer is 5, and dec is at
  layer 1. So 5 - 1 + 3 = 7

  This value should match Nock.@layer_1_contex_mug
  """
  @spec calculate_mug_of_layer(non_neg_integer(), non_neg_integer()) ::
          non_neg_integer()
  def calculate_mug_of_layer(index_in_core, parent_layer) do
    with {:ok, core} <- calculate_core(index_in_core, parent_layer),
         {:ok, parent} <- Noun.axis(7, core) do
      Noun.mug(parent)
    end
  end

  @spec calculate_core(non_neg_integer(), non_neg_integer()) ::
          :error | {:ok, Noun.t()}
  defp calculate_core(index_in_core, parent_layer) do
    Nock.nock(Nock.logics_core(), [
      8,
      [9, index_in_core, 0 | Noun.index_to_offset(parent_layer)],
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

  def dec(core) do
    maybe_sample = sample(core)

    case maybe_sample do
      {:ok, sample} when is_noun_atom(sample) and sample > 0 ->
        {:ok, sample - 1}

      _ ->
        :error
    end
  end

  def add(core) do
    maybe_sample = sample(core)

    case maybe_sample do
      {:ok, [a | b]} when is_noun_atom(a) and is_noun_atom(b) ->
        {:ok, a + b}

      _ ->
        :error
    end
  end

  def sub(core) do
    maybe_sample = sample(core)

    case maybe_sample do
      {:ok, [a | b]} when is_noun_atom(a) and is_noun_atom(b) and a >= b ->
        {:ok, a - b}

      _ ->
        :error
    end
  end

  def lth(core) do
    maybe_sample = sample(core)

    case maybe_sample do
      {:ok, [a | b]} when is_noun_atom(a) and is_noun_atom(b) and a < b ->
        {:ok, 0}

      {:ok, [a | b]} when is_noun_atom(a) and is_noun_atom(b) ->
        {:ok, 1}

      _ ->
        :error
    end
  end

  def lte(core) do
    maybe_sample = sample(core)

    case maybe_sample do
      {:ok, [a | b]} when is_noun_atom(a) and is_noun_atom(b) and a <= b ->
        {:ok, 0}

      {:ok, [a | b]} when is_noun_atom(a) and is_noun_atom(b) ->
        {:ok, 1}

      _ ->
        :error
    end
  end

  def gth(core) do
    maybe_sample = sample(core)

    case maybe_sample do
      {:ok, [a | b]} when is_noun_atom(a) and is_noun_atom(b) and a > b ->
        {:ok, 0}

      {:ok, [a | b]} when is_noun_atom(a) and is_noun_atom(b) ->
        {:ok, 1}

      _ ->
        :error
    end
  end

  def gte(core) do
    maybe_sample = sample(core)

    case maybe_sample do
      {:ok, [a | b]} when is_noun_atom(a) and is_noun_atom(b) and a >= b ->
        {:ok, 0}

      {:ok, [a | b]} when is_noun_atom(a) and is_noun_atom(b) ->
        {:ok, 1}

      _ ->
        :error
    end
  end

  def mul(core) do
    maybe_sample = sample(core)

    case maybe_sample do
      {:ok, [a | b]} when is_noun_atom(a) and is_noun_atom(b) ->
        {:ok, a * b}

      _ ->
        :error
    end
  end

  def div(core) do
    maybe_sample = sample(core)

    case maybe_sample do
      {:ok, [a | b]} when is_noun_atom(a) and is_noun_atom(b) and b != 0 ->
        {:ok, Kernel.div(a, b)}

      _ ->
        :error
    end
  end

  def mod(core) do
    maybe_sample = sample(core)

    case maybe_sample do
      {:ok, [a | b]} when is_noun_atom(a) and is_noun_atom(b) and b != 0 ->
        {:ok, Kernel.rem(a, b)}

      _ ->
        :error
    end
  end
end
