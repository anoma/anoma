defmodule Nock.Jets do
  @moduledoc """
  Jets for the Nock interpreter, taking a gate core. Not fully general.
  """

  import Noun
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
  def calculate_mug_of_layer(parent_layer) do
    # A core with a single gate will always have index 4 populated by
    # a gate
    with {:ok, core} <- calculate_core(4, parent_layer),
         {:ok, parent} <- Noun.axis(7, core) do
      Noun.mug(parent)
    end
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
            {:ok, val} -> {:ok, val}
            {:error, _} -> :error
          end
        rescue
          _ in ArgumentError -> :error
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
end
