defmodule Nock.Jets do
  @moduledoc """
  Jets for the Nock interpreter, taking a gate core. Not fully general.
  """

  import Noun

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
