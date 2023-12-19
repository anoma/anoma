defmodule Nock.Jets do
  @moduledoc """
  Jets for the Nock interpreter, taking a gate core. Not fully general.
  """

  def sample(core) do
    {:ok, sample} = Noun.axis(6, core)
    sample
  end

  def dec(core) do
    sample = sample(core)

    if sample > 0 do
      {:ok, sample - 1}
    else
      :error
    end
  end

  def add(core) do
    [a | b] = sample(core)
    {:ok, a + b}
  end

  def sub(core) do
    [a | b] = sample(core)

    if a >= b do
      {:ok, a - b}
    else
      :error
    end
  end

  def lth(core) do
    [a | b] = sample(core)

    if !(is_integer(a) && is_integer(b)) do
      :error
    end

    if a < b do
      {:ok, 0}
    else
      {:ok, 1}
    end
  end

  def lte(core) do
    [a | b] = sample(core)

    if !(is_integer(a) && is_integer(b)) do
      :error
    end

    if a <= b do
      {:ok, 0}
    else
      {:ok, 1}
    end
  end

  def gth(core) do
    [a | b] = sample(core)

    if !(is_integer(a) && is_integer(b)) do
      :error
    end

    if a > b do
      {:ok, 0}
    else
      {:ok, 1}
    end
  end

  def gte(core) do
    [a | b] = sample(core)

    if !(is_integer(a) && is_integer(b)) do
      :error
    end

    if a >= b do
      {:ok, 0}
    else
      {:ok, 1}
    end
  end

  def mul(core) do
    [a | b] = sample(core)
    {:ok, a * b}
  end

  def div(core) do
    [a | b] = sample(core)
    {:ok, Kernel.div(a, b)}
  end

  def mod(core) do
    [a | b] = sample(core)
    {:ok, Kernel.rem(a, b)}
  end
end
