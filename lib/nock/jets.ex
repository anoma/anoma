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
end
