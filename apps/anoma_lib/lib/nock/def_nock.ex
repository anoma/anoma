defmodule Nock.DefNock do
  defmacro __using__(_opts) do
    quote do
      import Nock.DefNock
    end
  end

  defmacro defnock(name, do: block) do
    quote do
      def unquote(name) do
        unquote(block_to_nock(block, 0))
      end
    end
  end

  def block_to_nock({:__block__, _ctx, exprs}, subject) do
    
  end

  def block_to_nock({:==, _ctx, [left, right]}, subject) do
    quote do
      [
        5,
        unquote(block_to_nock(left, subject))
        | unquote(block_to_nock(right, subject))
      ]
    end
  end

  def block_to_nock(a, _) do
    quote do
      [1 | unquote(a)]
    end
  end
end
