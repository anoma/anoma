defmodule Examples.EDefNock do
  use Nock.DefNock

  # 'example' is an Elixir binding. i.e., example = {type, nock}
  # the below should compile to
  # {core<a : atom><{stdlib type}>, [[1 123] <stdlib>]}
  defcore example do
    arm a do
      123
    end
  end

  # perhaps the syntax could be tightened to escape to the side less
  # should compile to
  # {core<const_zero : core<$ : atom><[atom {stdlib type}]<stdlib type>, <code>}
  # i want them to be callable like const_zero(123) elsewhere
  defcore library do
    arm const_zero do
      gate a = noun do
        0
      end
    end
  end

  # not sure how this should look...
  defcore types do
    arm label do
      gate a = noun do
        a = a
      end
    end
  end
end
