defmodule Examples.EDefNock do
  use Nock.DefNock

  defnock abc do
    a = 2
  end
end
