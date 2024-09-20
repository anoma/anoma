defmodule Examples.EDefNock do
  use Nock.DefNock

  defnock abc do
    a = 2
  end

  defnock equals do
    123 == 456
  end
end
