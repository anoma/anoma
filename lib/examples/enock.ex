defmodule Examples.ENock do
  use Memoize

  require ExUnit.Assertions

  alias Noun.Format

  ####################################################################
  ##                        Resource Logics                         ##
  ####################################################################

  # saves 5 microseconds on second use, should we bother, is this a
  # bad pattern?
  @spec zero_delta_logic() :: Noun.t()
  defmemo zero_delta_logic() do
    Format.parse_always("[[5 [1 0] [0 446]] 0 0]")
  end

  @spec counter_logic() :: Noun.t()
  def counter_logic() do
    [
      Format.parse_always("""
      [ 6
        [5 [1 1] 8 [9 1.406 0 511] 9 2 10 [6 0 118] 0 2]
        [6
          [5 [1 1] 8 [9 1.406 0 511] 9 2 10 [6 0 238] 0 2]
          [6
            [5 [1 1] 8 [9 1.406 0 511] 9 2 10 [6 0 958] 0 2]
            [6 [5 [1 0] 0 446] [0 0] 6 [0 3.570] [1 0] 1 1] 1 1] 1 1]
            1
            1
          ]
      """),
      0 | Nock.logics_core()
    ]
  end
end
