defmodule TestHelper.Nock do
  @moduledoc """
  I am a testing module that has some common definitions for nock
  functions.
  """

  import Nock

  @spec using_dec_core() :: Noun.t()
  def using_dec_core() do
    arm = Noun.Format.parse_always("[8 [9 342 0 7] 9 2 10 [6 0 14] 0 2]")
    sample = 999
    [arm, sample | stdlib_core()]
  end

  @spec factorial() :: Noun.t()
  def factorial() do
    arm = Noun.Format.parse_always("
    [ 8
      [1 1 0]
      8
      [ 1
        6
        [5 [0 30] 1 0]
        [0 13]
        9
        2
        10
        [30 8 [9 342 0 31] 9 2 10 [6 0 62] 0 2]
        10
        [6 [8 [9 20 0 31] 9 2 10 [6 [0 29] 0 28] 0 2] 0 12]
        0
        1
      ]
      9
      2
      0
      1
    ]")
    sample = 1
    [arm, sample | stdlib_core()]
  end
end
