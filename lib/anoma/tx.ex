defmodule Anoma.Tx do
  @moduledoc """
  Sample transactions.
  """

  @dialyzer :no_improper_lists

  # %ctr
  @counter_name_val 7_500_899
  def counter_name do
    @counter_name_val
  end

  # [%ctr 0]
  @zero_counter_val [
    Noun.Format.parse_always("[1 7.500.899 0]"),
    0 | Nock.stdlib_core()
  ]
  def zero_counter do
    @zero_counter_val
  end

  # [%ctr a]
  def set_counter(a) do
    [[1, 7_500_899 | a], 0 | Nock.stdlib_core()]
  end

  # [%ctr .+(.^(/[order]/ctr))]
  @increment_counter_val [
    Noun.Format.parse_always(
      "[[1 7.500.899] 4 12 [1 0] [0 6] 1 7.500.899 0]"
    ),
    0 | Nock.stdlib_core()
  ]
  def increment_counter do
    @increment_counter_val
  end

  # [%ctr (add a .^(/[order]/ctr))]
  def add_to_counter(a) do
    [
      [
        [1 | @counter_name_val],
        8,
        [9, 20, 0 | 7],
        9,
        2,
        10,
        [
          6,
          [7, [0 | 3], 1 | a],
          7,
          [0 | 3],
          12,
          [1 | 0],
          [0 | 6],
          1,
          @counter_name_val | 0
        ],
        0
        | 2
      ],
      0
      | Nock.stdlib_core()
    ]
  end
end
