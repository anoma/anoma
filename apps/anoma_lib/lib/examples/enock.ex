defmodule Examples.ENock do
  alias Anoma.RM.Transparent.Action
  alias Anoma.RM.Transparent.Transaction
  alias Anoma.RM.Transparent.Primitive.DeltaHash
  alias Examples.ECrypto
  alias Examples.ETransparent.EAction

  require ExUnit.Assertions

  import ExUnit.Assertions
  import Noun

  use Memoize

  ####################################################################
  ##                        Resource Logics                         ##
  ####################################################################

  # saves 5 microseconds on second use, should we bother, is this a
  # bad pattern?
  @spec zero_delta_logic() :: Noun.t()
  defmemo zero_delta_logic() do
    Noun.Format.parse_always("[[5 [1 0] [0 446]] 0 0]")
  end

  @doc """
  The counter arm.

  Availiable through `counter:logics` core.
  """

  @spec counter_arm() :: Noun.t()
  def counter_arm() do
    """
    [ 6
    [5 [1 1] 8 [9 1.406 0 1.023] 9 2 10 [6 0 118] 0 2]
    [6 [5 [1 1] 8 [9 1.406 0 1.023] 9 2 10 [6 0 238] 0 2] [6 [5 [1 1] 8 [9 1.406 0 1.023] 9 2 10 [6 0 958] 0 2] [6 [5 [1 0] 0 446] [0 0] 6 [0 3.570] [1 0] 1 1] 1 1] 1 1]
    1
    1
    ]
    """
    |> Noun.Format.parse_always()
  end

  @spec counter_logic() :: Noun.t()
  def counter_logic() do
    [
      counter_arm(),
      0 | Nock.Lib.logics_core()
    ]
  end

  #### Term Examples

  @spec zero(Noun.t()) :: Noun.t()
  def zero(key \\ "key") do
    zero_counter_arm = [1, [key] | 0]
    arm = [10, [2 | zero_counter_arm], 1, 0 | 0]
    sample = 0
    keyspace = 0
    [[8, [1 | sample], [1 | keyspace], [1 | arm], 0 | 1] | 999]
  end

  @spec inc(Noun.t()) :: Noun.t()
  def inc(key \\ "key") do
    increment_value_arm = [[1 | [key]], 4, 12, [1 | 0], [0 | 6], 1, [key] | 0]
    # Place the result in a list
    arm = [10, [2 | increment_value_arm], 1, 0 | 0]
    sample = 0
    keyspace = 0
    [[8, [1 | sample], [1 | keyspace], [1 | arm], 0 | 1] | 999]
  end

  ####################################################################
  ##                           Noun fun                             ##
  ####################################################################

  @spec one_two() :: Noun.t()
  def one_two() do
    assert {:ok, term = [1 | 2]} = Noun.Format.parse("[1 2]"),
           "Noun should parse sensibly"

    Noun.normalize_noun(term)
  end

  @spec nesting_noun() :: Noun.t()
  def nesting_noun() do
    assert {:ok, term = [1, [3 | 5] | 2]} =
             Noun.Format.parse("[1 [[3 5] 2]]"),
           "Basic Nouns should parse sensibly"

    term
  end

  @spec incorrectly_nested_noun() :: :error
  def incorrectly_nested_noun() do
    term = Noun.Format.parse("[[[[1]]]]")
    assert :error = term
    term
  end

  @spec incorrectly_ending() :: :error
  def incorrectly_ending() do
    term = Noun.Format.parse("[[[[")
    assert :error = term
    term
  end

  @spec incorrectly_starting() :: :error
  def incorrectly_starting() do
    term = Noun.Format.parse("]]]]")
    assert :error = term
    term
  end

  @spec indexed_noun() :: Noun.t()
  def indexed_noun() do
    assert {:ok, term} = Noun.Format.parse("[[4 5] [12 13] 7]")
    assert {:ok, term} == Noun.axis(1, term)
    assert {:ok, [4 | 5]} == Noun.axis(2, term)
    assert {:ok, [12 | 13]} == Noun.axis(6, term)
    assert {:ok, 7} == Noun.axis(7, term)
    assert {:ok, 4} == Noun.axis(4, term)
    assert {:ok, 5} == Noun.axis(5, term)
    assert {:ok, 12} == Noun.axis(12, term)
    assert {:ok, 13} == Noun.axis(13, term)
    term
  end

  @spec replacing_terms() :: Noun.t()
  def replacing_terms() do
    assert {:ok, term} = Noun.replace(2, 2, indexed_noun())
    assert {:ok, 2} == Noun.axis(2, term)
    term
  end

  ####################################################################
  ##                       Noun Transactions                        ##
  ####################################################################

  @spec trivial_swap() :: Noun.t()
  def trivial_swap() do
    swap = Examples.ETransparent.ETransaction.swap_from_actions()
    noun = swap |> Noun.Nounable.to_noun()
    {:ok, cued} = noun |> Noun.Jam.jam() |> Noun.Jam.cue()
    {:ok, cued_trans} = Transaction.from_noun(cued)

    assert Transaction.from_noun(noun) == {:ok, swap}
    assert Transaction.verify(cued_trans)

    noun
  end

  @spec trivial_swap_no_eph() :: Noun.t()
  def trivial_swap_no_eph() do
    Examples.ETransparent.ETransaction.swap_from_actions_non_eph_nullifier()
    |> Noun.Nounable.to_noun()
  end

  ####################################################################
  ##                        Noun Submission                         ##
  ####################################################################

  @spec transparent_core(Noun.t()) :: Noun.t()
  def transparent_core(
        tx_noun \\ Examples.ETransparent.ETransaction.swap_from_actions()
        |> Noun.Nounable.to_noun()
      ) do
    trivial_swap_arm = [1 | tx_noun]
    keyspace = 0
    swap = [[1, keyspace, trivial_swap_arm, 0 | 909], 0 | 707]
    swap
  end

  ####################################################################
  ##                          Jetted Cores                          ##
  ##    Requires special testing to ensure they behave properly.    ##
  ####################################################################

  @spec example_layer_depth(non_neg_integer) :: non_neg_integer
  defp example_layer_depth(layer),
    do: (Nock.Lib.stdlib_layers() - layer + 4) |> Noun.index_to_offset()

  @doc """
  The decrement arm in the tests core.

  Availiable through `use-dec:tests` core.
  """
  @spec dec_arm() :: Noun.t()
  def dec_arm() do
    layer_depth = example_layer_depth(1)

    "[8 [9 342 0 #{layer_depth}] 9 2 10 [6 0 14] 0 2]"
    |> Noun.Format.parse_always()
  end

  @spec dec() :: Noun.t()
  def dec() do
    sample = 999
    core = [dec_arm(), sample | Nock.Lib.logics_core()]

    assert Nock.nock(core, [9, 2, 0 | 1]) |> elem(1) |> Noun.equal?(998)

    assert Nock.nock(core, [9, 2, 10, [6, 1 | 22], 0 | 1])
           |> elem(1)
           |> Noun.equal?(21)

    assert Nock.nock(core, [9, 2, 10, [6, 1 | <<22>>], 0 | 1])
           |> elem(1)
           |> Noun.equal?(21),
           "dec works on internally binary atoms"

    core
  end

  @doc """
  A cue arm for taking cue:anoma out of the logics core environment.

  Can be gotten by defining gate locally as

  =localcue   =>  logics  |=  a=@  (cue a)

  and then grabbing the arm of localcue.
  """

  @spec cue_arm() :: Noun.t()
  def cue_arm() do
    layer_depth = example_layer_depth(5)

    "[8 [9 94 0 #{layer_depth}] 9 2 10 [6 0 14] 0 2]"
    |> Noun.Format.parse_always()
  end

  @spec cue() :: Noun.t()
  def cue() do
    sample = 999
    core = [cue_arm(), sample | Nock.Lib.logics_core()]

    core
  end

  @doc """
  A cue arm for taking jam:anoma out of the logics core environment.

  Can be gotten by defining gate locally as

  =localjam   =>  logics  |=  a=@  (jam a)

  and then grabbing the arm of localjam.
  """

  @spec jam_arm() :: Noun.t()
  def jam_arm() do
    layer_depth = example_layer_depth(5)

    "[8 [9 22 0 #{layer_depth}] 9 2 10 [6 0 14] 0 2]"
    |> Noun.Format.parse_always()
  end

  @spec jam() :: Noun.t()
  def jam() do
    sample = 999
    core = [jam_arm(), sample | Nock.Lib.logics_core()]

    core
  end

  @doc """
  The sign arm for taking sign:anoma from the logics core environment.

  Can be gotten by defining gate locally as:

  =localsign   =>  logics  |=  [a=@ b=@]  (sign [a b])

  and then grabbing the arm of localsign.
  """

  @spec sign_arm() :: Noun.t()
  def sign_arm() do
    layer_depth = example_layer_depth(6)

    "[8 [9 10 0 #{layer_depth}] 9 2 10 [6 7 [0 3] [0 12] 0 13] 0 2]"
    |> Noun.Format.parse_always()
  end

  @spec sign() :: Noun.t()
  def sign() do
    sample = [999 | 888]
    core = [sign_arm(), sample | Nock.Lib.logics_core()]

    valid_args = [ECrypto.blood_msg() | ECrypto.londo().internal.sign]

    assert Nock.nock(core, [9, 2, 10, [6, 1 | valid_args], 0 | 1])
           |> elem(1)
           |> Noun.equal?(ECrypto.blood_l_signed())

    core
  end

  @doc """
  The verify arm for taking verify:anoma from the logics core environment.

  Can be gotten by defining gate locally as:

  =localverify   =>  logics  |=  [a=@ b=@]  (verify [a b])

  and then grabbing the arm of localverify.
  """

  @spec verify_arm() :: Noun.t()
  def verify_arm() do
    layer_depth = example_layer_depth(6)

    "[8 [9 4 0 #{layer_depth}] 9 2 10 [6 7 [0 3] [0 12] 0 13] 0 2]"
    |> Noun.Format.parse_always()
  end

  @spec verify() :: Noun.t()
  def verify() do
    sample = [999 | 888]
    core = [verify_arm(), sample | Nock.Lib.logics_core()]

    valid_args = [ECrypto.blood_l_signed() | ECrypto.londo().external.sign]
    invalid_args = [ECrypto.blood_msg() | ECrypto.londo().internal.sign]

    assert Nock.nock(core, [9, 2, 10, [6, 1 | valid_args], 0 | 1])
           |> elem(1)
           |> Noun.equal?([0 | ECrypto.blood_msg()])

    assert Nock.nock(core, [9, 2, 10, [6, 1 | invalid_args], 0 | 1])
           |> elem(1)
           |> Noun.equal?(0),
           "Can't verify with someone's private key"

    core
  end

  @doc """
  The sign-detatched arm for taking sign-detached:anoma from the logics core environment.

  Can be gotten by defining gate locally as:

  =localsigndetached   =>  logics  |=  [a=@ b=@]  (sign-detached [a b])

  and then grabbing the arm of localsighdetached.
  """

  @spec sign_detatched_arm() :: Noun.t()
  def sign_detatched_arm() do
    layer_depth = example_layer_depth(6)

    "[8 [9 23 0 #{layer_depth}] 9 2 10 [6 7 [0 3] [0 12] 0 13] 0 2]"
    |> Noun.Format.parse_always()
  end

  @spec sign_detatched() :: Noun.t()
  def sign_detatched() do
    sample = [999 | 888]
    core = [sign_detatched_arm(), sample | Nock.Lib.logics_core()]

    valid_args = [ECrypto.blood_msg() | ECrypto.londo().internal.sign]

    assert Nock.nock(core, [9, 2, 10, [6, 1 | valid_args], 0 | 1])
           |> elem(1)
           |> Noun.equal?(ECrypto.blood_l_signed_detached())

    core
  end

  @doc """
  The verify-detatched arm for taking verify-detached:anoma from the logics core environment.

  Can be gotten by defining gate locally as:

  =localverifydetached   =>  logics  |=  [a=@ b=@ c=@]  (verify-detached [a b])

  and then grabbing the arm of localverifydetached.
  """

  @spec verify_detatched_arm() :: Noun.t()
  def verify_detatched_arm() do
    layer_depth = example_layer_depth(6)

    "[8 [9 22 0 #{layer_depth}] 9 2 10 [6 7 [0 3] [0 12] [0 26] 0 27] 0 2]"
    |> Noun.Format.parse_always()
  end

  @spec verify_detatched() :: Noun.t()
  def verify_detatched() do
    sample = [999 | 888]
    core = [verify_detatched_arm(), sample | Nock.Lib.logics_core()]

    sign = ECrypto.blood_l_signed_detached()
    valid = [sign, ECrypto.blood_msg() | ECrypto.londo().external.sign]
    wrong_msg = [sign, <<3>> | ECrypto.londo().external.sign]
    invalid_key = [sign, ECrypto.blood_msg() | ECrypto.londo().internal.sign]

    invalid_size = [
      <<3>>,
      ECrypto.blood_msg() | ECrypto.londo().external.sign
    ]

    all_invalid = [<<3>>, <<4>> | <<55>>]

    assert Nock.nock(core, [9, 2, 10, [6, 1 | valid], 0 | 1])
           |> elem(1)
           |> Noun.equal?(0)

    assert Nock.nock(core, [9, 2, 10, [6, 1 | wrong_msg], 0 | 1])
           |> elem(1)
           |> Noun.equal?(1)

    assert Nock.nock(core, [9, 2, 10, [6, 1 | invalid_key], 0 | 1])
           |> elem(1)
           |> Noun.equal?(1),
           "private key deosn't verify"

    assert Nock.nock(core, [9, 2, 10, [6, 1 | invalid_size], 0 | 1])
           |> elem(1)
           |> Noun.equal?(1),
           "Gracefully fail on invalidly sized messages"

    assert Nock.nock(core, [9, 2, 10, [6, 1 | all_invalid], 0 | 1])
           |> elem(1)
           |> Noun.equal?(1),
           "Everything being wrong, doesn't excuse a crash"

    core
  end

  @doc """
  The bex arm for taking bex:anoma from the logics core environment.

  Can be gotten by defining gate locally as:

  =localbex   =>  logics  |=  a=@  (bex a)

  and then grabbing the arm of localbex.
  """

  @spec bex_arm() :: Noun.t()
  def bex_arm() do
    layer_depth = example_layer_depth(4)

    "[8 [9 4 0 #{layer_depth}] 9 2 10 [6 0 14] 0 2]"
    |> Noun.Format.parse_always()
  end

  @spec bex() :: Noun.t()
  def bex() do
    sample = 888
    core = [bex_arm(), sample | Nock.Lib.logics_core()]

    assert Nock.nock(core, [9, 2, 10, [6, 1 | 2], 0 | 1])
           |> elem(1)
           |> Noun.equal?(4)

    assert Nock.nock(core, [9, 2, 10, [6, 1 | 5], 0 | 1])
           |> elem(1)
           |> Noun.equal?(32)

    assert Nock.nock(core, [9, 2, 10, [6, 1 | 28], 0 | 1])
           |> elem(1)
           |> Noun.equal?(268_435_456)

    core
  end

  @doc """
  The mix arm for taking mix:anoma from the logics core environment.

  Can be gotten by defining gate locally as:

  =localmix   =>  logics  |=  [a=@ b=@]  (mix [a b])

  and then grabbing the arm of locamix.
  """

  @spec mix_arm() :: Noun.t()
  def mix_arm() do
    layer_depth = example_layer_depth(5)

    "[8 [9 4 0 #{layer_depth}] 9 2 10 [6 7 [0 3] [0 12] 0 13] 0 2]"
    |> Noun.Format.parse_always()
  end

  @spec mix() :: Noun.t()
  def mix() do
    sample = [0 | 0]
    core = [mix_arm(), sample | Nock.Lib.logics_core()]

    assert Nock.nock(core, [9, 2, 10, [6, 1, 3 | 5], 0 | 1])
           |> elem(1)
           |> Noun.equal?(6)

    assert Nock.nock(core, [9, 2, 10, [6, 1, 11 | 11], 0 | 1])
           |> elem(1)
           |> Noun.equal?(0)

    core
  end

  @doc """
  The mat arm for taking mat:anoma from the logics core environment.

  Can be gotten by defining gate locally as:

  =localmat   =>  logics  |=  a  (mat a)

  and then grabbing the arm of locamix.
  """

  @spec mat_arm() :: Noun.t()
  def mat_arm() do
    layer_depth = example_layer_depth(5)

    "[8 [9 43 0 #{layer_depth}] 9 2 10 [6 0 14] 0 2]"
    |> Noun.Format.parse_always()
  end

  # Please make some assertions ☹
  @spec mat() :: Noun.t()
  def mat() do
    sample = 0
    core = [mat_arm(), sample | Nock.Lib.logics_core()]

    core
  end

  @doc """
  The shax arm for taking shax:anoma from the logics core environment.

  Can be gotten by defining gate locally as:

  =localshax   =>  logics  |=  a=@  (shax a)

  and then grabbing the arm of localshax.
  """

  @spec shax_arm() :: Noun.t()
  def shax_arm() do
    layer_depth = example_layer_depth(7)

    "[8 [9 22 0 #{layer_depth}] 9 2 10 [6 0 14] 0 2]"
    |> Noun.Format.parse_always()
  end

  @spec shax() :: Noun.t()
  def shax() do
    sample = 0
    core = [shax_arm(), sample | Nock.Lib.logics_core()]

    assert Nock.nock(core, [9, 2, 0 | 1])
           |> elem(1)
           |> Noun.equal?(
             38_772_261_170_797_515_502_142_737_251_560_910_253_885_555_854_579_348_417_967_781_179_871_348_437_219
           )

    assert Nock.nock(core, [9, 2, 10, [6, 1 | 7], 0 | 1])
           |> elem(1)
           |> Noun.equal?(
             55_140_411_965_103_990_925_642_572_973_048_070_470_495_109_172_463_110_593_783_713_869_232_563_762_634
           )

    core
  end

  ############################################################
  ##                      OG Cores                          ##
  ############################################################

  @doc """
  I represent the raw gate call as a 2-argument gate.

  Can be gotten by defining

  =lraw   =>  logics  |=   [a=@ b=@]  (~(raw og a) b)
  """

  @spec raw_arm() :: Noun.t()
  def raw_arm() do
    layer_depth = example_layer_depth(7)

    arm =
      "[8 [8 [9 47 0 #{layer_depth}] 9 23 10 [6 0 28] 0 2] 9 2 10 [6 0 29] 0 2]"
      |> Noun.Format.parse_always()

    sample = [0, 0]

    [arm, sample | Nock.Lib.logics_core()]
  end

  @doc """
  I am function calling the raw gate of the og door with specified
  seed and bitwidth.
  """

  @spec raw_call(Noun.t(), Noun.t()) :: {:ok, Noun.t()}
  def raw_call(seed, width) do
    Nock.nock(raw_arm(), [9, 2, 10, [6, 1 | [seed | width]], 0 | 1])
  end

  @spec raw_27_4() :: {:ok, Noun.t()}
  def raw_27_4() do
    call = raw_call(27, 4)
    assert call |> elem(1) |> Noun.equal?(9)

    call
  end

  @doc """
  I represent the raws gate call as a 2-argument gate.

  Can be gotten by defining

  =lraw   =>  logics  |=   [a=@ b=@]  (~(raws og a) b)
  """

  @spec raws_arm() :: Noun.t()
  def raws_arm() do
    layer_depth = example_layer_depth(7)

    arm =
      "[8 [8 [9 47 0 #{layer_depth}] 9 4 10 [6 0 28] 0 2] 9 2 10 [6 0 29] 0 2]"
      |> Noun.Format.parse_always()

    sample = [0, 0]

    [arm, sample | Nock.Lib.logics_core()]
  end

  @doc """
  I am function calling the raws gate of the og door with specified
  seed and bitwidth.
  """

  @spec raws_call(Noun.t(), Noun.t()) :: {:ok, Noun.t()}
  def raws_call(seed, width) do
    Nock.nock(raws_arm(), [9, 2, 10, [6, 1 | [seed | width]], 0 | 1])
  end

  @spec raws_test() :: :ok
  def raws_test() do
    {:ok, res} = raws_call(27, 4)
    rand = hd(res)
    assert Noun.equal?(rand, 9)

    :ok
  end

  @doc """
  I represent the rad gate call as a 2-argument gate.

  Can be gotten by defining

  =lrad   =>  logics  |=   [a=@ b=@]  (~(rad og a) b)
  """

  @spec rad_arm() :: Noun.t()
  def rad_arm() do
    layer_depth = example_layer_depth(7)

    arm =
      "[8 [8 [9 47 0 #{layer_depth}] 9 20 10 [6 0 28] 0 2] 9 2 10 [6 0 29] 0 2]"
      |> Noun.Format.parse_always()

    sample = [0, 0]

    [arm, sample | Nock.Lib.logics_core()]
  end

  @doc """
  I am function calling the rad gate of the og door with specified
  seed and range
  """

  @spec rad_call(any(), non_neg_integer()) :: {:ok, Noun.t()}
  def rad_call(seed, range) do
    Nock.nock(rad_arm(), [9, 2, 10, [6, 1 | [seed | range]], 0 | 1])
  end

  @spec rad_tests() :: {:ok, Noun.t()}
  def rad_tests() do
    assert rad_call(5, 11) |> elem(1) |> Noun.equal?(4)
    assert rad_call(10, 20) |> elem(1) |> Noun.equal?(2)
    assert rad_call(10, 2000) |> elem(1) |> Noun.equal?(260)
    assert rad_call(628, 2000) |> elem(1) |> Noun.equal?(1285)

    rad_call(628, 2000)
  end

  @doc """
  I represent the rads gate call as a 2-argument gate.

  Can be gotten by defining

  =lrad   =>  logics  |=   [a=@ b=@]  (~(rads og a) b)
  """

  @spec rads_arm() :: Noun.t()
  def rads_arm() do
    layer_depth = example_layer_depth(7)

    arm =
      "[8 [8 [9 47 0 #{layer_depth}] 9 22 10 [6 0 28] 0 2] 9 2 10 [6 0 29] 0 2]"
      |> Noun.Format.parse_always()

    sample = [0, 0]

    [arm, sample | Nock.Lib.logics_core()]
  end

  @doc """
  I am function calling the rads gate of the og door with specified
  seed and range
  """

  @spec rads_call(non_neg_integer(), non_neg_integer()) ::
          :error | {:ok, Noun.t()}
  def rads_call(seed, range) do
    Nock.nock(rads_arm(), [9, 2, 10, [6, 1 | [seed | range]], 0 | 1])
  end

  @spec rads_tests() :: :ok
  def rads_tests() do
    {:ok, cell} = rads_call(5, 11)
    rand = hd(cell)
    assert Noun.equal?(rand, 4)
    :ok
  end

  ############################################################
  ##                  Signed arithmetic                     ##
  ############################################################

  @spec abs_arm() :: Noun.t()
  def abs_arm() do
    layer_depth = example_layer_depth(8)

    "[8 [9 1.515 0 #{layer_depth}] 9 2 10 [6 0 14] 0 2]"
    |> Noun.Format.parse_always()
  end

  @spec abs() :: Noun.t()
  def abs() do
    arm = abs_arm()
    sample = 888
    core = [arm, sample | Nock.Lib.logics_core()]

    # abs(--0) == 0
    assert Nock.nock(core, [9, 2, 10, [6, 1 | 0], 0 | 1])
           |> elem(1)
           |> Noun.equal?(0)

    # abs(-2) == 2
    assert Nock.nock(core, [9, 2, 10, [6, 1 | 3], 0 | 1])
           |> elem(1)
           |> Noun.equal?(2)

    # abs(--2) == 2
    assert Nock.nock(core, [9, 2, 10, [6, 1 | 4], 0 | 1])
           |> elem(1)
           |> Noun.equal?(2)

    core
  end

  @doc """
  I represent the dif gate call as a 2-argument gate.

  Can be obtained by defining

  =ldif =>  logics  |=   [a=@ b=@]  (dif [a b])

  and computing

  .*  ldif  [0 2]
  """
  @spec dif_arm() :: Noun.t()
  def dif_arm() do
    layer_depth = example_layer_depth(8)

    "[8 [9 759 0 #{layer_depth}] 9 2 10 [6 7 [0 3] [0 12] 0 13] 0 2]"
    |> Noun.Format.parse_always()
  end

  @spec dif() :: Noun.t()
  def dif() do
    arm = dif_arm()
    sample = [888 | 999]
    core = [arm, sample | Nock.Lib.logics_core()]

    # --3 - -2 == --5
    assert Nock.nock(core, [9, 2, 10, [6, 1 | [6 | 3]], 0 | 1])
           |> elem(1)
           |> Noun.equal?(10)

    # -3 - --2 == -5
    assert Nock.nock(core, [9, 2, 10, [6, 1 | [5 | 4]], 0 | 1])
           |> elem(1)
           |> Noun.equal?(9)

    core
  end

  @doc """
  I represent the dul gate call as a 2-argument gate.

  Can be obtained by defining

  =ldul =>  logics  |=   [a=@s b=@]  (dul [a b])

  and computing

  .*  ldul  [0 2]
  """
  @spec dul_arm() :: Noun.t()
  def dul_arm() do
    layer_depth = example_layer_depth(8)

    "[8 [9 22 0 #{layer_depth}] 9 2 10 [6 7 [0 3] [0 12] 0 13] 0 2]"
    |> Noun.Format.parse_always()
  end

  @spec dul() :: Noun.t()
  def dul() do
    arm = dul_arm()
    sample = [888 | 999]
    core = [arm, sample | Nock.Lib.logics_core()]

    # dul(-1, --5) == 9
    assert Nock.nock(core, [9, 2, 10, [6, 1 | [1 | 10]], 0 | 1])
           |> elem(1)
           |> Noun.equal?(9)

    # dul(-11, -61) == 110
    assert Nock.nock(core, [9, 2, 10, [6, 1 | [21 | 121]], 0 | 1])
           |> elem(1)
           |> Noun.equal?(110)

    # dul(--5, 3) == 2
    assert Nock.nock(core, [9, 2, 10, [6, 1 | [10 | 3]], 0 | 1])
           |> elem(1)
           |> Noun.equal?(2)

    core
  end

  @doc """
  I represent the fra gate call as a 2-argument gate.

  Can be obtained by defining

  =lfra =>  logics  |=   [a=@s b=@s]  (fra [a b])

  and computing

  .*  lfra  [0 2]
  """
  @spec fra_arm() :: Noun.t()
  def fra_arm() do
    layer_depth = example_layer_depth(8)

    "[8 [9 190 0 #{layer_depth}] 9 2 10 [6 7 [0 3] [0 12] 0 13] 0 2]"
    |> Noun.Format.parse_always()
  end

  @spec fra() :: Noun.t()
  def fra() do
    arm = fra_arm()
    sample = [888 | 999]
    core = [arm, sample | Nock.Lib.logics_core()]

    # -1 / -1 == --1
    assert Nock.nock(core, [9, 2, 10, [6, 1 | [1 | 1]], 0 | 1])
           |> elem(1)
           |> Noun.equal?(2)

    # -11 / --2 == -5
    assert Nock.nock(core, [9, 2, 10, [6, 1 | [21 | 4]], 0 | 1])
           |> elem(1)
           |> Noun.equal?(9)

    # --0 / --1 == --0
    assert Nock.nock(core, [9, 2, 10, [6, 1 | [0 | 1]], 0 | 1])
           |> elem(1)
           |> Noun.equal?(0)

    # --5 / -2 == -2
    assert Nock.nock(core, [9, 2, 10, [6, 1 | [10 | 3]], 0 | 1])
           |> elem(1)
           |> Noun.equal?(3)

    core
  end

  @doc """
  I represent the new gate call as a 2-argument gate.

  Can be obtained by defining

  =lnew =>  logics  |=   [a=? b=@]  (new [a b])

  and computing

  .*  lnew  [0 2]
  """
  @spec new_arm() :: Noun.t()
  def new_arm() do
    layer_depth = example_layer_depth(8)

    "[8 [9 758 0 #{layer_depth}] 9 2 10 [6 7 [0 3] [0 12] 0 13] 0 2]"
    |> Noun.Format.parse_always()
  end

  @spec new() :: Noun.t()
  def new() do
    arm = new_arm()
    sample = [888 | 999]
    core = [arm, sample | Nock.Lib.logics_core()]

    # new(%.n, 2) == -2
    assert Nock.nock(core, [9, 2, 10, [6, 1 | [1 | 2]], 0 | 1])
           |> elem(1)
           |> Noun.equal?(3)

    # new(%.y, 2) == --2
    assert Nock.nock(core, [9, 2, 10, [6, 1 | [0 | 2]], 0 | 1])
           |> elem(1)
           |> Noun.equal?(4)

    core
  end

  @doc """
  I represent the old gate call as a 2-argument gate.

  Can be obtained by defining

  =lold =>  logics  |=   [a=@s]  (old a)

  and computing

  .*  lold  [0 2]
  """
  @spec old_arm() :: Noun.t()
  def old_arm() do
    layer_depth = example_layer_depth(8)

    "[8 [9 756 0 #{layer_depth}] 9 2 10 [6 0 14] 0 2]"
    |> Noun.Format.parse_always()
  end

  @spec old() :: Noun.t()
  def old() do
    arm = old_arm()
    sample = 888
    core = [arm, sample | Nock.Lib.logics_core()]

    # old(-2) == [%.n, 2]
    assert Nock.nock(core, [9, 2, 10, [6, 1 | 3], 0 | 1])
           |> elem(1)
           |> Noun.equal?([1 | 2])

    # old(--2) == [%.y, 2]
    assert Nock.nock(core, [9, 2, 10, [6, 1 | 4], 0 | 1])
           |> elem(1)
           |> Noun.equal?([0 | 2])

    core
  end

  @doc """
  I represent the pro gate call as a 2-argument gate.

  Can be obtained by defining

  =lpro =>  logics  |=   [a=@s b=@s]  (pro [a b])

  and computing

  .*  lpro  [0 2]
  """
  @spec pro_arm() :: Noun.t()
  def pro_arm() do
    layer_depth = example_layer_depth(8)

    "[8 [9 46 0 #{layer_depth}] 9 2 10 [6 7 [0 3] [0 12] 0 13] 0 2]"
    |> Noun.Format.parse_always()
  end

  @spec pro() :: Noun.t()
  def pro() do
    arm = pro_arm()
    sample = [888 | 999]
    core = [arm, sample | Nock.Lib.logics_core()]

    # -3 * --3 == -9
    assert Nock.nock(core, [9, 2, 10, [6, 1 | [5 | 6]], 0 | 1])
           |> elem(1)
           |> Noun.equal?(17)

    # -3 * -3 == --9
    assert Nock.nock(core, [9, 2, 10, [6, 1 | [5 | 5]], 0 | 1])
           |> elem(1)
           |> Noun.equal?(18)

    core
  end

  @doc """
  I represent the rem gate call as a 2-argument gate.

  Can be obtained by defining

  =lrem =>  logics  |=   [a=@s b=@s]  (rem [a b])

  and computing

  .*  lrem  [0 2]
  """
  @spec rem_arm() :: Noun.t()
  def rem_arm() do
    layer_depth = example_layer_depth(8)

    "[8 [9 6.058 0 #{layer_depth}] 9 2 10 [6 7 [0 3] [0 12] 0 13] 0 2]"
    |> Noun.Format.parse_always()
  end

  @spec rem() :: Noun.t()
  def rem() do
    arm = rem_arm()
    sample = [888 | 999]
    core = [arm, sample | Nock.Lib.logics_core()]

    # -17 % -3 == -2
    assert Nock.nock(core, [9, 2, 10, [6, 1 | [33 | 5]], 0 | 1])
           |> elem(1)
           |> Noun.equal?(3)

    # --17 % -3 == --2
    assert Nock.nock(core, [9, 2, 10, [6, 1 | [34 | 5]], 0 | 1])
           |> elem(1)
           |> Noun.equal?(4)

    # -17 % --3 == -2
    assert Nock.nock(core, [9, 2, 10, [6, 1 | [33 | 6]], 0 | 1])
           |> elem(1)
           |> Noun.equal?(3)

    # --17 % --3 == --2
    assert Nock.nock(core, [9, 2, 10, [6, 1 | [34 | 6]], 0 | 1])
           |> elem(1)
           |> Noun.equal?(4)

    core
  end

  @doc """
  I represent the sum gate call as a 2-argument gate.

  Can be obtained by defining

  =lsum =>  logics  |=   [a=@s b=@s]  (sum [a b])

  and computing

  .*  lsum  [0 2]
  """
  @spec sum_arm() :: Noun.t()
  def sum_arm() do
    layer_depth = example_layer_depth(8)

    "[8 [9 4 0 #{layer_depth}] 9 2 10 [6 7 [0 3] [0 12] 0 13] 0 2]"
    |> Noun.Format.parse_always()
  end

  @spec sum() :: Noun.t()
  def sum() do
    arm = sum_arm()
    sample = [888 | 999]
    core = [arm, sample | Nock.Lib.logics_core()]

    # -11 + --2 == -9
    assert Nock.nock(core, [9, 2, 10, [6, 1 | [21 | 4]], 0 | 1])
           |> elem(1)
           |> Noun.equal?(17)

    # --2 % --2 == --4
    assert Nock.nock(core, [9, 2, 10, [6, 1 | [4 | 4]], 0 | 1])
           |> elem(1)
           |> Noun.equal?(8)

    core
  end

  @spec sun_arm() :: Noun.t()
  def sun_arm() do
    layer_depth = example_layer_depth(8)

    "[8 [9 10 0 #{layer_depth}] 9 2 10 [6 0 14] 0 2]"
    |> Noun.Format.parse_always()
  end

  @spec sun() :: Noun.t()
  def sun() do
    arm = sun_arm()
    sample = 888
    core = [arm, sample | Nock.Lib.logics_core()]

    # sun(90) == 180
    assert Nock.nock(core, [9, 2, 10, [6, 1 | 90], 0 | 1])
           |> elem(1)
           |> Noun.equal?(180)

    core
  end

  @spec syn_arm() :: Noun.t()
  def syn_arm() do
    layer_depth = example_layer_depth(8)

    "[8 [9 188 0 #{layer_depth}] 9 2 10 [6 0 14] 0 2]"
    |> Noun.Format.parse_always()
  end

  @spec syn() :: Noun.t()
  def syn() do
    arm = syn_arm()
    sample = 888
    core = [arm, sample | Nock.Lib.logics_core()]

    # syn(--0) == %.y
    assert Nock.nock(core, [9, 2, 10, [6, 1 | 0], 0 | 1])
           |> elem(1)
           |> Noun.equal?(0)

    # syn(-2) == %.n
    assert Nock.nock(core, [9, 2, 10, [6, 1 | 3], 0 | 1])
           |> elem(1)
           |> Noun.equal?(1)

    # syn(--2) == %.y
    assert Nock.nock(core, [9, 2, 10, [6, 1 | 4], 0 | 1])
           |> elem(1)
           |> Noun.equal?(0)

    core
  end

  @doc """
  I represent the cmp gate call as a 2-argument gate.

  Can be obtained by defining

  =lcmp =>  logics  |=   [a=@s b=@s]  (cmp [a b])

  and computing

  .*  lcmp  [0 2]
  """
  @spec cmp_arm() :: Noun.t()
  def cmp_arm() do
    layer_depth = example_layer_depth(8)

    "[8 [9 191 0 #{layer_depth}] 9 2 10 [6 7 [0 3] [0 12] 0 13] 0 2]"
    |> Noun.Format.parse_always()
  end

  @spec cmp() :: Noun.t()
  def cmp() do
    arm = cmp_arm()
    sample = [888 | 999]
    core = [arm, sample | Nock.Lib.logics_core()]

    # cmp(-2, --1) == -1
    assert Nock.nock(core, [9, 2, 10, [6, 1 | [3 | 2]], 0 | 1])
           |> elem(1)
           |> Noun.equal?(1)

    # cmp(--2, --1) == --1
    assert Nock.nock(core, [9, 2, 10, [6, 1 | [4 | 2]], 0 | 1])
           |> elem(1)
           |> Noun.equal?(2)

    # cmp(--2, --2) == --0
    assert Nock.nock(core, [9, 2, 10, [6, 1 | [4 | 4]], 0 | 1])
           |> elem(1)
           |> Noun.equal?(0)

    # cmp(--2, --5) == -1
    assert Nock.nock(core, [9, 2, 10, [6, 1 | [4 | 10]], 0 | 1])
           |> elem(1)
           |> Noun.equal?(1)

    core
  end

  @doc """
  I represent the mug gate call.

  Can be obtained by defining

  =lmug =>  logics  |=   a=*  (mug a)

  and computing

  .*  lmug  [0 2]
  """
  @spec mug_arm() :: Noun.t()
  def mug_arm() do
    layer_depth = example_layer_depth(9)

    "[8 [9 189 0 #{layer_depth}] 9 2 10 [6 0 14] 0 2]"
    |> Noun.Format.parse_always()
  end

  @doc """
  I am the full mug gate with specified sample and logics context.
  """
  @spec mug_call(Noun.t()) :: Noun.t()
  def mug_call(noun) do
    sample = noun
    [mug_arm(), sample | Nock.Lib.logics_core()]
  end

  @spec mug_test() :: bool()
  def mug_test() do
    assert 10000
           |> mug_call()
           |> Nock.nock([9, 2, 0 | 1])
           |> elem(1)
           |> Noun.equal?(795_713_195)

    assert 10001
           |> mug_call()
           |> Nock.nock([9, 2, 0 | 1])
           |> elem(1)
           |> Noun.equal?(420_521_697)

    assert 1
           |> mug_call()
           |> Nock.nock([9, 2, 0 | 1])
           |> elem(1)
           |> Noun.equal?(1_901_865_568)

    assert [1, 2, 3, 4, 5 | 0]
           |> mug_call()
           |> Nock.nock([9, 2, 0 | 1])
           |> elem(1)
           |> Noun.equal?(1_565_443_491)
  end

  @doc """
  I represent the dor gate call.

  Can be obtained by defining

  =ldor =>  logics  |=   [a=* b=*]  (dor a b)

  and computing

  .*  ldor  [0 2]
  """
  @spec dor_arm() :: Noun.t()
  def dor_arm() do
    layer_depth = example_layer_depth(9)

    "[8 [9 765 0 #{layer_depth}] 9 2 10 [6 [0 28] 0 29] 0 2]"
    |> Noun.Format.parse_always()
  end

  @doc """
  I am the full dor gate with specified sample and logics context.
  """
  @spec dor_call(Noun.t(), Noun.t()) :: Noun.t()
  def dor_call(a, b) do
    sample = [a | b]
    [dor_arm(), sample | Nock.Lib.logics_core()]
  end

  @spec dor_test() :: bool()
  def dor_test() do
    assert dor_call(1, 2)
           |> Nock.nock([9, 2, 0 | 1])
           |> elem(1)
           |> Noun.equal?(0)

    assert dor_call(2, 1)
           |> Nock.nock([9, 2, 0 | 1])
           |> elem(1)
           |> Noun.equal?(1)

    assert dor_call([1, 2, 3], [1, 2, 4])
           |> Nock.nock([9, 2, 0 | 1])
           |> elem(1)
           |> Noun.equal?(0)

    assert dor_call([1, 2, 4], [1, 2, 3])
           |> Nock.nock([9, 2, 0 | 1])
           |> elem(1)
           |> Noun.equal?(1)
  end

  @doc """
  I represent the gor gate call.

  Can be obtained by defining

  =lgor =>  logics  |=   [a=* b=*]  (gor a b)

  and computing

  .*  lgor  [0 2]
  """
  @spec gor_arm() :: Noun.t()
  def gor_arm() do
    layer_depth = example_layer_depth(9)

    "[8 [9 190 0 #{layer_depth}] 9 2 10 [6 [0 28] 0 29] 0 2]"
    |> Noun.Format.parse_always()
  end

  @doc """
  I am the full gor gate with specified sample and logics context.
  """
  @spec gor_call(Noun.t(), Noun.t()) :: Noun.t()
  def gor_call(a, b) do
    sample = [a | b]
    [gor_arm(), sample | Nock.Lib.logics_core()]
  end

  @spec gor_test() :: bool()
  def gor_test() do
    assert gor_call(100, 99)
           |> Nock.nock([9, 2, 0 | 1])
           |> elem(1)
           |> Noun.equal?(0)

    assert gor_call(99, 100)
           |> Nock.nock([9, 2, 0 | 1])
           |> elem(1)
           |> Noun.equal?(1)

    assert gor_call("foo", "bar")
           |> Nock.nock([9, 2, 0 | 1])
           |> elem(1)
           |> Noun.equal?(1)
  end

  @doc """
  I represent the mor gate call.

  Can be obtained by defining

  =lmor =>  logics  |=   [a=* b=*]  (mor a b)

  and computing

  .*  lmor  [0 2]
  """
  @spec mor_arm() :: Noun.t()
  def mor_arm() do
    layer_depth = example_layer_depth(9)

    "[8 [9 10 0 #{layer_depth}] 9 2 10 [6 [0 28] 0 29] 0 2]"
    |> Noun.Format.parse_always()
  end

  @doc """
  I am the full mor gate with specified sample and logics context.
  """
  @spec mor_call(Noun.t(), Noun.t()) :: Noun.t()
  def mor_call(a, b) do
    sample = [a | b]
    [mor_arm(), sample | Nock.Lib.logics_core()]
  end

  @spec mor_test() :: bool()
  def mor_test() do
    assert mor_call("g", "f")
           |> Nock.nock([9, 2, 0 | 1])
           |> elem(1)
           |> Noun.equal?(0)

    assert mor_call("a", "z")
           |> Nock.nock([9, 2, 0 | 1])
           |> elem(1)
           |> Noun.equal?(1)

    assert mor_call(43326, 41106)
           |> Nock.nock([9, 2, 0 | 1])
           |> elem(1)
           |> Noun.equal?(1)
  end

  @doc """
  I represent the lte gate call as a 2-argument gate.

  Can be obtained by defining

  =llte =>  logics  |=   [a=@s b=@s]  (lte [a b])

  and computing

  .*  llte  [0 2]
  """
  @spec lte_arm() :: Noun.t()
  def lte_arm() do
    layer_depth = example_layer_depth(1)

    "[8 [9 84 0 #{layer_depth}] 9 2 10 [6 7 [0 3] [0 12] 0 13] 0 2]"
    |> Noun.Format.parse_always()
  end

  @spec lte() :: Noun.t()
  def lte() do
    sample = [888 | 999]
    core = [lte_arm(), sample | Nock.Lib.logics_core()]

    max_test_val = 4

    for i <- 0..max_test_val, j <- 0..max_test_val do
      expected = if i <= j, do: 0, else: 1

      assert Nock.nock(core, [9, 2, 10, [6, 1 | [i | j]], 0 | 1])
             |> elem(1)
             |> Noun.equal?(expected)
    end

    core
  end

  @doc """
  I represent the mor gate call.

  Can be obtained by defining

  =lsilt =>  logics  |=   a=(list)  (silt a)

  and computing

  .*  lsilt  [0 2]
  """
  @spec silt_arm() :: Noun.t()
  def silt_arm() do
    layer_depth = example_layer_depth(10)

    "[8 [9 22 0 #{layer_depth}] 9 2 10 [6 0 14] 0 2]"
    |> Noun.Format.parse_always()
  end

  def silt_call(list) do
    sample = list
    [silt_arm(), sample | Nock.Lib.logics_core()]
  end

  def silt_test() do
    list = [1, 2, 33, 2]

    {:ok, set} =
      list
      |> Noun.Nounable.List.to_noun()
      |> silt_call
      |> Nock.nock([9, 2, 0 | 1])

    assert list
           |> MapSet.new()
           |> Noun.Nounable.to_noun()
           |> Noun.equal?(set)
  end

  @doc """
  The gate representing an in core creation with a specified set.

  Can be gotten by defining

  =l   =>  logics  |=  a=(set)  ~(. in a)

  and getting it's arm with [0 2]
  """
  @spec in_arm() :: Noun.t()
  def in_arm() do
    layer_depth = example_layer_depth(10)

    arm =
      "[8 [9 21 0 #{layer_depth}] 10 [6 0 14] 0 2]"
      |> Noun.Format.parse_always()

    sample = 0
    [arm, sample | Nock.Lib.logics_core()]
  end

  @spec in_call(Noun.t()) :: :error | {:ok, Noun.t()}
  def in_call(set) do
    Nock.nock(in_arm(), [9, 2, 10, [6, 1 | set], 0 | 1])
  end

  @doc """
  I represent a put gate with a specified instantiated in core given
  as an extra argument.

  Can be gotten by defining locally

  =l    =>  logics  |=  [a=_in b=*]  (put:in b)

  and grabbing the arm with [0 2]
  """
  @spec put_with_core() :: Noun.t()
  def put_with_core() do
    arm =
      "[8 [7 [0 12] 9 84 0 1] 9 2 10 [6 0 29] 0 2]"
      |> Noun.Format.parse_always()

    sample = [0, 0]

    [arm, sample | Nock.Lib.logics_core()]
  end

  @spec put_with_core_call(Noun.t(), Noun.t()) ::
          :error | {:ok, Noun.t()}
  def put_with_core_call(core, elem) do
    Nock.nock(put_with_core(), [9, 2, 10, [6, 1 | [core | elem]], 0 | 1])
  end

  def put_test() do
    set_elixir = [[1 | 2], [2 | 4], [2 | 3]] |> MapSet.new()
    noun_set = set_elixir |> Noun.Nounable.to_noun()

    elem1 = [2 | 4]
    elem2 = [1 | 5]

    noun_set_new =
      set_elixir |> MapSet.put(elem2) |> Noun.Nounable.to_noun()

    {:ok, in_core} = noun_set |> in_call()

    assert in_core
           |> put_with_core_call(elem1)
           |> elem(1)
           |> Noun.equal?(noun_set)

    assert in_core
           |> put_with_core_call(elem2)
           |> elem(1)
           |> Noun.equal?(noun_set_new)
  end

  @doc """
  I represent a wyt gate with a specified instantiated in core given
  as an extra argument.

  Can be gotten by defining locally

  =l    =>  logics  |=  a=_in  wyt:a

  and grabbing the arm with [0 2]
  """
  @spec wyt_with_core() :: Noun.t()
  def wyt_with_core() do
    arm =
      "[7 [0 6] 9 92 0 1]"
      |> Noun.Format.parse_always()

    sample = 0

    [arm, sample | Nock.Lib.logics_core()]
  end

  @spec wyt_with_core_call(Noun.t()) ::
          :error | {:ok, Noun.t()}
  def wyt_with_core_call(core) do
    Nock.nock(wyt_with_core(), [9, 2, 10, [6, 1 | core], 0 | 1])
  end

  def wyt_test() do
    set = [1, 2, 3, 4] |> MapSet.new() |> Noun.Nounable.to_noun()
    {:ok, in_core} = in_call(set)

    assert in_core |> wyt_with_core_call() |> elem(1) |> Noun.equal?(4)
  end

  @doc """
  I represent a tap:in call with a specified instantiated in core given
  as an extra argument.

  Can be gotten by defining locally

  =l    =>  logics  |=  a=_in  tap:a

  and grabbing the arm with [0 2]
  """
  @spec tap_in_with_core() :: Noun.t()
  def tap_in_with_core() do
    arm =
      "[7 [0 6] 9 186 0 1]"
      |> Noun.Format.parse_always()

    sample = 0

    [arm, sample | Nock.Lib.logics_core()]
  end

  @spec tap_in_with_core_call(Noun.t()) ::
          :error | {:ok, Noun.t()}
  def tap_in_with_core_call(core) do
    Nock.nock(tap_in_with_core(), [9, 2, 10, [6, 1 | core], 0 | 1])
  end

  def tap_in_test() do
    set = [123, 0] |> MapSet.new() |> Noun.Nounable.MapSet.to_noun()
    {:ok, in_core} = in_call(set)
    {:ok, res} = in_core |> tap_in_with_core_call()

    # this is due to how 0 and 123 get located in set
    # test it like this since this is deterministic
    assert res |> Noun.equal?([0, 123])
  end

  @doc """
  I represent an int gate with a specified instantiated in core given
  as an extra argument.

  Can be gotten by defining locally

  =l    =>  logics  |=  [a=_in b=(set)]  (int:a b)

  and grabbing the arm with [0 2]
  """
  @spec int_with_core() :: Noun.t()
  def int_with_core() do
    arm =
      "[8 [7 [0 12] 9 85 0 1] 9 2 10 [6 0 29] 0 2]"
      |> Noun.Format.parse_always()

    sample = [0 | 0]

    [arm, sample | Nock.Lib.logics_core()]
  end

  @spec int_with_core_call(Noun.t(), Noun.t()) ::
          :error | {:ok, Noun.t()}
  def int_with_core_call(core, set) do
    Nock.nock(int_with_core(), [9, 2, 10, [6, 1 | [core | set]], 0 | 1])
  end

  def int_test() do
    set1 = [1, 2, 3, 4] |> MapSet.new() |> Noun.Nounable.to_noun()
    set2 = [3, 4, 5, 6] |> MapSet.new() |> Noun.Nounable.to_noun()
    {:ok, in_core} = in_call(set1)

    set_res =
      MapSet.new([1, 2, 3, 4])
      |> MapSet.intersection(MapSet.new([3, 4, 5, 6]))
      |> Noun.Nounable.to_noun()

    assert in_core
           |> int_with_core_call(set2)
           |> elem(1)
           |> Noun.equal?(set_res)
  end

  @doc """
  I represent a dif gate with a specified instantiated in core given
  as an extra argument.

  Can be gotten by defining locally

  =l    =>  logics  |=  [a=_in b=(set)]  (dif:a b)

  and grabbing the arm with [0 2]
  """
  @spec dif_with_core() :: Noun.t()
  def dif_with_core() do
    arm =
      "[8 [7 [0 12] 9 175 0 1] 9 2 10 [6 0 29] 0 2]"
      |> Noun.Format.parse_always()

    sample = [0 | 0]

    [arm, sample | Nock.Lib.logics_core()]
  end

  @spec dif_with_core_call(Noun.t(), Noun.t()) ::
          :error | {:ok, Noun.t()}
  def dif_with_core_call(core, set) do
    Nock.nock(dif_with_core(), [9, 2, 10, [6, 1 | [core | set]], 0 | 1])
  end

  def dif_test() do
    set1 = [1, 2, 3, 4] |> MapSet.new() |> Noun.Nounable.to_noun()
    set2 = [3, 4, 5, 6] |> MapSet.new() |> Noun.Nounable.to_noun()
    {:ok, in_core} = in_call(set1)

    set_res =
      MapSet.new([1, 2, 3, 4])
      |> MapSet.difference(MapSet.new([3, 4, 5, 6]))
      |> Noun.Nounable.to_noun()

    assert in_core
           |> dif_with_core_call(set2)
           |> elem(1)
           |> Noun.equal?(set_res)
  end

  @doc """
  I represent a has gate with a specified instantiated in core given
  as an extra argument.

  Can be gotten by defining locally

  =l    =>  logics  |=  [a=_in b=(set)]  (has:a b)

  and grabbing the arm with [0 2]
  """
  @spec has_with_core() :: Noun.t()
  def has_with_core() do
    arm =
      "[8 [7 [0 12] 9 762 0 1] 9 2 10 [6 0 29] 0 2]"
      |> Noun.Format.parse_always()

    sample = [0 | 0]

    [arm, sample | Nock.Lib.logics_core()]
  end

  @spec has_with_core_call(Noun.t(), Noun.t()) ::
          :error | {:ok, Noun.t()}
  def has_with_core_call(core, elem) do
    Nock.nock(has_with_core(), [9, 2, 10, [6, 1 | [core | elem]], 0 | 1])
  end

  def has_test() do
    set = [1, 2, 3, 4] |> MapSet.new() |> Noun.Nounable.to_noun()
    elem1 = 1
    elem2 = 5
    {:ok, in_core} = in_call(set)

    assert in_core |> has_with_core_call(elem1) |> elem(1) |> Noun.equal?(0)
    assert in_core |> has_with_core_call(elem2) |> elem(1) |> Noun.equal?(1)
  end

  @doc """
  I represent a uni gate with a specified instantiated in core given
  as an extra argument.

  Can be gotten by defining locally

  =l    =>  logics  |=  [a=_in b=(set)]  (uni:a b)

  and grabbing the arm with [0 2]
  """
  @spec uni_with_core() :: Noun.t()
  def uni_with_core() do
    arm =
      "[8 [7 [0 12] 9 174 0 1] 9 2 10 [6 0 29] 0 2]"
      |> Noun.Format.parse_always()

    sample = [0 | 0]

    [arm, sample | Nock.Lib.logics_core()]
  end

  @spec uni_with_core_call(Noun.t(), Noun.t()) ::
          :error | {:ok, Noun.t()}
  def uni_with_core_call(core, set) do
    Nock.nock(uni_with_core(), [9, 2, 10, [6, 1 | [core | set]], 0 | 1])
  end

  def uni_test() do
    set1 = [1, 2, 3, 4] |> MapSet.new() |> Noun.Nounable.to_noun()
    set2 = [3, 4, 5, 6] |> MapSet.new() |> Noun.Nounable.to_noun()
    {:ok, in_core} = in_call(set1)

    set_res =
      MapSet.new([1, 2, 3, 4])
      |> MapSet.union(MapSet.new([3, 4, 5, 6]))
      |> Noun.Nounable.to_noun()

    assert in_core
           |> uni_with_core_call(set2)
           |> elem(1)
           |> Noun.equal?(set_res)
  end

  @doc """
  I represent a duni gate with a specified instantiated in core given
  as an extra argument.

  Can be gotten by defining locally

  =l    =>  logics  |=  [a=_in b=(set)]  (duni:a b)

  and grabbing the arm with [0 2]
  """
  @spec duni_with_core() :: Noun.t()
  def duni_with_core() do
    arm =
      "[8 [7 [0 12] 9 763 0 1] 9 2 10 [6 0 29] 0 2]"
      |> Noun.Format.parse_always()

    sample = [0 | 0]

    [arm, sample | Nock.Lib.logics_core()]
  end

  @spec duni_with_core_call(Noun.t(), Noun.t()) ::
          :error | {:ok, Noun.t()}
  def duni_with_core_call(core, set) do
    Nock.nock(duni_with_core(), [9, 2, 10, [6, 1 | [core | set]], 0 | 1])
  end

  def duni_test() do
    set1 = [1, 2, 3, 4] |> MapSet.new() |> Noun.Nounable.to_noun()
    set2 = [3, 4, 5, 6] |> MapSet.new() |> Noun.Nounable.to_noun()
    set3 = [5, 6] |> MapSet.new() |> Noun.Nounable.to_noun()
    {:ok, in_core} = in_call(set1)

    set_res =
      MapSet.new([1, 2, 3, 4])
      |> MapSet.union(MapSet.new([5, 6]))
      |> Noun.Nounable.to_noun()

    assert in_core |> duni_with_core_call(set2) == :error

    assert in_core
           |> duni_with_core_call(set3)
           |> elem(1)
           |> Noun.equal?(set_res)
  end

  @doc """
  The gate representing a by core creation with a specified set.

  Can be gotten by defining

  =l   =>  logics  |=  a=(set)  ~(. by a)

  and getting it's arm with [0 2]
  """
  @spec by_arm() :: Noun.t()
  def by_arm() do
    layer_depth = example_layer_depth(11)

    arm =
      "[8 [9 93 0 #{layer_depth}] 10 [6 0 14] 0 2]"
      |> Noun.Format.parse_always()

    sample = 0
    [arm, sample | Nock.Lib.logics_core()]
  end

  @spec by_call(Noun.t()) :: :error | {:ok, Noun.t()}
  def by_call(set) do
    Nock.nock(by_arm(), [9, 2, 10, [6, 1 | set], 0 | 1])
  end

  @doc """
  I represent a put gate with a specified instantiated in core given
  as an extra argument.

  Can be gotten by defining locally

  =l    =>  logics  |=  [a=_by b=(pair)]  (put:a b)

  and grabbing the arm with [0 2]
  """
  @spec mput_with_core() :: Noun.t()
  def mput_with_core() do
    arm =
      "[8 [7 [0 12] 9 340 0 1] 9 2 10 [6 0 29] 0 2]"
      |> Noun.Format.parse_always()

    sample = [0 | 0]

    [arm, sample | Nock.Lib.logics_core()]
  end

  @spec mput_with_core_call(Noun.t(), Noun.t(), Noun.t()) ::
          :error | {:ok, Noun.t()}
  def mput_with_core_call(core, key, val) do
    Nock.nock(mput_with_core(), [9, 2, 10, [6, 1 | [core, key | val]], 0 | 1])
  end

  def mput_test() do
    map = %{"a" => 1} |> Noun.Nounable.Map.to_noun()
    {:ok, by_core} = by_call(map)

    map_res =
      %{"a" => 1} |> Map.put("a", 3) |> Noun.Nounable.Map.to_noun()

    assert by_core
           |> mput_with_core_call("a", 3)
           |> elem(1)
           |> Noun.equal?(map_res)
  end

  @doc """
  I represent a got gate with a specified instantiated in core given
  as an extra argument.

  Can be gotten by defining locally

  =l    =>  logics  |=  [a=_by b=*]  (got:a b)

  and grabbing the arm with [0 2]
  """
  @spec got_with_core() :: Noun.t()
  def got_with_core() do
    arm =
      "[8 [7 [0 12] 9 701 0 1] 9 2 10 [6 0 29] 0 2]"
      |> Noun.Format.parse_always()

    sample = [0 | 0]

    [arm, sample | Nock.Lib.logics_core()]
  end

  @spec got_with_core_call(Noun.t(), Noun.t()) ::
          :error | {:ok, Noun.t()}
  def got_with_core_call(core, key) do
    Nock.nock(got_with_core(), [9, 2, 10, [6, 1 | [core | key]], 0 | 1])
  end

  def got_test() do
    map = %{"a" => 1} |> Noun.Nounable.Map.to_noun()
    {:ok, by_core} = by_call(map)

    assert by_core
           |> got_with_core_call("a")
           |> elem(1)
           |> Noun.equal?(1)
  end

  @doc """
  I represent a tap:by call with a specified instantiated in core given
  as an extra argument.

  Can be gotten by defining locally

  =l    =>  logics  |=  a=_by  tap:a

  and grabbing the arm with [0 2]
  """
  @spec tap_by_with_core() :: Noun.t()
  def tap_by_with_core() do
    arm =
      "[7 [0 6] 9 174 0 1]"
      |> Noun.Format.parse_always()

    sample = 0

    [arm, sample | Nock.Lib.logics_core()]
  end

  @spec tap_by_with_core_call(Noun.t()) ::
          :error | {:ok, Noun.t()}
  def tap_by_with_core_call(core) do
    Nock.nock(tap_by_with_core(), [9, 2, 10, [6, 1 | core], 0 | 1])
  end

  def tap_by_test() do
    set = %{123 => "blah"} |> Map.new() |> Noun.Nounable.to_noun()
    {:ok, by_core} = by_call(set)
    {:ok, res} = by_core |> tap_by_with_core_call()

    # this is due to how 0 and 123 get located in set
    # test it like this since this is deterministic
    assert res |> Noun.equal?([[123 | "blah"]])
  end

  def kind_arm() do
    layer_depth = Nock.Lib.stdlib_layers() |> example_layer_depth()

    "[8 [9 1492 0 #{layer_depth}] 9 2 10 [6 0 14] 0 2]"
    |> Noun.Format.parse_always()
  end

  def kind_call(resource) do
    sample = resource
    [kind_arm(), sample | Nock.Lib.logics_core()]
  end

  def kind_test() do
    resource = Examples.ETransparent.EResource.trivial_true_resource()

    {:ok, kind} =
      resource
      |> Noun.Nounable.to_noun()
      |> kind_call
      |> Nock.nock([9, 2, 0 | 1])

    assert resource
           |> Anoma.RM.Transparent.Resource.kind()
           |> Noun.equal?(kind)
  end

  def delta_add_arm() do
    layer_depth = Nock.Lib.stdlib_layers() |> example_layer_depth()

    "[8 [9 92 0 #{layer_depth}] 9 2 10 [6 7 [0 3] [0 12] 0 13] 0 2]"
    |> Noun.Format.parse_always()
  end

  def delta_add_call(delta1, delta2) do
    sample = [delta1 | delta2]
    [delta_add_arm(), sample | Nock.Lib.logics_core()]
  end

  def delta_add_test() do
    delta = EAction.trivial_true_commit_delta()

    {:ok, delta} =
      delta_add_call(delta, delta) |> Nock.nock([9, 2, 0 | 1])

    delta_original = EAction.trivial_true_commit_delta()
    assert delta == DeltaHash.delta_add(delta_original, delta_original)
  end

  def delta_sub_arm() do
    layer_depth = Nock.Lib.stdlib_layers() |> example_layer_depth()

    "[8 [9 1527 0 #{layer_depth}] 9 2 10 [6 7 [0 3] [0 12] 0 13] 0 2]"
    |> Noun.Format.parse_always()
  end

  def delta_sub_call(delta1, delta2) do
    sample = [delta1 | delta2]
    [delta_sub_arm(), sample | Nock.Lib.logics_core()]
  end

  def delta_sub_test() do
    delta = EAction.trivial_true_commit_delta()

    assert delta_sub_call(delta, delta)
           |> Nock.nock([9, 2, 0 | 1])
           |> elem(1)
           |> Noun.equal?(2)
  end

  def action_delta_arm() do
    layer_depth = Nock.Lib.stdlib_layers() |> example_layer_depth()

    "[8 [9 4 0 #{layer_depth}] 9 2 10 [6 0 14] 0 2]"
    |> Noun.Format.parse_always()
  end

  def action_delta_call(action) do
    sample = action
    [action_delta_arm(), sample | Nock.Lib.logics_core()]
  end

  def action_delta_test() do
    action = EAction.trivial_true_commit_action() |> Noun.Nounable.to_noun()

    {:ok, delta} =
      action |> action_delta_call() |> Nock.nock([9, 2, 0 | 1])

    delta_original = EAction.trivial_true_commit_action() |> Action.delta()

    assert delta == delta_original
  end

  def make_delta_arm() do
    layer_depth = Nock.Lib.stdlib_layers() |> example_layer_depth()

    "[8 [9 1494 0 #{layer_depth}] 9 2 10 [6 0 14] 0 2]"
    |> Noun.Format.parse_always()
  end

  def make_delta_call(actions) do
    sample = actions
    [make_delta_arm(), sample | Nock.Lib.logics_core()]
  end

  def make_delta_test() do
    actions =
      MapSet.new([EAction.trivial_true_commit_action()])
      |> Noun.Nounable.to_noun()

    {:ok, delta} =
      actions |> make_delta_call() |> Nock.nock([9, 2, 0 | 1])

    assert delta == EAction.trivial_true_commit_action() |> Action.delta()
  end

  def is_commitment_arm() do
    layer_depth = Nock.Lib.stdlib_layers() |> example_layer_depth()

    "[8 [9 6.102 0 #{layer_depth}] 9 2 10 [6 0 14] 0 2]"
    |> Noun.Format.parse_always()
  end

  def make_is_commitment_call(atom) do
    sample = atom
    [is_commitment_arm(), sample | Nock.Lib.logics_core()]
  end

  def is_commitment_test() do
    atom_true = "CM_whatever"
    atom_false = "NF_whatever"
    atom_weird_still_false = "a"

    {:ok, res1} =
      atom_true |> make_is_commitment_call() |> Nock.nock([9, 2, 0 | 1])

    {:ok, res2} =
      atom_false |> make_is_commitment_call() |> Nock.nock([9, 2, 0 | 1])

    {:ok, res3} =
      atom_weird_still_false
      |> make_is_commitment_call()
      |> Nock.nock([9, 2, 0 | 1])

    assert Noun.equal?(res1, 0)
    assert Noun.equal?(res2, 1)
    assert Noun.equal?(res3, 1)
  end

  def is_nullifier_arm() do
    layer_depth = Nock.Lib.stdlib_layers() |> example_layer_depth()

    "[8 [9 372 0 #{layer_depth}] 9 2 10 [6 0 14] 0 2]"
    |> Noun.Format.parse_always()
  end

  def make_is_nullifier_call(atom) do
    sample = atom
    [is_nullifier_arm(), sample | Nock.Lib.logics_core()]
  end

  def is_nullifier_test() do
    atom_true = "NF_whatever"
    atom_false = "CM_whatever"
    atom_weird_still_false = "a"

    {:ok, res1} =
      atom_true |> make_is_nullifier_call() |> Nock.nock([9, 2, 0 | 1])

    {:ok, res2} =
      atom_false |> make_is_nullifier_call() |> Nock.nock([9, 2, 0 | 1])

    {:ok, res3} =
      atom_weird_still_false
      |> make_is_commitment_call()
      |> Nock.nock([9, 2, 0 | 1])

    assert Noun.equal?(res1, 0)
    assert Noun.equal?(res2, 1)
    assert Noun.equal?(res3, 1)
  end

  ############################################################
  ##                      Block Cores                       ##
  ############################################################

  @doc """
  I am an lash arm in the block door.

  My index inside the door can be seen by asking to dump the logic of
  =llsh   =>  logics  |=  a=@  lsh:block
  """

  @spec lsh(Noun.t()) :: Noun.t()
  def lsh(value) do
    block_calling_biop(value, 90)
  end

  @doc """
  I am an lash arm in the block door.

  My index inside the door can be seen by asking to dump the logic of
  =lmet   =>  logics  |=  a=@  met:block
  """

  @spec met(Noun.t()) :: Noun.t()
  def met(value) do
    block_calling_mono(value, 190)
  end

  @doc """
  I am an lash arm in the block door.

  My index inside the door can be seen by asking to dump the logic of
  =luend   =>  logics  |=  a=@  luend:block
  """

  @spec uend(Noun.t()) :: Noun.t()
  def uend(value) do
    block_calling_biop(value, 367)
  end

  @doc """
  I am an lash arm in the block door.

  My index inside the door can be seen by asking to dump the logic of
  =rsh   =>  logics  |=  a=@  rsh:block
  """

  @spec rsh(Noun.t()) :: Noun.t()
  def rsh(value) do
    block_calling_biop(value, 767)
  end

  @doc """
  I evaluate met at block size 0 and gate-input 28.

  met(0) evaluates the gate of the block door at block size 0,
  [6 1 28] replaces the sample with 28.
  """

  @spec met0() :: Noun.t()
  def met0() do
    met = met(0)

    assert Nock.nock(met, [9, 2, 10, [6, 1 | 28], 0 | 1])
           |> elem(1)
           |> Noun.equal?(5)

    assert Nock.nock(met, [9, 2, 10, [6, 1 | <<28>>], 0 | 1])
           |> elem(1)
           |> Noun.equal?(5)

    met
  end

  @doc """
  I evaluate met at block size 1 and gate-input 28.

  met(1) evaluates the gate of the block door at block size 1,
  [6 1 28] replaces the sample with 28.
  """

  @spec met1() :: Noun.t()
  def met1() do
    met = met(1)

    assert Nock.nock(met, [9, 2, 10, [6, 1 | 28], 0 | 1])
           |> elem(1)
           |> Noun.equal?(3)

    assert Nock.nock(met, [9, 2, 10, [6, 1 | <<28>>], 0 | 1])
           |> elem(1)
           |> Noun.equal?(3)

    met
  end

  @doc """
  I evaluate met at block size 2 and gate-input 28.

  met(2) evaluates the gate of the block door at block size 2,
  [6 1 28] replaces the sample with 28.
  """

  @spec met2() :: Noun.t()
  def met2() do
    met = met(2)

    assert Nock.nock(met, [9, 2, 10, [6, 1 | 28], 0 | 1])
           |> elem(1)
           |> Noun.equal?(2)

    assert Nock.nock(met, [9, 2, 10, [6, 1 | <<28>>], 0 | 1])
           |> elem(1)
           |> Noun.equal?(2)

    met
  end

  @doc """
  I evaluate uend at block size 0 and gate-input [5 80].

  uend(0) evaluates the gate of the block door at block size 0,
  [6 1 5 80] replaces the sample with [5 80].
  """

  @spec uend0() :: Noun.t()
  def uend0() do
    uend = uend(0)

    assert Nock.nock(uend, [9, 2, 10, [6, 1, 5 | 80], 0 | 1])
           |> elem(1)
           |> Noun.equal?(16)

    uend
  end

  @doc """
  I evaluate uend at block size 1 and gate-input [3 80] and [4 80].

  uend(1) evaluates the gate of the block door at block size 1,
  [6 1 3 80] replaces the sample with [3 80],
  [6 1 4 80] replaces the sample with [3 80]
  """

  @spec uend1() :: Noun.t()
  def uend1() do
    uend = uend(1)

    assert Nock.nock(uend, [9, 2, 10, [6, 1, 3 | 80], 0 | 1])
           |> elem(1)
           |> Noun.equal?(16)

    assert Nock.nock(uend, [9, 2, 10, [6, 1, 4 | 80], 0 | 1])
           |> elem(1)
           |> Noun.equal?(80)

    uend
  end

  @doc """
  I evaluate lsh at block size 0 and gate-input [2 6].

  lsh(0) evaluates the gate of the block door at block size 0,
  [6 1 2 6] replaces the sample with [2 6].
  """

  @spec lsh0() :: Noun.t()
  def lsh0() do
    lsh = lsh(0)

    assert Nock.nock(lsh, [9, 2, 10, [6, 1, 2 | 6], 0 | 1])
           |> elem(1)
           |> Noun.equal?(24)

    lsh
  end

  @doc """
  I evaluate lsh at block size 1 and gate-input [2 6].

  lsh(1) evaluates the gate of the block door at block size 1,
  [6 1 2 6] replaces the sample with [2 6].
  """

  @spec lsh1() :: Noun.t()
  def lsh1() do
    lsh = lsh(1)

    assert Nock.nock(lsh, [9, 2, 10, [6, 1, 2 | 6], 0 | 1])
           |> elem(1)
           |> Noun.equal?(96)

    lsh
  end

  @doc """
  I evaluate lsh at block size 1 and gate-input [2 6].

  lsh(2) evaluates the gate of the block door at block size 2,
  [6 1 2 6] replaces the sample with [2 6].
  """

  @spec lsh2() :: Noun.t()
  def lsh2() do
    lsh = lsh(2)

    assert Nock.nock(lsh, [9, 2, 10, [6, 1, 2 | 6], 0 | 1])
           |> elem(1)
           |> Noun.equal?(1536)

    lsh
  end

  @doc """
  I evaluate rsh at block size 0 and gate-input [2 40].

  rsh(0) evaluates the gate of the block door at block size 0,
  [6 1 2 40] replaces the sample with [2 40].
  """

  @spec rsh0() :: Noun.t()
  def rsh0() do
    rsh = rsh(0)

    assert Nock.nock(rsh, [9, 2, 10, [6, 1, 2 | 40], 0 | 1])
           |> elem(1)
           |> Noun.equal?(10)

    rsh
  end

  @doc """
  I evaluate rsh at block size 1 and gate-input [2 40].

  rsh(1) evaluates the gate of the block door at block size 1,
  [6 1 2 40] replaces the sample with [2 40].
  """

  @spec rsh1() :: Noun.t()
  def rsh1() do
    rsh = rsh(1)

    assert Nock.nock(rsh, [9, 2, 10, [6, 1, 2 | 40], 0 | 1])
           |> elem(1)
           |> Noun.equal?(2)

    rsh
  end

  @doc """
  I evaluate rsh at block size 2 and gate-input [2 40].

  rsh(2) evaluates the gate of the block door at block size 2,
  [6 1 1 40] replaces the sample with [1 40].
  """

  @spec rsh2() :: Noun.t()
  def rsh2() do
    rsh = rsh(2)

    assert Nock.nock(rsh, [9, 2, 10, [6, 1, 1 | 40], 0 | 1])
           |> elem(1)
           |> Noun.equal?(2)

    rsh
  end

  ####################################################################
  ##                           RNG Core                             ##
  ####################################################################

  @doc """
  The gate representing an og core creation with a specified seed.

  Can be gotten by defining

  =l   =>  logics  |=  [seed=@]  ~(. og seed)

  and getting it's arm with [0 2]
  """
  @spec og_arm() :: Noun.t()
  def og_arm() do
    layer_depth = example_layer_depth(7)

    arm =
      "[8 [9 47 0 #{layer_depth}] 10 [6 0 14] 0 2]"
      |> Noun.Format.parse_always()

    sample = 0
    [arm, sample | Nock.Lib.logics_core()]
  end

  @spec og_call(non_neg_integer()) :: :error | {:ok, Noun.t()}
  def og_call(seed) do
    Nock.nock(og_arm(), [9, 2, 10, [6, 1 | seed], 0 | 1])
  end

  @doc """
  I represent a raws gate with a specified instantiated og core given
  as an extra argument.

  Can be gotten by defining locally

  =l    =>  logics  |=  [rng=_og width=@]  (raws:rng width)

  and grabbing the arm with [0 2]
  """
  @spec raws_with_core() :: Noun.t()
  def raws_with_core() do
    arm =
      "[8 [7 [0 12] 9 4 0 1] 9 2 10 [6 0 29] 0 2]"
      |> Noun.Format.parse_always()

    sample = [0, 0]

    [arm, sample | Nock.Lib.logics_core()]
  end

  @spec raws_with_core_call(non_neg_integer(), non_neg_integer()) ::
          :error | {:ok, Noun.t()}
  def raws_with_core_call(core, width) do
    Nock.nock(raws_with_core(), [9, 2, 10, [6, 1 | [core | width]], 0 | 1])
  end

  @spec raws_with_out_core_test() :: any()
  def raws_with_out_core_test() do
    {:ok, og_with_27} = og_call(27)

    {:ok, res1} = raws_call(27, 10)
    assert raws_with_core_call(og_with_27, 10) |> elem(1) |> Noun.equal?(res1)
  end

  @doc """
  I represent a split gate call given an og core with seed.

  Can be gotten by defining locally

  =l    =>  logics  |=  [rng=_og]  split:rng

  and grabbing the arm with [0 2]
  """
  @spec split_arm() :: Noun.t()
  def split_arm() do
    arm = "[7 [0 6] 9 21 0 1]" |> Noun.Format.parse_always()
    sample = 0
    [arm, sample | Nock.Lib.logics_core()]
  end

  @spec split_call(Noun.t()) :: :error | {:ok, Noun.t()}
  def split_call(core) do
    Nock.nock(split_arm(), [9, 2, 10, [6, 1 | core], 0 | 1])
  end

  @spec call_split_test() :: any()
  def call_split_test() do
    {:ok, og_with_123} = og_call(123)
    {:ok, [rng1 | rng2]} = og_with_123 |> split_call()

    # check that these are actual og cores
    {:ok, [rbits1 | _core1]} = raws_with_core_call(rng1, 23)
    {:ok, [rbits2 | _core2]} = raws_with_core_call(rng2, 23)

    # check the bits do not collide
    refute Noun.equal?(rbits1, rbits2)
  end

  ####################################################################
  ##                          Normal Cores                          ##
  ####################################################################

  @doc """
  I am the battery of the fib:tests gate of the anoma stadard library.

  You can dump me by calling

  .*  fib:tests  [0 2]
  """

  @spec factorial_arm() :: Noun.t()
  def factorial_arm() do
    layer_depth = (Nock.Lib.stdlib_layers() + 5) |> Noun.index_to_offset()
    "
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
        [30 8 [9 342 0 #{layer_depth}] 9 2 10 [6 0 62] 0 2]
        10
        [6 [8 [9 20 0 #{layer_depth}] 9 2 10 [6 [0 29] 0 28] 0 2] 0 12]
        0
        1
      ]
      9
      2
      0
      1
    ]" |> Noun.Format.parse_always()
  end

  @spec factorial() :: Noun.t()
  def factorial() do
    sample = 1
    core = [factorial_arm(), sample | Nock.Lib.logics_core()]

    assert Nock.nock(core, [9, 2, 10, [6, 1 | 7], 0 | 1])
           |> elem(1)
           |> Noun.equal?(13),
           "calling into the standard library works well"

    core
  end

  ####################################################################
  ##                             Helpers                            ##
  ####################################################################

  @spec block_calling_biop(Noun.t(), Noun.t()) :: Noun.t()
  defp block_calling_biop(value, index) do
    # get the battery for calling a 2-argument gate located at index
    # `index` at a block door evaluated with block size value `value`

    # check the index of the block by defining block locally
    # =lblock   =>  logics  |=  a=@  block
    # then check the gate index by dumping
    # =lgateblock   =>  logics  |=  a=@  gate:block
    # finally check how the door inputs its block-size by evaluating
    # =>  logics  !=(~(gate block val))
    # with different values
    layer_depth = example_layer_depth(4)

    arm =
      Noun.Format.parse_always(
        "[8 [8 [9 10 0 #{layer_depth}] 9 #{index} 10 [6 7 [0 3] 1 #{value}] 0 2] 9 2 10 [6 [0 28] 0 29] 0 2]"
      )

    sample = [999 | 888]
    [arm, sample | Nock.Lib.logics_core()]
  end

  @spec block_calling_mono(Noun.t(), Noun.t()) :: Noun.t()
  defp block_calling_mono(value, index) do
    # get the battery for calling a 1-argument gate located at index
    # `index` at a block door evaluated with block size value `value`

    # check the index of the block by defining block locally
    # =lblock   =>  logics  |=  a=@  block
    # then check the gate index by dumping
    # =lgateblock   =>  logics  |=  a=@  gate:block
    # finally check how the door inputs its block-size by evaluating
    # =>  logics  !=(~(gate block val))
    # with different values
    layer_depth = example_layer_depth(4)

    arm =
      Noun.Format.parse_always(
        "[8 [8 [9 10 0 #{layer_depth}] 9 #{index} 10 [6 7 [0 3] 1 #{value}] 0 2] 9 2 10 [6 0 14] 0 2]"
      )

    sample = 999
    [arm, sample | Nock.Lib.logics_core()]
  end

  @spec increment_counter_val(Noun.t()) :: Noun.t()
  def increment_counter_val(val) do
    arm = [[1 | val], 4, 12, [1 | 0], [0 | 6], 1, val | 0]
    sample = 0
    [[8, [1 | sample], [1 | arm], 0 | 1] | Nock.Lib.logics_core()]
  end

  # [%ctr 0]
  @spec zero_counter(Noun.t()) :: Noun.t()
  def zero_counter(val) do
    arm = [1, val | 0]
    sample = 0
    [[8, [1 | sample], [1 | arm], 0 | 1] | Nock.Lib.logics_core()]
  end

  ####################################################################
  ##                    Cueing to a Jam Session                     ##
  ####################################################################

  # I want the results to be useful here... sadly they are not ☹
  # Maybe move the tests to the core itself... idk

  # todo: BAD. fix the disaster left by integer jam
  @spec jamming_and_cueing() :: :ok
  def jamming_and_cueing() do
    jam_and_cue(0, atom_integer_to_binary(2))
    jam_and_cue(1, atom_integer_to_binary(12))
    jam_and_cue(2, atom_integer_to_binary(72))
    jam_and_cue(19, atom_integer_to_binary(2480))
    jam_and_cue(581_949_002, atom_integer_to_binary(1_191_831_557_952))
    jam_and_cue([0 | 19], atom_integer_to_binary(39_689))
    jam_and_cue([1 | 1], atom_integer_to_binary(817))
    jam_and_cue([10_000 | 10_000], atom_integer_to_binary(4_952_983_169))

    jam_and_cue(
      [65_536 | <<0, 0, 1>>],
      atom_integer_to_binary(158_376_919_809)
    )

    jam_and_cue(
      [999_999_999 | 999_999_999],
      atom_integer_to_binary(1_301_217_674_263_809)
    )

    jam_and_cue(
      [222, 444 | 888],
      atom_integer_to_binary(250_038_217_192_960_129)
    )

    jam_and_cue(
      [[107 | 110] | [107 | 110]],
      atom_integer_to_binary(635_080_761_093)
    )

    jam_and_cue(
      [0, 1, 2, 3, 4, 5, 6, 7, 8, 9 | 10],
      atom_integer_to_binary(25_681_224_503_728_653_597_984_370_231_065)
    )

    jam_and_cue(
      [99, 100, 101, 102, 103, 104 | 0],
      atom_integer_to_binary(223_372_995_869_285_333_705_242_560_449)
    )

    jam_and_cue(
      [[222, 444 | 888] | [222, 444 | 888]],
      atom_integer_to_binary(170_479_614_045_978_345_989)
    )

    jam_and_cue(
      [[0 | 1], [1 | 2], [2 | 3], [3 | 4] | 0],
      atom_integer_to_binary(11_976_248_475_217_237_797)
    )

    jam_and_cue(
      [
        [0 | 1],
        [1 | 2],
        [2 | 3],
        [3 | 4],
        [4 | 5],
        [5 | 6],
        [6 | 7],
        [7 | 8],
        [8 | 9] | 0
      ],
      atom_integer_to_binary(
        7_694_087_033_387_855_647_747_387_855_514_468_399_947_749_137_782_565
      )
    )

    jam_and_cue(
      [
        [0 | 1],
        [2 | 3],
        [4 | 5],
        [6 | 7],
        [8 | 9],
        [10 | 11],
        [12 | 13],
        [14 | 15],
        [16 | 17],
        [18 | 19],
        [20 | 21] | 0
      ],
      atom_integer_to_binary(
        308_947_677_754_874_070_959_300_747_182_056_036_528_545_493_781_368_831_595_479_491_505_523_344_414_501
      )
    )

    assert Noun.equal?(
             dec(),
             dec() |> Noun.Jam.jam() |> Noun.Jam.cue!()
           )

    :ok
  end

  @spec jam_and_cue(any(), any()) :: any()
  def jam_and_cue(jam_value, cue_value) do
    assert Noun.equal?(jam_value, Noun.Jam.cue!(cue_value))
    assert cue_value == Noun.Jam.jam(Noun.normalize_noun(jam_value))
  end

  ####################################################################
  ##                          Scry Crash                            ##
  ####################################################################

  @spec nock_scry_crash() :: Noun.t()
  def nock_scry_crash() do
    code = [12, [1 | 0] | [1 | 0]]

    :error =
      Nock.nock(0, code, %Nock{
        scry_function: fn _ -> raise("this is your last scry") end
      })

    {:ok, 123} =
      Nock.nock(0, code, %Nock{
        scry_function: fn _ -> {:ok, 123} end
      })

    code
  end
end
