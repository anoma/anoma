defmodule Nock.Jets.Mugs do
  @layer_1_context_mug Nock.Jets.calculate_mug_of_layer(1)
  @layer_4_context_mug 1_869_390_925
  @layer_5_context_mug 4_018_337_361
  @layer_6_context_mug 3_932_234_981
  @layer_7_context_mug 4_202_542_228
  @layer_8_context_mug 1_736_366_676
  @layer_4_block_context_mug 2_756_805_836

  # always the topmost layer
  @layer_rm_context_mug 2_116_148_812

  # hardcoded jet registry
  # valid statuses:
  # - :enabled, jet is fully enabled
  # - :disabled, jet is fully disabled
  # - :check, check that jet and naive produce the same result
  @jet_registry %{
    3_739_349_216 =>
      {"dec", 7, @layer_1_context_mug, &Nock.Jets.dec/1, :enabled, 10},
    3_819_277_753 =>
      {"add", 7, @layer_1_context_mug, &Nock.Jets.add/1, :enabled, 10},
    2_374_874_615 =>
      {"sub", 7, @layer_1_context_mug, &Nock.Jets.sub/1, :enabled, 10},
    1_130_480_894 =>
      {"lth", 7, @layer_1_context_mug, &Nock.Jets.lth/1, :enabled, 10},
    2_271_972_775 =>
      {"lte", 7, @layer_1_context_mug, &Nock.Jets.lte/1, :enabled, 10},
    3_066_075_584 =>
      {"gth", 7, @layer_1_context_mug, &Nock.Jets.gth/1, :enabled, 10},
    2_449_621_320 =>
      {"gte", 7, @layer_1_context_mug, &Nock.Jets.gte/1, :enabled, 10},
    2_208_363_748 =>
      {"mul", 7, @layer_1_context_mug, &Nock.Jets.mul/1, :enabled, 10},
    91_135_323 =>
      {"div", 7, @layer_1_context_mug, &Nock.Jets.div/1, :enabled, 10},
    567_449_323 =>
      {"mod", 7, @layer_1_context_mug, &Nock.Jets.mod/1, :enabled, 10},
    1_531_543_893 =>
      {"verify", 7, @layer_6_context_mug, &Nock.Jets.verify/1, :enabled, 100},
    3_991_451_804 =>
      {"sign", 7, @layer_6_context_mug, &Nock.Jets.sign/1, :enabled, 100},
    3_656_885_839 =>
      {"verify-detatched", 7, @layer_6_context_mug,
       &Nock.Jets.verify_detatched/1, :enabled, 100},
    1_699_002_748 =>
      {"sign-detatched", 7, @layer_6_context_mug, &Nock.Jets.sign_detatched/1,
       :enabled, 100},
    1_585_653_763 =>
      {"bex", 7, @layer_4_context_mug, &Nock.Jets.bex/1, :enabled, 20},
    1_023_807_257 =>
      {"mix", 7, @layer_5_context_mug, &Nock.Jets.mix/1, :enabled, 20},
    2_968_763_525 =>
      {"jam", 7, @layer_5_context_mug, &Nock.Jets.jam/1, :enabled, 50},
    3_271_615_052 =>
      {"cue", 7, @layer_5_context_mug, &Nock.Jets.cue/1, :enabled, 50},
    1_423_749_879 =>
      {"shax", 7, @layer_7_context_mug, &Nock.Jets.shax/1, :enabled, 100},
    1_761_078_299 =>
      {"met", 14, @layer_4_block_context_mug, &Nock.Jets.met/1, :enabled, 20},
    3_976_423_375 =>
      {"end", 14, @layer_4_block_context_mug, &Nock.Jets.nend/1, :enabled, 20},
    3_534_989_962 =>
      {"lsh", 14, @layer_4_block_context_mug, &Nock.Jets.lsh/1, :enabled, 20},
    3_410_895_654 =>
      {"rsh", 14, @layer_4_block_context_mug, &Nock.Jets.rsh/1, :enabled, 20},
    724_462_226 =>
      {"abs", 7, @layer_8_context_mug, &Nock.Jets.abs/1, :enabled, 30},
    2_668_782_675 =>
      {"dif", 7, @layer_8_context_mug, &Nock.Jets.dif/1, :enabled, 30},
    1_814_685_155 =>
      {"dul", 7, @layer_8_context_mug, &Nock.Jets.dul/1, :enabled, 30},
    2_357_319_448 =>
      {"fra", 7, @layer_8_context_mug, &Nock.Jets.fra/1, :enabled, 30},
    2_272_237_948 =>
      {"pro", 7, @layer_8_context_mug, &Nock.Jets.pro/1, :enabled, 30},
    2_517_398_177 =>
      {"rem", 7, @layer_8_context_mug, &Nock.Jets.rem/1, :enabled, 30},
    2_325_836_748 =>
      {"sum", 7, @layer_8_context_mug, &Nock.Jets.sum/1, :enabled, 30},
    244_446_486 =>
      {"sun", 7, @layer_8_context_mug, &Nock.Jets.sun/1, :enabled, 30},
    1_720_910_226 =>
      {"syn", 7, @layer_8_context_mug, &Nock.Jets.syn/1, :enabled, 30},
    3_800_851_664 =>
      {"cmp", 7, @layer_8_context_mug, &Nock.Jets.cmp/1, :enabled, 30},
    3_823_717_687 =>
      {"delta-add", 7, @layer_rm_context_mug, &Nock.Jets.delta_add/1,
       :enabled, 50},
    332_825_089 =>
      {"delta-sub", 7, @layer_rm_context_mug, &Nock.Jets.delta_sub/1,
       :enabled, 50},
    4_289_938_596 =>
      {"action-delta", 7, @layer_rm_context_mug, &Nock.Jets.action_delta/1,
       :enabled, 50},
    4_289_816_935 =>
      {"make-delta", 7, @layer_rm_context_mug, &Nock.Jets.make_delta/1,
       :enabled, 50}
  }

  @doc """
  Gives the jet registry
  """
  @spec jet_registry() :: map()
  def jet_registry, do: @jet_registry
end
