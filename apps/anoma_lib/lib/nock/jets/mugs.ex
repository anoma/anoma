defmodule Nock.Jets.Mugs do
  @moduledoc """
  I am the Mugs module for the Nock Jet system.

  I have all the information for the mugs of any jetted arm in the Anoma
  standard library.
  """

  alias Nock.Jets

  @index_map %{
    :dec => %{:index => 342, :layer => 1},
    :add => %{:index => 20, :layer => 1},
    :sub => %{:index => 47, :layer => 1},
    :lth => %{:index => 343, :layer => 1},
    :lte => %{:index => 84, :layer => 1},
    :gth => %{:index => 43, :layer => 1},
    :gte => %{:index => 22, :layer => 1},
    :mul => %{:index => 4, :layer => 1},
    :div => %{:index => 170, :layer => 1},
    :mod => %{:index => 46, :layer => 1},
    :verify => %{:index => 4, :layer => 6},
    :sign => %{:index => 10, :layer => 6},
    :verify_detatched => %{:index => 22, :layer => 6},
    :sign_detatched => %{:index => 23, :layer => 6},
    :bex => %{:index => 4, :layer => 4},
    :mix => %{:index => 4, :layer => 5},
    :jam => %{:index => 22, :layer => 5},
    :cue => %{:index => 94, :layer => 5},
    :mat => %{:index => 43, :layer => 5},
    :shax => %{:index => 22, :layer => 7},
    :raw => %{:index => 23, :layer => 7, :door => 47},
    :raws => %{:index => 4, :layer => 7, :door => 47},
    :rad => %{:index => 20, :layer => 7, :door => 47},
    :rads => %{:index => 22, :layer => 7, :door => 47},
    :split => %{:index => 21, :layer => 7, :door => 47},
    :met => %{:index => 190, :layer => 4, :door => 10},
    :end => %{:index => 367, :layer => 4, :door => 10},
    :lsh => %{:index => 90, :layer => 4, :door => 10},
    :rsh => %{:index => 767, :layer => 4, :door => 10},
    :abs => %{:index => 1515, :layer => 8},
    :dif_8 => %{:index => 759, :layer => 8},
    :dul => %{:index => 22, :layer => 8},
    :fra => %{:index => 190, :layer => 8},
    :pro => %{:index => 46, :layer => 8},
    :rem => %{:index => 1514, :layer => 8},
    :sum => %{:index => 4, :layer => 8},
    :sun => %{:index => 10, :layer => 8},
    :syn => %{:index => 188, :layer => 8},
    :cmp => %{:index => 191, :layer => 8},
    :new => %{:index => 758, :layer => 8},
    :old => %{:index => 756, :layer => 8},
    :mug => %{:index => 189, :layer => 9},
    :dor => %{:index => 765, :layer => 9},
    :gor => %{:index => 190, :layer => 9},
    :mor => %{:index => 10, :layer => 9},
    :silt => %{:index => 22, :layer => 10},
    :put_10 => %{:index => 84, :layer => 10, :door => 21},
    :wyt => %{:index => 92, :layer => 10, :door => 21},
    :tap => %{:index => 186, :layer => 10, :door => 21},
    :uni => %{:index => 174, :layer => 10, :door => 21},
    :int => %{:index => 85, :layer => 10, :door => 21},
    :dif_10 => %{:index => 175, :layer => 10, :door => 21},
    :duni => %{:index => 763, :layer => 10, :door => 21},
    :has => %{:index => 762, :layer => 10, :door => 21},
    :put_11 => %{:index => 340, :layer => 11, :door => 93},
    :got => %{:index => 701, :layer => 11, :door => 93},
    :secp256k1_sign => %{:index => 23, :layer => 12},
    :secp256k1_verify => %{:index => 10, :layer => 12},
    :secp256k1_pub_key => %{:index => 4, :layer => 12},
    :kind => %{:index => 5972, :layer => Nock.Lib.stdlib_layers()},
    :delta_add => %{:index => 372, :layer => Nock.Lib.stdlib_layers()},
    :delta_sub => %{:index => 12013, :layer => Nock.Lib.stdlib_layers()},
    :zero_delta => %{:index => 174, :layer => Nock.Lib.stdlib_layers()},
    :resource_delta => %{:index => 701, :layer => Nock.Lib.stdlib_layers()},
    :commitment => %{:index => 3002, :layer => Nock.Lib.stdlib_layers()},
    :is_commitment => %{:index => 12012, :layer => Nock.Lib.stdlib_layers()},
    :nullifier => %{:index => 2815, :layer => Nock.Lib.stdlib_layers()},
    :is_nullifier => %{:index => 5974, :layer => Nock.Lib.stdlib_layers()},
    :compliance_unit_delta => %{
      :index => 702,
      :layer => Nock.Lib.stdlib_layers()
    },
    :action_delta => %{:index => 4, :layer => Nock.Lib.stdlib_layers()},
    :make_delta => %{:index => 11951, :layer => Nock.Lib.stdlib_layers()},
    :action_create => %{:index => 382, :layer => Nock.Lib.stdlib_layers()},
    :trm_compliance_key => %{
      :index => 1502,
      :layer => Nock.Lib.stdlib_layers()
    },
    :trm_delta_key => %{:index => 374, :layer => Nock.Lib.stdlib_layers()},
    :t_compose => %{:index => 383, :layer => Nock.Lib.stdlib_layers()},
    :cairo_compose => %{:index => 92, :layer => Nock.Lib.stdlib_layers()},
    :cairo_create_from_cus => %{
      :index => 1406,
      :layer => Nock.Lib.stdlib_layers()
    },
    :cairo_prove_delta => %{
      :index => 1503,
      :layer => Nock.Lib.stdlib_layers()
    }
  }

  # hardcoded jet registry
  # valid statuses:
  # - :enabled, jet is fully enabled
  # - :disabled, jet is fully disabled
  # - :check, check that jet and naive produce the same result
  @jet_registry Jets.generate_registry(
                  @index_map,
                  [
                    {:dec, &Nock.Jets.dec/1, :enabled, 10},
                    {:add, &Nock.Jets.add/1, :enabled, 10},
                    {:sub, &Nock.Jets.sub/1, :enabled, 10},
                    {:lth, &Nock.Jets.lth/1, :enabled, 10},
                    {:lte, &Nock.Jets.lte/1, :enabled, 10},
                    {:gth, &Nock.Jets.gth/1, :enabled, 10},
                    {:gte, &Nock.Jets.gte/1, :enabled, 10},
                    {:mul, &Nock.Jets.mul/1, :enabled, 10},
                    {:div, &Nock.Jets.div/1, :enabled, 10},
                    {:mod, &Nock.Jets.mod/1, :enabled, 10},
                    {:verify, &Nock.Jets.verify/1, :enabled, 100},
                    {:sign, &Nock.Jets.sign/1, :enabled, 100},
                    {:verify_detatched, &Nock.Jets.verify_detatched/1,
                     :enabled, 100},
                    {:sign_detatched, &Nock.Jets.sign_detatched/1, :enabled,
                     100},
                    {:bex, &Nock.Jets.bex/1, :enabled, 20},
                    {:mix, &Nock.Jets.mix/1, :enabled, 20},
                    {:jam, &Nock.Jets.jam/1, :enabled, 50},
                    {:cue, &Nock.Jets.cue/1, :enabled, 50},
                    {:shax, &Nock.Jets.shax/1, :enabled, 100},
                    {:met, &Nock.Jets.met/1, :enabled, 20},
                    {:end, &Nock.Jets.nend/1, :enabled, 20},
                    {:lsh, &Nock.Jets.lsh/1, :enabled, 20},
                    {:rsh, &Nock.Jets.rsh/1, :enabled, 20},
                    {:abs, &Nock.Jets.abs/1, :enabled, 30},
                    {:dif_8, &Nock.Jets.dif/1, :enabled, 30},
                    {:dul, &Nock.Jets.dul/1, :enabled, 30},
                    {:fra, &Nock.Jets.fra/1, :enabled, 30},
                    {:pro, &Nock.Jets.pro/1, :enabled, 30},
                    {:rem, &Nock.Jets.rem/1, :enabled, 30},
                    {:sum, &Nock.Jets.sum/1, :enabled, 30},
                    {:sun, &Nock.Jets.sun/1, :enabled, 30},
                    {:syn, &Nock.Jets.syn/1, :enabled, 30},
                    {:cmp, &Nock.Jets.cmp/1, :enabled, 30},
                    {:mug, &Nock.Jets.nmug/1, :enabled, 50},
                    {:dor, &Nock.Jets.dor/1, :enabled, 30},
                    {:gor, &Nock.Jets.gor/1, :enabled, 30},
                    {:mor, &Nock.Jets.mor/1, :enabled, 30},
                    {:silt, &Nock.Jets.silt/1, :enabled, 30},
                    {:put_10, &Nock.Jets.put/1, :enabled, 30},
                    {:uni, &Nock.Jets.uni/1, :enabled, 30},
                    {:int, &Nock.Jets.int/1, :enabled, 30},
                    {:dif_10, &Nock.Jets.sdif/1, :enabled, 30},
                    {:duni, &Nock.Jets.duni/1, :enabled, 30},
                    {:has, &Nock.Jets.has/1, :enabled, 30},
                    {:put_11, &Nock.Jets.mput/1, :enabled, 30},
                    {:got, &Nock.Jets.got/1, :enabled, 30},
                    {:kind, &Nock.Jets.kind/1, :enabled, 100},
                    {:delta_add, &Nock.Jets.delta_add/1, :enabled, 50},
                    {:delta_sub, &Nock.Jets.delta_sub/1, :enabled, 50},
                    {:resource_delta, &Nock.Jets.resource_delta/1, :enabled,
                     50},
                    {:compliance_unit_delta, &Nock.Jets.compliance_delta/1,
                     :enabled, 10},
                    {:action_delta, &Nock.Jets.action_delta/1, :enabled, 50},
                    {:make_delta, &Nock.Jets.make_delta/1, :enabled, 50},
                    {:action_create, &Nock.Jets.action_create/1, :enabled,
                     10},
                    {:trm_compliance_key, &Nock.Jets.trm_compliance_key/1,
                     :enabled, 10},
                    {:trm_delta_key, &Nock.Jets.trm_delta_key/1, :enabled,
                     10},
                    {:t_compose, &Nock.Jets.t_compose/1, :enabled, 10},
                    {:cairo_compose, &Nock.Jets.cairo_compose/1, :enabled,
                     10},
                    {:cairo_create_from_cus,
                     &Nock.Jets.cairo_create_from_cus/1, :enabled, 10},
                    {:cairo_prove_delta, &Nock.Jets.cairo_prove_delta/1,
                     :enabled, 10},
                    {:secp256k1_sign, &Nock.Jets.secp256k1_sign/1, :enabled,
                     10},
                    {:secp256k1_verify, &Nock.Jets.secp256k1_verify/1,
                     :enabled, 10},
                    {:secp256k1_pub_key, &Nock.Jets.secp256k1_public_key/1,
                     :enabled, 10}
                  ]
                )

  @doc """
  Gives the jet registry
  """
  @spec jet_registry() :: map()
  def jet_registry, do: @jet_registry

  @doc """
  I provide the index map
  """
  @spec index_map() :: map()
  def index_map, do: @index_map
end
