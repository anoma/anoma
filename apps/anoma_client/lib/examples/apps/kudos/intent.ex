defmodule Anoma.Client.Examples.Apps.Kudos.Intent do
  @moduledoc """
  I implement an example of submitting an intent to the solver.
  """
  use Anoma.Client.Web.ConnCase
  use EventBroker.WithSubscription

  alias Anoma.Client.Examples.Apps.Kudos
  alias Anoma.Client.Examples.Apps.Kudos.Helpers
  alias Anoma.Client.Examples.Apps.Kudos.Balance
  alias Anoma.Client.Examples.Apps.Kudos.Initialize

  alias Noun.Jam
  alias Noun.Nounable
  alias Anoma.RM.Transparent.Resource

  require Anoma.Node.Examples.EEvent
  require Logger

  import Anoma.Client.Examples.Apps.Kudos

  @doc """
  I create an intent that says from wants to swap a resource with to.

  This example assumes that the user bob's resource is of kind bob.
  """
  @spec create_swap_intent(Kudos.t(), Keyword.t()) :: {Kudos.t(), binary()}
  def create_swap_intent(kudos, opts) do
    opts = Keyword.validate!(opts, [:from, :to, :want, :give, :quantity])
    from = Keyword.fetch!(opts, :from)
    to = Keyword.fetch!(opts, :to)
    want = Keyword.fetch!(opts, :want)
    give = Keyword.fetch!(opts, :give)
    quantity = Keyword.fetch!(opts, :quantity)

    from_user = Map.get(kudos.users, from)
    to_user = Map.get(kudos.users, to)

    # ----------------------------------------------------------------------------
    # Prove GetGiveResource for bob

    resources_sender = Helpers.resources_for(kudos, from)

    {kudos, proved_get_give_bob} =
      prove_get_give_resource(kudos, give, quantity, resources_sender)

    # ----------------------------------------------------------------------------
    # Prove the Swap nockma

    swap_nockma = Kudos.nockma_file(:swap)

    # compute the proved logic
    {_kudos, proved_logic} = Initialize.prove_logic(kudos)

    latest_root = Helpers.get_latest_root(kudos)

    payload = %{
      program: Base.encode64(swap_nockma),
      private_inputs: [
        Base.encode64(Jam.jam(latest_root)),
        Base.encode64(Jam.jam(:crypto.strong_rand_bytes(32))),
        Base.encode64(Jam.jam([" " | from_user.sign_key])),
        Base.encode64(Jam.jam([" " | from_user.public_key])),
        proved_logic,
        proved_get_give_bob,
        Base.encode64(Jam.jam(want)),
        Base.encode64(Jam.jam([" " | to_user.public_key])),
        Base.encode64(Jam.jam(12))
      ]
    }

    # make the request to the prove endpoint, and expect a transaction back.
    data =
      kudos.client.conn
      |> post(~p"/nock/prove", payload)
      |> json_response(200)

    assert Kernel.match?(%{"io" => [], "result" => binary}, data)

    # extract the prove result
    proved_swap = data["result"]

    # count the current intents in the intentpool
    before = Enum.count(Helpers.list_intents(kudos))

    # submit the intent to the intentpool
    Helpers.submit_intent(kudos, proved_swap)

    # at this point a single intent is in the intentpool so the balances
    # should not have changed.
    after_submit = Enum.count(Helpers.list_intents(kudos))

    assert after_submit - before == 1

    {kudos, proved_swap}
  end

  @doc """
  I create two intents that tell bob wants to swap with alice, and vice versa.

  I assert that the two intents are added and then solved by the basic solver.
  """
  @spec do_swap(Kudos.t(), {atom(), atom()}) :: Kudos.t()
  def do_swap(kudos \\ setup(), {from, to} \\ {:bob, :alice}) do
    with_subscription [[]] do
      # To know when the transaction has been added, I listen for events that
      # signal a new transaction. As soon as that event occurs, I can rest assured
      # that the intents have been solved.

      # ----------------------------------------------------------------------------
      # Initialize bob and alice both with resources.

      Initialize.initialize_kudos(kudos, from)
      Initialize.initialize_kudos(kudos, to)

      # bob will have two resources now
      assert 1 == Enum.count(Helpers.resources_for(kudos, from))
      assert 1 == Enum.count(Helpers.resources_for(kudos, to))

      # ----------------------------------------------------------------------------
      # Count the intents in the intentpool before adding two of my own.

      before = Enum.count(Helpers.list_intents(kudos))

      # ----------------------------------------------------------------------------
      # Swap bob to alice

      create_swap_intent(kudos,
        from: :bob,
        to: :alice,
        want: "alice",
        give: "bob",
        quantity: 12
      )

      # ----------------------------------------------------------------------------
      # Swap alice to bob

      create_swap_intent(kudos,
        from: :alice,
        to: :bob,
        want: "bob",
        give: "alice",
        quantity: 12
      )

      # ----------------------------------------------------------------------------
      # Wait for 10 blocsk to be sure the intents have been solved and are in
      # the mempool.

      Helpers.wait_n_blocks(kudos, 10)

      # ----------------------------------------------------------------------------
      # The solver should have solved these two intents, and they should no longer be in the intent pool.

      assert Enum.count(Helpers.list_intents(kudos)) == before

      # ----------------------------------------------------------------------------
      # Verify bob has alice, and alice has bob.
      {_kudos, balances_from} = Balance.check_balance(kudos, from)
      assert balances_from == [{"alice", 12}]

      {_kudos, balances_to} = Balance.check_balance(kudos, to)

      assert balances_to == [{"bob", 12}]
      kudos
    end
  end

  @doc """
  I prove the Swap.nockma file and return its result.
  """
  @spec prove_swap(Kudos.t()) :: {Kudos.t(), String.t()}
  def prove_swap(kudos \\ setup()) do
    nockma = Kudos.nockma_file(:swap)

    payload = %{"program" => Base.encode64(nockma)}

    data =
      kudos.client.conn
      |> post(~p"/nock/prove", payload)
      |> json_response(200)

    assert Kernel.match?(%{"io" => [], "result" => binary}, data)

    # extract the prove result
    proved = data["result"]

    {kudos, proved}
  end

  @doc """
  I prove the GetGiveResource.nockma file for a given intent. I encode the
  symbol the owner wants to give and the quantity. For this i need a list of the
  owner's resources.
  """
  @spec prove_get_give_resource(Kudos.t(), String.t(), number(), [
          Resource.t()
        ]) :: {Kudos.t(), binary()}
  def prove_get_give_resource(kudos, symbol, quantity, resources) do
    nockma = Kudos.nockma_file(:get_give_resource)

    resources_jammed =
      Enum.map(resources, fn r -> Nounable.to_noun(r) |> Jam.jam() end)
      |> Jam.jam()

    payload = %{
      "program" => Base.encode64(nockma),
      "private_inputs" => [
        Base.encode64(Jam.jam(symbol)),
        Base.encode64(Jam.jam(quantity)),
        Base.encode64(resources_jammed)
      ]
    }

    data =
      kudos.client.conn
      |> post(~p"/nock/prove", payload)
      |> json_response(200)

    assert Kernel.match?(%{"io" => [], "result" => binary}, data)

    # extract the prove result
    proved = data["result"]

    assert proved != "error"

    {kudos, proved}
  end
end
