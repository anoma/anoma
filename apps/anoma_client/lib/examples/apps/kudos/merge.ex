defmodule Anoma.Client.Examples.Apps.Kudos.Merge do
  @moduledoc """
  I implement the merge examples in Kudos.

  Merging means that two resources are merged into one resource for a third party.
  """
  use Anoma.Client.Web.ConnCase
  use EventBroker.WithSubscription

  alias Anoma.Client.Examples.Apps.Kudos
  alias Anoma.Client.Examples.Apps.Kudos.Helpers
  alias Anoma.Client.Examples.Apps.Kudos.Initialize
  alias Anoma.Client.Examples.Apps.Kudos.Transfer
  alias Noun.Jam
  alias Noun.Nounable

  require Anoma.Node.Examples.EEvent

  import Anoma.Client.Examples.Apps.Kudos

  @spec merge_2_in_1(Kudos.t()) :: Kudos.t()
  def merge_2_in_1(kudos \\ setup()) do
    # ----------------------------------------------------------------------------
    # Initialize bob with a resource and transfer it to bob.
    #
    # I do this twice, so bob has two resources.

    Transfer.transfer_from_caracalla_to_bob(kudos)
    Transfer.transfer_from_caracalla_to_bob(kudos)

    # bob will have two resources now
    resources = Helpers.resources_for(kudos, :bob)

    assert 2 == Enum.count(resources)

    # ----------------------------------------------------------------------------
    # Prove merge logic

    # compute the proved logic
    {_kudos, proved_logic} = Initialize.prove_logic(kudos)

    merge_nockma = Kudos.nockma_file(:merge)

    # To prove the merge logic, I need to supply a few inputs:
    # To construct the inputs for the transfer transaction, have a look at
    # apps/anoma_client/priv/juvix/src/Kudos/Merge.juvix
    latest_root = Helpers.get_latest_root(kudos)
    owner_sign = Map.get(kudos.users, :bob).sign_key
    owner_publ = Map.get(kudos.users, :bob).public_key
    recvr_publ = Map.get(kudos.users, :alice).public_key

    # this has to be double jammed, I don't know why.
    resources_jammed =
      Enum.map(resources, fn r -> Nounable.to_noun(r) |> Jam.jam() end)
      |> Jam.jam()

    # create the inputs for the merge transaction
    payload = %{
      program: Base.encode64(merge_nockma),
      private_inputs: [
        Base.encode64(Jam.jam(latest_root)),
        Base.encode64(Jam.jam(:crypto.strong_rand_bytes(32))),
        Base.encode64(Jam.jam([" " | owner_sign])),
        Base.encode64(Jam.jam([" " | owner_publ])),
        Base.encode64(resources_jammed),
        Base.encode64(Jam.jam([" " | recvr_publ])),
        proved_logic
      ]
    }

    data =
      kudos.client.conn
      |> post(~p"/nock/prove", payload)
      |> json_response(200)

    # assert the transaction succeeded
    assert Kernel.match?(%{"result" => binary}, data)

    # extract the prove result
    proved_merge = data["result"]

    assert proved_merge != "error"

    # ----------------------------------------------------------------------------
    # Submit the transaction

    :ok = Helpers.submit_transaction_and_confirm(kudos, proved_merge)

    # ----------------------------------------------------------------------------
    # Verify that the right users have the right resources.

    # bob should have 0 resources now.
    resources = Helpers.resources_for(kudos, :bob)
    assert Enum.empty?(resources)

    # alice should have 1 resource now.
    resources = Helpers.resources_for(kudos, :alice)
    assert 1 == Enum.count(resources)

    kudos
  end
end
