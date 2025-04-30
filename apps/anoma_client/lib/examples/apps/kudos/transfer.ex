defmodule Anoma.Client.Examples.Apps.Kudos.Transfer do
  @moduledoc """
  I implement examples that run Kudos merge transactions against the client.
  """
  use Anoma.Client.Web.ConnCase
  use EventBroker.WithSubscription

  alias Anoma.Client.Examples.Apps.Kudos
  alias Anoma.Client.Examples.Apps.Kudos.Helpers
  alias Anoma.Client.Examples.Apps.Kudos.Initialize
  alias Noun.Jam
  alias Noun.Nounable

  require Anoma.Node.Examples.EEvent

  import Anoma.Client.Examples.Apps.Kudos

  @doc """
  I create a transfer between two users. This example plays out the following
  scenario.

    - User Caracalla initializes 12 caracallas for themselves
    - User Caracalla transfers 12 caracallas to User Bob.
    - User Caracalla has no resources.
    - User Bob has 12 caracallas.
  """

  @spec transfer_from_caracalla_to_bob(Kudos.t()) :: Kudos.t()
  def transfer_from_caracalla_to_bob(
        kudos \\ setup(),
        {sender, receiver} \\ {:caracalla, :bob}
      ) do
    # verify the resources bob has, before the transfer
    resources_before_transfer =
      Enum.count(Helpers.resources_for(kudos, receiver))

    # this call initializes, and returns the resources for this user.
    {_kudos, _, _amount, _kind} = Initialize.initialize_kudos(kudos, sender)
    [resource] = Helpers.resources_for(kudos, sender)

    resource_jammed = resource |> Nounable.to_noun() |> Jam.jam()

    # compute the proved logic
    {_kudos, proved_logic} = Initialize.prove_logic(kudos)

    # ----------------------------------------------------------------------------
    # Prove transfer logic

    transfer_nockma = Kudos.nockma_file(:transfer)

    # To prove the transfer logic, I need to supply a few inputs:
    # To construct the inputs for the transfer transaction, have a look at
    # apps/anoma_client/priv/juvix/src/Kudos/Transfer.juvix
    latest_root = Helpers.get_latest_root(kudos)
    owner_sign = Map.get(kudos.users, sender).sign_key
    owner_publ = Map.get(kudos.users, sender).public_key
    recvr_publ = Map.get(kudos.users, receiver).public_key

    # create the inputs for the transfer transaction
    payload = %{
      program: Base.encode64(transfer_nockma),
      private_inputs: [
        Base.encode64(Jam.jam(latest_root)),
        Base.encode64(Jam.jam(:crypto.strong_rand_bytes(32))),
        Base.encode64(Jam.jam([" " | owner_sign])),
        Base.encode64(Jam.jam([" " | owner_publ])),
        Base.encode64(Jam.jam([" " | recvr_publ])),
        Base.encode64(resource_jammed),
        proved_logic
      ]
    }

    data =
      kudos.client.conn
      |> post(~p"/nock/prove", payload)
      |> json_response(200)

    # assert the transaction succeeded
    assert Kernel.match?(%{"io" => [], "result" => binary}, data)

    # extract the prove result
    proved_transfer = data["result"]

    # ----------------------------------------------------------------------------
    # Submit the transaction

    :ok = Helpers.submit_transaction_and_confirm(kudos, proved_transfer)

    # ----------------------------------------------------------------------------
    # Assert the resources of bob and caracalla

    assert Enum.empty?(Helpers.resources_for(kudos, sender))

    assert Enum.count(Helpers.resources_for(kudos, receiver)) ==
             resources_before_transfer + 1

    kudos
  end
end
