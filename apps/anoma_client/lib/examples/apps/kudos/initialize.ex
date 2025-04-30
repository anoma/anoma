defmodule Anoma.Client.Examples.Apps.Kudos.Initialize do
  @moduledoc """
  I implement examples on how to initialize kudos.
  """
  use Anoma.Client.Web.ConnCase
  use EventBroker.WithSubscription

  alias Anoma.Client.Examples.Apps.Kudos
  alias Anoma.Client.Examples.Apps.Kudos.Helpers
  alias Anoma.Node.Utility.Indexer
  alias Anoma.RM.Transparent.Resource
  alias Noun.Jam

  require Anoma.Node.Examples.EEvent

  import Anoma.Client.Examples.Apps.Kudos

  @doc """
  I prove the Logic.nockma file and return its result.
  """
  @spec prove_logic(Kudos.t()) :: {Kudos.t(), String.t()}
  def prove_logic(kudos \\ setup()) do
    logic_nockma = Kudos.nockma_file(:logic)

    payload = %{"program" => Base.encode64(logic_nockma)}

    data =
      kudos.client.conn
      |> post(~p"/nock/prove", payload)
      |> json_response(200)

    assert Kernel.match?(%{"io" => [], "result" => binary}, data)

    # extract the prove result
    proved_logic = data["result"]

    {kudos, proved_logic}
  end

  @doc """
  I create a new Kudos resource.
  """
  @spec initialize_kudos(Kudos.t(), atom()) ::
          {Kudos.t(), binary(), non_neg_integer(), String.t()}
  def initialize_kudos(kudos \\ setup(), user \\ :bob) do
    # compute the proved logic
    {kudos, proved_logic} = prove_logic(kudos)

    # read the nockma file for create
    create_nockma = Kudos.nockma_file(:create)

    # ----------------------------------------------------------------------------
    # Create the creation transaction by proving  Create.nockma with the proper
    # parameters.
    #
    # The sign key and public key have to be padded with a cell, for an unknown
    # reason. It's just like that.
    #
    # The amount is the amount we wish to create. In this case, 10.
    #
    # The kind in this case is the username of the user that got auto-generated.
    # This can be "banana", or "apple", etc.

    # amount to create
    amount = 12
    kind = Map.get(kudos.users, user).username

    payload = %{
      program: Base.encode64(create_nockma),
      private_inputs: [
        Base.encode64(Jam.jam([" " | Map.get(kudos.users, user).sign_key])),
        Base.encode64(Jam.jam([" " | Map.get(kudos.users, user).public_key])),
        Base.encode64(Jam.jam(:crypto.strong_rand_bytes(32))),
        proved_logic,
        Base.encode64(Jam.jam(amount)),
        Base.encode64(Jam.jam(kind))
      ]
    }

    # make the request to the prove endpoint, and expect a transaction back.
    data =
      kudos.client.conn
      |> post(~p"/nock/prove", payload)
      |> json_response(200)

    # verify that there is no error.
    assert Kernel.match?(%{"io" => [], "result" => binary}, data)

    # extract the prove result
    proved_create = data["result"]

    assert proved_create != "error"

    # ----------------------------------------------------------------------------
    # Submit the create transaction to the mempool
    #
    # The result of the above prove call is a balanced transaction that can be
    # submitted to the mempool directly. When this is done, a new block will be
    # created, and the user bob will have a resource to its name with the given
    # quantity.

    :ok = Helpers.submit_transaction_and_confirm(kudos, proved_create)

    # assert that there are now resources in the indexer
    assert not Enum.empty?(Indexer.get(kudos.client.node.node_id, :resources))

    {kudos, proved_create, amount, kind}
  end

  @doc """
  I assert that a user who initialized kudos, has exactly one resource.
  """
  @spec user_has_resources_after_initialize(Kudos.t(), atom()) ::
          {Kudos.t(), [Resource.t()]}
  def user_has_resources_after_initialize(kudos \\ setup(), user \\ :bob) do
    {kudos, _, _quantity, _kind} = initialize_kudos(kudos, user)

    resources = Helpers.resources_for(kudos, user)

    assert Enum.count(resources) == 1

    {kudos, resources}
  end
end
