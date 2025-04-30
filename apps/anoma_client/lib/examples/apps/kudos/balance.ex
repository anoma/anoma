defmodule Anoma.Client.Examples.Apps.Kudos.Balance do
  @moduledoc """
  I contain logic to test the balance of a given resource and user.
  """
  use Anoma.Client.Web.ConnCase
  use EventBroker.WithSubscription

  alias Anoma.Client.Examples.Apps.Kudos
  alias Anoma.Client.Examples.Apps.Kudos.Helpers
  alias Noun.Jam
  alias Noun.Nounable
  alias Anoma.RM.Transparent.Resource

  require Anoma.Node.Examples.EEvent
  require Logger

  import Anoma.Client.Examples.Apps.Kudos

  @doc """
  Given a user, I check its balance, and return a list of all its resources and
  how many they have.
  """
  @spec check_balance(Kudos.t(), atom()) ::
          {Kudos.t(), [{String.t(), number()}]}
  def check_balance(kudos, user \\ :bob) do
    resources = Helpers.resources_for(kudos, user)
    {_kudos, balances} = prove_get_balance(kudos, resources)

    {kudos, balances}
  end

  @doc """
  I prove the GetBalance.nockma file and return its result. The result is the
  base64 encoded, jammed representation of the resources in the following
  format: "resource_name: quantity"
  """
  @spec prove_get_balance(Kudos.t(), [Resource.t()]) ::
          {Kudos.t(), [{String.t(), number()}]}
  def prove_get_balance(kudos \\ setup(), resources) do
    get_balance_nockma = Kudos.nockma_file(:get_balance)

    resources_jammed =
      Enum.map(resources, fn r -> Nounable.to_noun(r) |> Jam.jam() end)
      |> Jam.jam()

    payload = %{
      "program" => Base.encode64(get_balance_nockma),
      "private_inputs" => [Base.encode64(resources_jammed)]
    }

    data =
      kudos.client.conn
      |> post(~p"/nock/prove", payload)
      |> json_response(200)

    assert Kernel.match?(%{"io" => [], "result" => binary}, data)

    # extract the prove result
    proved_get_balance = data["result"]

    # parse the result
    balances =
      proved_get_balance
      |> Base.decode64!()
      |> Noun.Jam.cue!()
      |> String.trim()
      |> String.split("\n")
      |> Enum.map(fn s -> String.split(s, " : ") end)
      |> Enum.map(fn [kind, quantity] ->
        {kind, String.to_integer(quantity)}
      end)

    {kudos, balances}
  end
end
