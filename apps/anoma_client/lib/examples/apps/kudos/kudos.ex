defmodule Anoma.Client.Examples.Apps.Kudos do
  @moduledoc """
  I contain functions to run the Kudos code against the client.

  Each step of the kudos app is split out into its own separate file.

    This requires the Juvix files to be compiled, located at
  `apps/anoma_client/priv/juvix/src/Kudos`.
  """
  use Anoma.Client.Web.ConnCase

  alias Anoma.Client.Examples.Apps.Kudos
  alias Anoma.Client.Examples.EClient
  alias Anoma.Crypto.Sign

  use TypedStruct

  @doc """
  The path to the compiled Juvix files.
  """
  @kudos_path :code.priv_dir(:anoma_client) |> Path.join("/juvix/src/Kudos")

  ############################################################
  #                           Structs                        #
  ############################################################

  typedstruct module: User do
    @typedoc """
    I contain all data that is necessary for a user to interact with Kudos.
    """

    field(:username, String.t(),
      default: Base.encode16(:crypto.strong_rand_bytes(4))
    )

    field(:public_key, <<_::32, _::_*8>>)
    field(:private_key, <<_::32, _::_*8>>)
    field(:sign_key, <<_::64, _::_*8>>)
  end

  typedstruct do
    @typedoc """
    I contain state for running the Kudos examples.
    In particular, I contain a client connection and a keypair.
    """
    field(:client, EClient.t())

    field(:users, %{atom() => User.t()}, default: %{})
  end

  ############################################################
  #                           API                            #
  ############################################################

  @doc """
  I create a Kudos context that holds a keypair and a list of clients.
  """
  @spec setup :: Kudos.t()
  def setup(client \\ EClient.setup()) do
    Anoma.Node.Utility.Consensus.start_link(
      node_id: client.node.node_id,
      interval: 500
    )

    # create a few users to use during examples.
    users =
      for user <- [:bob, :alice, :caracalla], into: %{} do
        username = Atom.to_string(user)

        # create a keypair and destructure it into sign key, public key, and secret key.
        %{public: _public, secret: secret} = Sign.new_keypair()
        <<private::binary-size(32), public::binary-size(32)>> = secret

        {user,
         %User{
           username: username,
           public_key: public,
           private_key: private,
           sign_key: secret
         }}
      end

    %Kudos{
      client: client,
      users: users
    }
  end

  ############################################################
  #                           Helpers                        #
  ############################################################

  @type file ::
          :create
          | :logic
          | :get_balance
          | :transfer
          | :merge
          | :swap
          | :get_give_resource

  @spec nockma_file(file) :: binary()
  def nockma_file(filename) do
    filename =
      "#{filename}"
      |> String.split("_")
      |> Enum.map(&String.capitalize/1)
      |> Enum.join("")

    @kudos_path
    |> Path.join("/.compiled/#{filename}.nockma")
    |> File.read!()
  end
end
