defmodule Anoma.LocalDomain.System.GraphQLPoller do
  @moduledoc """
  I define the GraphQLPoller application for the local domain.

  GraphQLPoller is an application that polls for events from a graphQL endpoint for a protocol adapter contract.
  """

  use Anoma.LocalDomain.Application, name: "graphQLPoller"
  use Anoma.LocalDomain.DefScry

  def write_cipherkey(name, key) do
    Anoma.LocalDomain.Storage.write_local(
      ~k"/poller/cipherkey/!name", key
    )
  end

  def set_endpoint(url) do
    Anoma.LocalDomain.Storage.write_local(
      ~k"/poller/graphql_endpoint", url
    )
  end

  def read_blockheight() do
    Anoma.LocalDomain.Storage.read_latest(
      ~k"/poller/blockheight"
    )
  end

  def write_blockheight(height) do
    Anoma.LocalDomain.Storage.write_local(
      ~k"/poller/blockheight", height
    )
  end

  def write_cipher() do
  end

  @impl true
  def init() do
    super()

    case read_blockheight() do
      {:ok, _} -> :ok

      :absent ->
        write_blockheight(0)
    end

    Supervisor.start_link([{__MODULE__.Poller, {}}], strategy: :one_for_one, name: __MODULE__.Supervisor)
  end

  # defscry do
  #   _prev_prefixes, ~k"/" ->
  #     with {:ok, s} <-
  #     Anoma.LocalDomain.Scry.scry
  # end
end
