defmodule Anoma.Client.Web.IndexerController do
  use Anoma.Client.Web, :controller
  # use OpenApiSpex.ControllerSpecs

  action_fallback(Anoma.Client.Web.FallbackController)

  alias Anoma.Client.Node.GRPCProxy

  ############################################################
  #                           OpenAPI Spec                   #
  ############################################################

  ############################################################
  #                          Actions                         #
  ############################################################

  @doc """
  I return the list of nullifiers.
  """
  def list_nullifiers(conn, _params) do
    with {:ok, nullifiers} <- GRPCProxy.list_nullifiers() do
      render(conn, "binaries.json", nullifiers: nullifiers)
    end
  end

  @doc """
  I return the list of unrevealed commits.
  """
  def list_unrevealed_commits(conn, _params) do
    with {:ok, commits} <- GRPCProxy.list_unrevealed_commits() do
      render(conn, "binaries.json", commits: commits)
    end
  end

  @doc """
  I return the list of commits.
  """
  def list_commits(conn, _params) do
    with {:ok, commits} <- GRPCProxy.list_commits() do
      render(conn, "commits.json", commits: commits)
    end
  end

  @doc """
  I return the list of unspent resources.
  """
  def list_unspent_resources(conn, _params) do
    with {:ok, unspent_resources} <- GRPCProxy.list_unspent_resources() do
      render(conn, "binaries.json", unspent_resources: unspent_resources)
    end
  end

  @doc """
  I return the specific block
  """
  def get_blocks(conn, %{"direction" => direction, "offset" => offset}) do
    with direction <- String.to_existing_atom(direction),
         offset <- String.to_integer(offset),
         {:ok, blocks} <- GRPCProxy.get_blocks({direction, offset}) do
      render(conn, "blocks.json", blocks: blocks)
    end
  end

  @doc """
  """
  def get_latest_block(conn, _params) do
    with {:ok, block} <- GRPCProxy.get_latest_block() do
      render(conn, "block.json", block: block)
    end
  end

  @doc """
  """
  def root(conn, _params) do
    with {:ok, root} <- GRPCProxy.root() do
      render(conn, "root.json", root: root)
    end
  end

  @doc """
  """
  def filter_resource(conn,  %{"filters" => filters}) do
    with filters <- parse_filters(filters),
         {:ok, resources} <- GRPCProxy.filter(filters) do
      render(conn, "binaries.json", resources: resources)
    end
  end

  defp parse_filters(""), do: []

  defp parse_filters(filters) do
    Enum.map(filters, fn filter ->
      case filter do
        {"owner", binary} ->
          {:owner, binary}

        {"kind", kind} ->
          {:kind, kind}
      end
    end)
  end
end
