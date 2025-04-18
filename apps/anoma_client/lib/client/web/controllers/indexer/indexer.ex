defmodule Anoma.Client.Web.IndexerController do
  use Anoma.Client.Web, :controller
  use OpenApiSpex.ControllerSpecs

  action_fallback(Anoma.Client.Web.FallbackController)

  alias Anoma.Client.Node.GRPCProxy
  alias Anoma.Client.Web.IndexerController.Spec
  alias OpenApiSpex.Schema
  alias OpenApiSpex.Operation

  ############################################################
  #                           OpenAPI Spec                   #
  ############################################################

  tags(["Indexer"])

  operation(:list_nullifiers,
    summary: "List all nullifiers",
    parameters: [],
    request_body: {},
    responses: [
      ok: {"List of all nullifiers", "application/json", Spec.Nullifiers}
    ]
  )

  operation(:list_unrevealed_commits,
    summary: "List all unrevealed commits",
    parameters: [],
    request_body: {},
    responses: [
      ok:
        {"List of all unrevealed commits", "application/json",
         Spec.UnrevealedCommits}
    ]
  )

  operation(:list_commits,
    summary: "List all commits",
    parameters: [],
    request_body: {},
    responses: [
      ok: {"List of all unrevealed commits", "application/json", Spec.Commits}
    ]
  )

  operation(:list_unspent_resources,
    summary: "List all unspent resources",
    parameters: [],
    request_body: {},
    responses: [
      ok:
        {"List of unspent resources", "application/json",
         Spec.UnspentResources}
    ]
  )

  operation(:get_blocks,
    summary: "List all blocks",
    parameters: [
      Operation.parameter(
        :direction,
        :path,
        %Schema{
          type: :string,
          description: "before or after offset",
          default: "before",
          enum: ["before", "after"]
        },
        "Direction",
        example: "before"
      ),
      Operation.parameter(
        :offset,
        :path,
        %Schema{
          type: :integer,
          description: "offset of blocks",
          default: 0,
          minimum: 0
        },
        "Block offset",
        example: 1
      )
    ],
    request_body: {},
    responses: [
      ok: {"List of blocks", "application/json", Spec.Blocks}
    ]
  )

  operation(:get_latest_block,
    summary: "List latest block",
    parameters: [],
    request_body: {},
    responses: [
      ok:
        {"A block", "application/json",
         %Schema{
           type: :object,
           properties: %{
             block: Spec.Block
           }
         }}
    ]
  )

  operation(:root,
    summary: "Get root",
    parameters: [],
    request_body: {},
    responses: [
      ok:
        {"The root hash of the chain", "application/json",
         %Schema{
           type: :object,
           properties: %{
             root: %Schema{
               type: :string
             }
           }
         }}
    ]
  )

  operation(:filter_resource,
    summary: "Filter resources",
    parameters: [],
    request_body:
      {"A list of filter objects", "application/json", Spec.Filters},
    responses: []
  )

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
  def filter_resource(conn, %{"filters" => filters}) do
    with filters <- parse_filters(filters),
         {:ok, resources} <- GRPCProxy.filter(filters) do
      render(conn, "binaries.json", resources: resources)
    end
  end

  # @doc """
  # Example of filters query:
  # %{"filters" => [%{"owner" => "jeremy"}]}
  # """
  defp parse_filters(""), do: []

  defp parse_filters(filters) when is_list(filters) do
    Enum.map(filters, fn filter ->
      case filter do
        %{"owner" => binary} ->
          {:owner, Base.decode64!(binary)}

        %{"kind" => kind} ->
          {:kind, Base.decode64!(kind)}
      end
    end)
  end
end
