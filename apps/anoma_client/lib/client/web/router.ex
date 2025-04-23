defmodule Anoma.Client.Web.Router do
  use Anoma.Client.Web, :router

  pipeline :api do
    plug(:accepts, ["json"])
    plug(OpenApiSpex.Plug.PutApiSpec, module: Anoma.Client.Web.ApiSpec)
  end

  scope "/spec" do
    pipe_through(:api)
    get("/openapi", OpenApiSpex.Plug.RenderSpec, [])
    get("/swaggerui", OpenApiSpex.Plug.SwaggerUI, path: "/spec/openapi")
  end

  scope "/transactions", Anoma.Client.Web do
    pipe_through(:api)
    post("/compose", TransactionController, :compose)
    post("/verify", TransactionController, :verify)
  end

  scope "/intents", Anoma.Client.Web do
    pipe_through(:api)
    get("/", IntentsController, :index)
    post("/", IntentsController, :add_intent)
  end

  scope "/executor", Anoma.Client.Web do
    pipe_through(:api)
    post("/", ExecutorController, :add_read_only_transaction)
  end

  scope "/nock", Anoma.Client.Web do
    pipe_through(:api)
    post("/run", NockController, :run)
    post("/prove", NockController, :prove)
  end

  scope "/mempool", Anoma.Client.Web do
    pipe_through(:api)

    post("/add", MempoolController, :add_transaction)
  end

  scope "/subscribe", Anoma.Client.Web do
    pipe_through(:api)

    post("/", SubscribeController, :subscribe)
  end

  scope "/indexer", Anoma.Client.Web do
    pipe_through(:api)

    get("/nullifiers", IndexerController, :list_nullifiers)
    get("/unrevealed-commits", IndexerController, :list_unrevealed_commits)
    get("/commits", IndexerController, :list_commits)
    get("/unspent-resources", IndexerController, :list_unspent_resources)
    get("/blocks/:direction/:offset", IndexerController, :get_blocks)
    get("/latest-block", IndexerController, :get_latest_block)
    get("/root", IndexerController, :root)
    post("/filter-resources", IndexerController, :filter_resource)
  end
end
