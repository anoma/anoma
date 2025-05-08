defmodule Anoma.LocalDomain.Web.Router do
  use Anoma.LocalDomain.Web, :router

  pipeline :api do
    plug(:accepts, ["json"])
    plug(OpenApiSpex.Plug.PutApiSpec, module: Anoma.LocalDomain.Web.ApiSpec)
  end

  scope "/spec" do
    pipe_through(:api)
    get("/openapi", OpenApiSpex.Plug.RenderSpec, [])
    get("/swaggerui", OpenApiSpex.Plug.SwaggerUI, path: "/spec/openapi")
  end

  scope "/transactions", Anoma.LocalDomain.Web do
    pipe_through(:api)
    post("/compose", TransactionController, :compose)
    post("/verify", TransactionController, :verify)
  end

  scope "/intents", Anoma.LocalDomain.Web do
    pipe_through(:api)
    get("/", IntentsController, :index)
    post("/", IntentsController, :add_intent)
  end

  scope "/executor", Anoma.LocalDomain.Web do
    pipe_through(:api)
    post("/", ExecutorController, :add_read_only_transaction)
  end

  scope "/nock", Anoma.LocalDomain.Web do
    pipe_through(:api)
    post("/run", NockController, :run)
    post("/prove", NockController, :prove)
  end

  scope "/mempool", Anoma.LocalDomain.Web do
    pipe_through(:api)

    post("/add", MempoolController, :add_transaction)
  end

  scope "/subscribe", Anoma.LocalDomain.Web do
    pipe_through(:api)

    post("/", SubscribeController, :subscribe)
  end
end
