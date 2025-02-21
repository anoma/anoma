defmodule Anoma.Client.Web.Router do
  use Anoma.Client.Web, :router

  pipeline :api do
    plug(:accepts, ["json"])
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
end
