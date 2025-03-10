import Config

config :anoma_dashboard,
  ecto_repos: [AnomaDashboard.Repo],
  generators: [context_app: false]

# Configures the endpoint
config :anoma_dashboard, AnomaDashboard.Endpoint,
  url: [host: "localhost"],
  adapter: Bandit.PhoenixAdapter,
  render_errors: [
    formats: [html: AnomaDashboard.ErrorHTML, json: AnomaDashboard.ErrorJSON],
    layout: false
  ],
  pubsub_server: AnomaDashboard.PubSub,
  live_view: [signing_salt: "S4S3nv/C"]

# Configure esbuild (the version is required)
config :esbuild,
  version: "0.17.11",
  anoma_dashboard: [
    args:
      ~w(js/app.js --bundle --target=es2017 --outdir=../priv/static/assets --external:/fonts/* --external:/images/*),
    cd: Path.expand("../apps/anoma_dashboard/assets", __DIR__),
    env: %{"NODE_PATH" => Path.expand("../deps", __DIR__)}
  ]

# Configure tailwind (the version is required)
config :tailwind,
  version: "3.4.3",
  anoma_dashboard: [
    args: ~w(
      --config=tailwind.config.js
      --input=css/app.css
      --output=../priv/static/assets/app.css
    ),
    cd: Path.expand("../apps/anoma_dashboard/assets", __DIR__)
  ]

# ----------------------------------------------------------------------------
# Endpoint

# Configures the endpoint
config :anoma_client, Anoma.Client.Web.Endpoint,
  server: true,
  adapter: Bandit.PhoenixAdapter,
  http: [
    ip: {127, 0, 0, 1},
    port: String.to_integer(System.get_env("HTTP_PORT") || "5000")
  ],
  check_origin: false,
  debug_errors: false,
  render_errors: [view: Anoma.Client.Web.ErrorJSON, accepts: ~w(json)],
  code_reloader: false

config :anoma_client, Anoma.Client.Web.SocketHandler,
  port: 6000,
  path: "/ws"

# codec: Riverside.Codec.RawBinary,
# max_connection_age: :infinity,
# show_debug_logs: true,
# idle_timeout: 120_000,
# reuse_port: false

config :logger,
  level: :error,
  handle_otp_reports: false,
  handle_sasl_reports: false

config :anoma_client,
  grpc_port: String.to_integer(System.get_env("CLIENT_GRPC_PORT") || "50052")

config :anoma_lib, []

config :anoma_node,
  grpc_port: String.to_integer(System.get_env("GRPC_PORT") || "50051")

config :anoma_protobuf, []
config :compile_protoc, []
config :event_broker, []
import Config

# Configures Elixir's Logger
config :logger, :console,
  format: "$time $metadata[$level] $message\n",
  metadata: [:request_id]

# Use Jason for JSON parsing in Phoenix
config :phoenix, :json_library, Jason

# Import environment specific config. This must remain at the bottom
# of this file so it overrides the configuration defined above.
import_config "#{config_env()}.exs"
