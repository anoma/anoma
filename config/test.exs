import Config

# ----------------------------------------------------------------------------
# Logger

config :logger,
  level: :error

# ----------------------------------------------------------------------------
# Anoma Client Web Endpoint

# Configures the endpoint
config :anoma_client, Anoma.Client.Web.Endpoint,
  server: true,
  adapter: Bandit.PhoenixAdapter,
  http: [
    ip: {127, 0, 0, 1},
    port: 5000
  ],
  check_origin: false,
  debug_errors: false,
  render_errors: [view: Anoma.Client.Web.ErrorJSON, accepts: ~w(json)]

# ----------------------------------------------------------------------------
# Anoma Client

config :anoma_client,
  grpc_port: 41051

# ----------------------------------------------------------------------------
# Anoma Node

config :anoma_node,
  grpc_port: 51051

# ----------------------------------------------------------------------------
# Mnesia

# rocksdb is disabled for testing because it slows tests down too much
config :anoma_node, :mnesia,
  persist_to_disk: false,
  rocksdb: false
