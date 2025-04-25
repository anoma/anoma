import Config

# ----------------------------------------------------------------------------
# Logger

config :logger,
  level: :error,
  handle_otp_reports: false,
  handle_sasl_reports: false

# ----------------------------------------------------------------------------
# Anoma Client Web Endpoint

# Configures the endpoint
config :anoma_client, Anoma.Client.Web.Endpoint,
  server: true,
  adapter: Bandit.PhoenixAdapter,
  http: [
    ip: {127, 0, 0, 1},
    port: 4000
  ],
  check_origin: false,
  debug_errors: false,
  render_errors: [view: Anoma.Client.Web.ErrorJSON, accepts: ~w(json)]

# ----------------------------------------------------------------------------
# Anoma Client

config :anoma_client,
  grpc_port: 40051

# ----------------------------------------------------------------------------
# Anoma Node

config :anoma_node,
  grpc_port: 50051

# ----------------------------------------------------------------------------
# Mnesia

# persist_to_disk: should the mnesia data be written to disk?
#                  default: false
#
# data_dir:        the directory where the data will be written,
#                  if persisted to disk
#                  default: platform dependent
#                           linux: `$XDG_DATA_HOME/anoma` or `~/.config/anoma `
#                           macos: `~/Library/Application Support/Anoma`
#
# rocksdb:         should the rockdb backend be used?
#                  default: true
config :anoma_node, :mnesia,
  persist_to_disk: false,
  rocksdb: false

# Import environment specific config. This must remain at the bottom
# of this file so it overrides the configuration defined above.
import_config "#{config_env()}.exs"
