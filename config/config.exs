import Config

config :logger,
  level: :error,
  handle_otp_reports: false,
  handle_sasl_reports: false

# ----------------------------------------------------------------------------
# Endpoint

# Configures the endpoint
config :anoma_client, Anoma.Client.Web.Endpoint,
  server: true,
  adapter: Bandit.PhoenixAdapter,
  http: [
    ip: {127, 0, 0, 1},
    port: String.to_integer(System.get_env("CLIENT_HTTP_PORT") || "4000")
  ],
  check_origin: false,
  debug_errors: false,
  render_errors: [view: Anoma.Client.Web.ErrorJSON, accepts: ~w(json)]

config :anoma_client,
  grpc_port: String.to_integer(System.get_env("CLIENT_GRPC_PORT") || "40051")

config :anoma_lib, []

config :anoma_node,
  grpc_port: String.to_integer(System.get_env("NODE_GRPC_PORT") || "50051")

config :anoma_protobuf, []
config :compile_protoc, []
config :event_broker, []

############################################################
#                       Mnesia                             #
############################################################

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
if File.exists?("config/#{config_env()}.exs") do
  import_config "#{config_env()}.exs"
end
