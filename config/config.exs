import Config

# ----------------------------------------------------------------------------
# Environment variables
# port on which the node will listen for grpc requests
node_grpc_port =
  String.to_integer(System.get_env("NODE_GRPC_PORT") || "50052")

# interface at which the node grpc server will listen
node_grpc_host = System.get_env("NODE_GRPC_HOST") || "localhost"

# grpc port on which the client will listen for grpc requests
client_grpc_port =
  String.to_integer(System.get_env("CLIENT_GRPC_PORT") || "50051")

# interface at which this grpc server will listen
client_grpc_host = System.get_env("CLIENT_GRPC_HOST") || "localhost"

# ----------------------------------------------------------------------------
# Logger

config :logger,
  level: :error,
  handle_otp_reports: false,
  handle_sasl_reports: false

config :anoma_lib, []

config :anoma_client,
  grpc_port: node_grpc_port,
  grpc_host: node_grpc_host

config :anoma_node,
  grpc_port: node_grpc_port,
  grpc_host: node_grpc_host

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
