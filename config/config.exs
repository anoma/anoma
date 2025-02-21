import Config

# ----------------------------------------------------------------------------
# Endpoint

# Configures the endpoint
config :anoma_client, Anoma.Client.Web.Endpoint,
  server: true,
  adapter: Bandit.PhoenixAdapter,
  http: [
    ip: {127, 0, 0, 1},
    port: String.to_integer(System.get_env("HTTP_PORT") || "4000")
  ],
  check_origin: false,
  debug_errors: false,
  render_errors: [view: Anoma.Client.Web.ErrorJSON, accepts: ~w(json)],
  code_reloader: false

config :anoma_client, Anoma.Client.Web.SocketHandler,
  port: 3000,
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
