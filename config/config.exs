import Config

# ----------------------------------------------------------------------------
# Environment variables

# port on which the client will listen for http requests
client_http_port =
  String.to_integer(System.get_env("CLIENT_HTTP_PORT") || "4000")

# port on which the client will listen for websocket connections
client_websocket_port =
  String.to_integer(System.get_env("CLIENT_WEBSOCKET_PORT") || "3000")

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
# Client

# Configures the endpoint
config :anoma_client, Anoma.Client.Web.Endpoint,
  server: true,
  adapter: Bandit.PhoenixAdapter,
  http: [
    ip: {127, 0, 0, 1},
    port: client_http_port
  ],
  check_origin: false,
  debug_errors: false,
  render_errors: [view: Anoma.Client.Web.ErrorJSON, accepts: ~w(json)],
  code_reloader: false

config :anoma_client, Anoma.Client.Web.SocketHandler,
  port: client_websocket_port,
  path: "/ws"

config :anoma_client,
  grpc_port: client_grpc_port,
  grpc_host: client_grpc_host

# ----------------------------------------------------------------------------
# Logger

config :logger,
  level: :error,
  handle_otp_reports: false,
  handle_sasl_reports: false

# ----------------------------------------------------------------------------
# Node
config :anoma_node,
  grpc_port: node_grpc_port,
  grpc_host: node_grpc_host

config :anoma_lib, []

config :anoma_protobuf, []
config :compile_protoc, []
config :event_broker, []
