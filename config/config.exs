import Config

config :logger,
  level: :error,
  handle_otp_reports: true,
  handle_sasl_reports: true

config :anoma_client, []
config :anoma_lib, []

config :anoma_node,
  grpc_port: String.to_integer(System.get_env("GRPC_PORT") || "50051")

config :anoma_protobuf, []
config :compile_protoc, []
config :event_broker, []
