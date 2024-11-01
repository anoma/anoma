import Config

level =
  if Mix.env() == :test do
    :none
  else
    :error
  end

config :logger,
  level: level,
  handle_otp_reports: true,
  handle_sasl_reports: true

config :anoma_client, []
config :anoma_lib, []
config :anoma_node, []
config :anoma_protobuf, []
config :compile_protoc, []
config :event_broker, []
