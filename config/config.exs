import Config

# ----------------------------------------------------------------------------
# Logger

config :logger,
  level: :error,
  handle_otp_reports: true,
  handle_sasl_reports: true

# remove logging calls from the code in production
# https://hexdocs.pm/logger/1.17.3/Logger.html#module-compile-configuration
config :logger,
  compile_time_purge_matching: [
    [level_lower_than: :error]
  ]

config :anoma_client, []
config :anoma_lib, []
config :anoma_node, []
config :anoma_protobuf, []
config :compile_protoc, []
config :event_broker, []

# ----------------------------------------------------------------------------
# Import environment specific config.

if config_env() in [:dev, :test] do
  import_config "dev.exs"
end
