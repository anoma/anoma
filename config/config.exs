import Config

config :logger,
  level: :warning,
  handle_otp_reports: true,
  handle_sasl_reports: true

config :anoma,
  env: config_env()

import_config "#{config_env()}.exs"
